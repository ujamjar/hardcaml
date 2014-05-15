(* 
 * hardcaml - hardware design in OCaml
 *
 *   (c) 2014 MicroJamJar Ltd
 *
 * Author(s): andy.ray@ujamjar.com
 * Description: 
 *
 *)

(* compilation and dynamic loading of C simulators 
 * for applications to dynamically load the simulator, 
 * use "-linkall -dllpath ."
 * The dllpath thing is important - we need the dynamic linker to find the
 * .so file (which is generated in the same directory as the
 * exe is run at the moment)
 * *)
let mli dut = "
type _port = {
  name: string;
  width: int;
  bits: (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t;
}
and port = _port
and _simulator = {
  data: int Com.opaque;
  regs: int Com.opaque;
  mems: int Com.opaque;
  muxs: int Com.opaque;
  in_ports: port array;
  out_ports: port array;
}
and simulator = _simulator

external init : unit -> simulator option
	= \"camlidl_" ^ dut ^ "_init\"

external cycle : simulator option -> unit
	= \"camlidl_" ^ dut ^ "_cycle\"

external reset : simulator option -> unit
	= \"camlidl_" ^ dut ^ "_reset\"

val make : unit -> 
    Bits_ext.Comb.BigarraybitsInt32.t 
        HardCaml.Cyclesim.Api.cyclesim
"
let ml dut = "
type _port = {
  name: string;
  width: int;
  bits: (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t;
}
and port = _port
and _simulator = {
  data: int Com.opaque;
  regs: int Com.opaque;
  mems: int Com.opaque;
  muxs: int Com.opaque;
  in_ports: port array;
  out_ports: port array;
}
and simulator = _simulator

external init : unit -> simulator option
	= \"camlidl_" ^ dut ^ "_init\"

external cycle : simulator option -> unit
	= \"camlidl_" ^ dut ^ "_cycle\"

external reset : simulator option -> unit
	= \"camlidl_" ^ dut ^ "_reset\"

open HardCaml

module B = Bits_ext.Comb.BigarraybitsInt32

let make () = 
    let sim' = init() in
    let sim = match sim' with Some(x) -> x 
                            | _ -> failwith \"simulation creation failed\"
    in

    let in_ports = Array.map 
        (fun d -> d.name, ref (B.zero d.width))
        sim.in_ports
    in
    let out_ports = Array.map 
        (fun d -> d.name, ref (d.bits, d.width))
        sim.out_ports
    in

    let inputs() = 
        for i=0 to Array.length sim.in_ports - 1 do
            let d = sim.in_ports.(i).bits in
            let d',_ = !(snd in_ports.(i)) in
            for j=0 to Bigarray.Array1.dim d - 1 do
                d.{j} <- d'.{j}
            done
        done
    in
    let cycle() = inputs(); cycle sim' in
    let reset() = inputs(); reset sim' in

    {
        Cyclesim.Api.sim_reset = reset;
        Cyclesim.Api.sim_cycle = cycle;
        Cyclesim.Api.sim_in_ports = Array.to_list in_ports;
        Cyclesim.Api.sim_out_ports = Array.to_list out_ports;
        Cyclesim.Api.sim_internal_ports = [];
    }

let _ = 
    C_ext.csim := Some(fun () -> make())
"

let csim = ref None

let run prog args = 
    let cmd = prog ^ " " ^ args in
    (*Printf.printf "%s\n" cmd;*)
    match Unix.system cmd with
    | Unix.WEXITED(x) -> begin
            if x<>0 then failwith (prog ^ " failed")
    end
    | _ -> failwith (prog ^ " failed")

let gcc = run "gcc"
let ocamlc = 
        if Dynlink.is_native then run "ocamlfind ocamlopt" 
        else run "ocamlfind ocamlc"
let ocamlmklib = run "ocamlmklib"

let ocamlmklib o cmo idl name = 
    if Dynlink.is_native then
        ocamlc (o ^ " " ^ cmo ^ " " ^ idl ^ " -linkall -shared -o " ^ name ^ ".cmxs")
    else
        ocamlmklib (o ^ " " ^ cmo ^ " " ^ idl ^ " -o " ^ name)

(* XXX - need a better way to get this path...not nice *)
let camlidl = "/usr/lib/ocaml/libcamlidl.a"

let compile name = 
    let n x = name ^ "." ^ x in
    let c,mli,ml = n "c", n "mli", n "ml" in
    let o,cmi,cmo = n "o", n "cmi", 
        n (if Dynlink.is_native then "cmx" else "cmo")
    in
    gcc ("-fPIC -O1 -c -DOCAML " ^ c);
    ocamlc ("-package hardcaml.x -c " ^ mli);
    ocamlc ("-package hardcaml.x -c " ^ ml);
    ocamlmklib o cmo camlidl name

let write_c name circ = 
    let f = open_out (name^".c") in
    C.write ~name (output_string f) circ;
    close_out f

let write_ml name = 
    let f = open_out (name^".ml") in
    output_string f (ml name);
    close_out f

let write_mli name = 
    let f = open_out (name^".mli") in
    output_string f (mli name);
    close_out f

let load name = 
    let cma = if Dynlink.is_native then ".cmxs" else ".cma" in
    csim := None;
    begin
    try
        Dynlink.allow_unsafe_modules true;
        Dynlink.loadfile_private (name^cma);
    with Dynlink.Error(x) ->
        failwith (Dynlink.error_message x)
    end;
    match !csim with
    | Some(x) -> x()
    | None -> failwith "couldn't load simulation"

let make name circ = 
    write_ml name;
    write_mli name;
    write_c name circ;
    compile name;
    load name

let get_csim() = 
    match !csim with
    | None -> failwith "no simulator installed"
    | Some(x) -> x()

    
