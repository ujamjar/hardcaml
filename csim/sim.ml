open HardCaml

type port = 
  {
    name : string;
    width : int;
    get : unit -> Int32.t array;
    set : Int32.t array -> unit;
  }

module C = struct

  open Ctypes
  open PosixTypes
  open Foreign

  type port_c
  let port_t : port_c structure typ = structure "port"
  let name = field port_t "name" string
  let width = field port_t "width" int
  let bits = field port_t "bits" (ptr int32_t)
  let () = seal port_t

  type sim_c
  let sim_t : sim_c structure typ = structure "simualator"
  let data = field sim_t "data" (ptr uint32_t)
  let regs = field sim_t "regs" (ptr uint32_t)
  let mems = field sim_t "mems" (ptr uint32_t)
  let muxes = field sim_t "muxes" (ptr uint32_t)
  let in_ports = field sim_t "in_ports" (ptr port_t) 
  let num_in_ports = field sim_t "num_in_ports" int
  let out_ports = field sim_t "out_ports" (ptr port_t) 
  let num_out_ports = field sim_t "num_out_ports" int
  let () = seal sim_t

  let init from = Foreign.foreign ~from "init" (void @-> returning (ptr sim_t))
  let reset from = Foreign.foreign ~from "reset" (ptr sim_t @-> returning void)
  let cycle from = Foreign.foreign ~from "cycle" (ptr sim_t @-> returning void)

  let get_ports sim ports num_ports = 
    let ports = getf (!@ sim) ports in
    let num = getf (!@ sim) num_ports in
    let carr = CArray.from_ptr ports num in
    Array.init num (fun i ->
        let p = CArray.get carr i in
        let w = getf p width in
        let wds = (w + 31) / 32 in
        let d = CArray.from_ptr (getf p bits) wds in
        let arr = Array.init wds (fun i -> 0l) in
        {
          name = getf p name;
          width = getf p width;
          get = (fun () -> 
                  for i=0 to wds-1 do
                    arr.(i) <- CArray.get d i
                  done;
                  arr);
          set = (fun arr -> 
                  for i=0 to wds-1 do
                    CArray.set d i arr.(i) 
                  done);
        })

end

let compile_shared_lib name = 
  match Unix.system ("gcc -shared -fPIC "^name^".c -o lib"^name^".so") with
  | Unix.WEXITED 0 -> ()
  | _ -> failwith ("failed to compile "^name)

let make_from_shared_lib dll = 
  let module B = Bits.Comb.ArraybitsInt32 in
  let from = Dl.dlopen ~filename:dll ~flags:[Dl.RTLD_NOW] in
  let sim = C.init from () in
  let reset = C.reset from in
  let cycle = C.cycle from in
  let in_ports = C.get_ports sim C.in_ports C.num_in_ports in
  let out_ports = C.get_ports sim C.out_ports C.num_out_ports in
  let none () = () in
  let sim_in_ports = Array.map (fun p -> (p.name, ref (B.zero p.width)), p) in_ports in
  let sim_out_ports = Array.map (fun p -> (p.name, ref (B.zero p.width)), p) out_ports in
  let to_ports x = List.map fst @@ Array.to_list x in
  let sim_cycle_comb0 () =
    Array.iter 
      (fun ((n,d),p) ->
        let (d,w) = !d in
        assert (w = p.width);
        p.set d) 
      sim_in_ports;
    cycle sim;
    Array.iter
      (fun ((n,d),p) ->
        let (_,w) = !d in
        assert (w = p.width);
        d := (p.get (), w))
      sim_out_ports;
  in
  {
    Cyclesim.Api.sim_in_ports = to_ports sim_in_ports;
    sim_out_ports = to_ports sim_out_ports;
    sim_out_ports_next = [];
    sim_internal_ports = [];
    sim_reset = (fun () -> reset sim);
    sim_cycle_check = none;
    sim_cycle_comb0;
    sim_cycle_seq = none;
    sim_cycle_comb1 = none;
    sim_lookup_signal = (fun _ -> failwith "csim: sim_lookup_singal");
    sim_lookup_reg = (fun _ -> failwith "csim: sim_lookup_reg");
    sim_lookup_memory = (fun _ -> failwith "csim: sim_lookup_mem");
  }

let make ?(name="tmp") circ = 
  let cname = name ^ ".c" in
  let lname = "lib" ^ name ^ ".so" in
  let f = open_out cname in
  let () = C.write (output_string cname) circ in
  let () = close_out f in
  let () = compile_shared_lib cname in
  make_from_shared_lib ("./" ^ name ^ ".so"


