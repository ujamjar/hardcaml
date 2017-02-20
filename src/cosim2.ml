(* 
 * hardcaml - hardware design in OCaml
 *
 *   (c) 2014 MicroJamJar Ltd
 *
 * Author(s): andy.ray@ujamjar.com
 * Description: 
 *
 *)

open Astring

type delta_message = 
  {
    sets : (int * int32 list) list;
    gets : int list;
    delta_time : int64;
  }

type init_message = string list 

type control_message = 
  | Finish
  | Run of delta_message

type response_message = (int * int32 list) list

let net_addr = "localhost"
let net_port = 10101

module Comms = struct

  open Unix

  let empty = ""

  let create_client server port = 
    let sock = socket PF_INET SOCK_STREAM 0 in
    let server_addr = ADDR_INET( (gethostbyname server).h_addr_list.(0), port ) in
    let () = connect sock server_addr in
    sock

  let create_server client port = 
    let sock = socket PF_INET SOCK_STREAM 0 in
    let () = setsockopt sock SO_REUSEADDR true in
    let client_addr = ADDR_INET( (gethostbyname client).h_addr_list.(0), port ) in
    let () = bind sock client_addr in
    let () = listen sock 1 in
    sock

  let accept_client sock = fst (accept sock)

  (* send value stored in byte buffer *)
  let send sock bytes = ignore @@ write sock bytes 0 (Bytes.length bytes)

  let send_int = 
    let b = Bytes.create 4 in
    (fun socket v ->
      for i=0 to 3 do
        Bytes.set b i (Char.of_byte ((v lsr (i*8)) land 255));
      done;
      send socket b)

  let send_int32 = 
    let b = Bytes.create 4 in
    (fun socket v ->
      for i=0 to 3 do
        Bytes.set b i (Char.of_byte Int32.(to_int (logand (shift_right v (i*8)) 255l)));
      done;
      send socket b)

  let send_int64 =
    let b = Bytes.create 8 in
    (fun socket v ->
      for i=0 to 7 do
        Bytes.set b i (Char.of_byte Int64.(to_int (logand (shift_right v (i*8)) 255L)));
      done;
      send socket b)

  let send_string socket str = 
    send_int socket (String.length str);
    send socket (Bytes.of_string str)

  let recv sock bytes len = 
    let rec f pos size =
      if size<=0 then () 
      else begin
        let l = read sock bytes pos size in
        let () = if l <= 0 then failwith "failed to read from socket" in
        f (pos+l) (size-l)
      end
    in
    f 0 len

  let recv_int32 = 
    let b = Bytes.create 4 in
    (fun socket ->
      let f i = Int32.shift_left (Int32.of_int @@ Char.to_int @@ Bytes.get b i) (i*8) in
      recv socket b 4;
      Int32.(logor
          (logor (f 0) (f 1))
          (logor (f 2) (f 3))))

  let recv_int socket = Int32.to_int @@ recv_int32 socket 

  let recv_int64 = 
    let b = Bytes.create 8 in
    (fun socket ->
      let f i = Int64.shift_left (Int64.of_int @@ Char.to_int @@ Bytes.get b i) (i*8) in
      recv socket b 4;
      Int64.(logor
        (logor
          (logor (f 0) (f 1))
          (logor (f 2) (f 3)))
        (logor
          (logor (f 4) (f 5))
          (logor (f 6) (f 7)))))

  let recv_string socket = 
    let len = recv_int socket in
    (*Printf.printf "recv_string %i\n" len;*)
    let b = Bytes.create len in
    recv socket b len;
    Bytes.to_string b

  let recv_string_is socket m = 
    let s = recv_string socket in
    if s <> m then 
      failwith ("failed to get expected string '" ^ m ^ "' got '" ^ s ^ "'")

end

(* constants that are used in the hardcaml-vpi c-code *)
let _FINISH = 0
let _RUN = 1

let control server message = 
  match message with
  | Finish -> ignore @@ Comms.send_int server _FINISH; []
  | Run{gets;sets;delta_time} -> begin
    ignore @@ Comms.send_int server _RUN;
    ignore @@ Comms.send_int64 server delta_time;
    ignore @@ Comms.send_int server (List.length sets);
    ignore @@ Comms.send_int server (List.length gets);
    List.iter 
      (fun (idx,b) -> 
        ignore @@ Comms.send_int server idx;
        List.iter (fun b -> ignore @@ Comms.send_int32 server b) b)
      sets;
    List.iter 
      (fun idx -> ignore @@ Comms.send_int server idx) 
      gets;
    List.map 
      (fun idx -> 
        let words = Comms.recv_int server in
        let rec f w = if w=0 then [] else Comms.recv_int32 server :: f (w-1) in
        idx, f words)
      gets
  end

let testbench_name name = name ^ "_hardcaml_testbench"
let instance_name name = "the_hardcaml_" ^ name

let write_testbench ?dump_file ~name ~inputs ~outputs os = 
  
  let declare net s = 
    let width = snd s in
    os ("  " ^ net ^ " ");
    if width > 1 then begin
      os "["; os (string_of_int (width - 1)); os ":0] "
    end;
    os (fst s);
    os ";\n"
  in

  os ("module " ^ name ^ "_hardcaml_testbench;\n");
  List.iter (declare "reg") inputs;
  List.iter (declare "wire") outputs;
  begin
    match dump_file with
    | Some(dump_file) -> begin
      os "  initial begin\n";
      os ("    $dumpfile(\"" ^ dump_file ^ "\");\n");
      os ("    $dumpvars(0, " ^ instance_name name ^ ");\n");
      os "  end\n";
    end
    | None -> ()
  end;
  os ("  " ^ name ^ " " ^ instance_name name ^ " (");
  let ports = List.map (fun s -> "." ^ fst s ^ "(" ^ fst s ^ ")") (inputs @ outputs) in
  os (String.concat ~sep:", "  ports);
  os ");\n";
  os "endmodule"

let write_testbench_from_circuit ?dump_file os circuit = 
  let open Signal.Types in
  let cname = Circuit.name circuit in
  let name s = List.hd (Signal.Types.names s) in
  let inputs = List.map (fun s -> name s, width s) (Circuit.inputs circuit) in
  let outputs = List.map (fun s -> name s, width s) (Circuit.outputs circuit) in
  write_testbench ?dump_file ~name:cname ~inputs ~outputs os

let derive_clocks_and_resets circuit =
  let open Signal.Types in
  let seq_elts = Circuit.find_signals (fun s -> is_reg s || is_mem s) (Circuit.outputs circuit) in
  let clocks_and_resets = 
    List.map (function
      | Signal_reg(_,r) -> r.reg_clock, r.reg_reset
      | Signal_mem(_,_,r,_) -> r.reg_clock, r.reg_reset
      | _ -> failwith "unexpected") seq_elts
  in
  let module SSet = Set.Make(struct type t = string let compare = compare end) in
  let unique_names l = 
    SSet.elements 
      (List.fold_left 
        (fun set s ->
          try SSet.add (List.hd (names s)) set
          with _ -> set) 
        SSet.empty l)
  in
  unique_names (List.map fst clocks_and_resets),
  unique_names (List.map snd clocks_and_resets)

module Icarus = struct

  let compile verilog vvp = 
    match Unix.system ("iverilog -o " ^ vvp ^ " " ^ (String.concat ~sep:" " verilog)) with
    | Unix.WEXITED(0) -> ()
    | _ -> failwith ("Failed to compile verilog to vvp")

  let load_sim vvp_file = 
    let command = 
      "LD_LIBRARY_PATH=$LD_LIBRARY_PATH:`ocamlc -where` vvp " ^
        "-M`opam config var hardcaml-vpi:lib` " ^
        "-mhc_ivl " ^ vvp_file
    in
    let _ = Unix.open_process_out command in
    ()

  let compile_and_load_sim ?dump_file circuit =
    let verilog_file_name = Filename.temp_file "hardcaml_cosim_" "_verilog" in
    let vvp_file_name = Filename.temp_file "hardcaml_cosim_" "_vvp" in
    let () = at_exit (fun _ -> Unix.unlink verilog_file_name; Unix.unlink vvp_file_name) in
    (* write RTL and testbench *)
    let verilog_file = open_out verilog_file_name in
    let () = Rtl.Verilog.write (output_string verilog_file) circuit in
    let () = write_testbench_from_circuit ?dump_file (output_string verilog_file) circuit in
    let () = close_out verilog_file in
    (* compile *)
    let () = compile [verilog_file_name] vvp_file_name in
    (* load simulation *)
    load_sim vvp_file_name

end

module Modelsim = struct

  (* we'll assume vlib work has been performed already *)
  let compile verilog _ = 
    match Unix.system ("vlog " ^ (String.concat ~sep:" " verilog)) with
    | Unix.WEXITED(0) -> ()
    | _ -> failwith ("Failed to compile verilog with modelsim")
   
  let load_sim tb =  
    let command = 
      "vsim -c -pli `opam config var hardcaml-vpi:lib`/hc_mti.vpi "^tb^" -do \"run -a\""
    in
    let _ = Unix.open_process_out command in
    ()

  let compile_and_load_sim ?dump_file circuit = 
    let verilog_file_name = Filename.temp_file "hardcaml_cosim_" "_verilog" in
    let () = at_exit (fun _ -> Unix.unlink verilog_file_name) in
    (* write RTL and testbench *)
    let verilog_file = open_out verilog_file_name in
    let () = Rtl.Verilog.write (output_string verilog_file) circuit in
    let () = write_testbench_from_circuit ?dump_file (output_string verilog_file) circuit in
    let () = close_out verilog_file in
    (* compile *)
    let () = compile [verilog_file_name] "" in
    (* load simulation *)
    let name = Circuit.name circuit ^ "_hardcaml_testbench" in
    load_sim name

end

module Make(B : Comb.S) = struct
  
  let to_i32l x = 
    let rec f x = 
      let w = B.width x in
      if w <= 32 then [B.to_int32 x]
      else 
        B.to_int32 (B.select x 31 0) :: f (B.select x (w-1) 32)
    in
    f x

  let of_i32l w x = 
    let rec f w = function
      | [] -> failwith "of_i32l"
      | h::[] -> assert (w <= 32); [B.consti32 w h]
      | h::t -> assert (w > 32); (B.consti32 32 h :: f (w-32) t)
    in
    B.concat (List.rev (f w x))

  let rec read_nets server = 
    let idx = Comms.recv_int server in
    if idx < 0 then []
    else
      let name = Comms.recv_string server in
      let width = Comms.recv_int server in
      (name,(idx,width)) :: read_nets server

  module SIM = Modelsim

  let init_sim start_sim inputs = 
    (* create server *)
    (*let () = Printf.printf "creating server...\n%!" in*)
    let server = Comms.create_server net_addr net_port in
    let _ = at_exit (fun _ -> Unix.close server) in

    (* start simulator *)
    let () = start_sim () in

    (* wait for connection *)
    (*let () = Printf.printf "accept...\n%!" in*)
    let server = Comms.accept_client server in
    
    (* say hello *)
    (*let () = Printf.printf "hello...\n%!" in*)
    let () = Comms.recv_string_is server "hello hardcaml" in
    (*let () = Printf.printf "got hello...\n%!" in*)
    let () = Comms.send_string server "hello verilog" in
    let nets = read_nets server in
 
    (* set all input ports to zero *)
    let sets = 
      List.map 
        (fun (name,w) ->
          let idx, w' = List.assoc name nets in
          assert (w = w');
          idx, to_i32l (B.zero w)) 
        inputs
    in
    let _ = control server 
      (Run { sets; gets = []; delta_time = 0L })
    in

    server, nets

  let make_sim_obj ~server ~clocks ~resets ~inputs ~outputs ~nets = 

    let find n = fst (List.assoc n nets) in

    let inputs = List.map (fun (n,b) -> n, find n, ref (B.zero b)) inputs in
    let outputs = List.map (fun (n,b) -> n, find n, b, ref (B.zero b)) outputs in

    (* clock cycle update *)
    let clocks_1 = List.map (fun (n,_) -> find n, to_i32l B.vdd) clocks in
    let clocks_0 = List.map (fun (n,_) -> find n, to_i32l B.gnd) clocks in
    let get_outputs = List.map (fun (_,n,_,_) -> n) outputs in
    let fcycle () = 
      let set_inputs = List.map (fun (_,n,v) -> n, to_i32l !v) inputs in
      let _ = control server
        (Run { sets = clocks_1; gets = []; delta_time = 0L; })
      in
      let _ = control server 
        (Run { sets = set_inputs; gets = []; delta_time = 5L; })
      in
      let res = control server
        (Run { sets = clocks_0; gets = get_outputs; delta_time = 5L; })
      in
      List.iter2 
        (fun (_,n,b,v) (n',v') -> assert (n = n'); v := of_i32l b v')
        outputs res
    in

    (* reset update *)
    let resets_1 = List.map (fun (n,_) -> find n, to_i32l B.vdd) resets in
    let resets_0 = List.map (fun (n,_) -> find n, to_i32l B.gnd) resets in
    let freset () = 
      let _ = control server
        (Run { sets = resets_1; gets = []; delta_time = 10L; })
      in
      let _ = control server 
        (Run { sets = resets_0; gets = []; delta_time = 0L; })
      in
      ()
    in

    (* simulation object *)
    Cyclesim.Api.({
      sim_in_ports = List.map (fun (n,_,v) -> n,v) inputs;
      sim_out_ports = List.map (fun (n,_,_,v) -> n,v) outputs;
      sim_out_ports_next = List.map (fun (n,_,_,v) -> n,v) outputs;
      sim_internal_ports = [];
      sim_reset = freset;
      sim_cycle_check = (fun () -> ());
      sim_cycle_comb0 = fcycle; (* XXX SPLIT ME UP, and this thing might work properly! XXX *)
      sim_cycle_seq = (fun () -> ());
      sim_cycle_comb1 = (fun () -> ());
      sim_lookup_signal = (fun uid -> failwith "sim_lookup_signal not implemented");
      sim_lookup_reg = (fun uid -> failwith "sim_lookup_reg not implemented");
      sim_lookup_memory = (fun uid -> failwith "sim_lookup_memory not implemented");
    })

  (* create simulator from hardcaml circuit *)
  let make ?dump_file circuit = 
    let open Signal.Types in

    (* query circuit for ports *)
    let port_name s = 
      match names s with
      | [n] -> n
      | _ -> failwith "not a port_name"
    in
    let get_port s = port_name s, width s in
    let inputs = List.map get_port (Circuit.inputs circuit) in
    let outputs = List.map get_port (Circuit.outputs circuit) in
 
    (* initialize server and simulation *)
    let server, nets = init_sim (fun () -> SIM.compile_and_load_sim ?dump_file circuit) inputs in

    (* create simulation object *)
    let clocks, resets = derive_clocks_and_resets circuit in
 
    (* remove clocks and resets from input ports *)
    let inputs = 
      let cr = clocks @ resets in
      (* inputs without clocks and resets *)
      List.filter (fun (n,_) -> not (List.mem n cr)) inputs 
    in

    let clocks, resets = List.map (fun n -> n,1) clocks, List.map (fun n -> n,1) resets in
    make_sim_obj ~server ~clocks ~resets ~inputs ~outputs ~nets

  let load ~clocks ~resets ~inputs ~outputs vvp_file =
    (* initialize server and simulation *)
    let server, nets = init_sim (fun () -> SIM.load_sim vvp_file) (clocks@resets@inputs) in

    (* create simulation object *)
    make_sim_obj ~server ~clocks ~resets ~inputs ~outputs ~nets

end

