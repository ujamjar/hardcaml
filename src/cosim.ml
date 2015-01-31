(* 
 * hardcaml - hardware design in OCaml
 *
 *   (c) 2014 MicroJamJar Ltd
 *
 * Author(s): andy.ray@ujamjar.com
 * Description: 
 *
 *)

type delta_message = 
  {
    sets : (string * string) list;
    gets : string list;
    delta_time : int64;
  }

type init_message = string list 

type control_message = 
  | Finish
  | Run of delta_message

type response_message = (string * string) list

let net_addr = "localhost"
let net_port = 10101

module Comms = struct

  open Unix

  let empty = Bytes.create 0

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
  let send sock bytes = write sock bytes 0 (Bytes.length bytes)

  (* recv marshalled value to a buffer *)
  let recv sock = 
    let header = Bytes.create Marshal.header_size in
    let () = 
      if Marshal.header_size <> read sock header 0 Marshal.header_size then 
        failwith "recv_marshalled Marshal.header_size"
    in
    let data_size = Marshal.data_size header 0 in
    let data = Bytes.create data_size in
    let () = 
      if data_size <> read sock data 0 data_size then 
        failwith "recv_marshalled Marshal.data_size"
    in
    Bytes.concat empty [ header; data ]

  let send_string socket str = send socket (Marshal.to_bytes str [])
  let recv_string socket = (Marshal.from_bytes (recv socket) 0 : string)
  let recv_string_is socket expected = 
    let got = recv_string socket in
    if got <> expected then 
      failwith ("recv_string_is expected '" ^ expected ^ "' got '" ^ got ^ "'")

end

let control server message = 
  let _ = Comms.send server (Marshal.to_bytes message []) in
  match message with
  | Finish -> []
  | Run ({ gets; }) when gets=[] -> []
  | _ -> (Marshal.from_bytes (Comms.recv server) 0 : response_message)

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
  os (String.concat ", "  ports);
  os ");\n";
  os "endmodule"

let write_testbench_from_circuit ?dump_file os circuit = 
  let open Signal.Types in
  let cname = Circuit.name circuit in
  let name s = List.hd (Signal.Types.names s) in
  let inputs = List.map (fun s -> name s, width s) (Circuit.inputs circuit) in
  let outputs = List.map (fun s -> name s, width s) (Circuit.outputs circuit) in
  write_testbench ?dump_file ~name:cname ~inputs ~outputs os

let compile verilog vvp = 
  match Unix.system ("iverilog -o " ^ vvp ^ " " ^ (String.concat " " verilog)) with
  | Unix.WEXITED(0) -> ()
  | _ -> failwith ("Failed to compile verilog to vvp")

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
    List.fold_left 
      (fun set s ->
        try SSet.add (List.hd (names s)) set
        with _ -> set) 
      SSet.empty l
    |> SSet.elements
  in
  unique_names (List.map fst clocks_and_resets),
  unique_names (List.map snd clocks_and_resets)

let load_sim vvp_file = 
  let command = "`opam config var lib`/hardcaml/hardcaml_vvp.sh " ^ vvp_file in
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

module Make(B : Comb.S) = struct
  
  let is_legal_char c = c = '1' || c = '0'

  let rec is_legal s i = 
    try 
      if is_legal_char s.[i] then is_legal s (i+1)
      else false
    with _ -> 
      true
    
  let legalise_value s = 
    if is_legal s 0 then s
    else String.map (fun c -> if is_legal_char c then c else '0') s

  let init_sim start_sim inputs outputs = 
    (* create server *)
    let server = Comms.create_server net_addr net_port in
    let _ = at_exit (fun _ -> Unix.close server) in

    (* start simulator *)
    let () = start_sim () in

    (* wait for connection *)
    let server = Comms.accept_client server in
    
    (* say hello *)
    let () = Comms.recv_string_is server "hello hardcaml" in
    let _ = Comms.send server (Marshal.to_bytes (List.map fst (inputs@outputs)) []) in
 
    (* set all input ports to zero *)
    let _ = control server 
      (Run { sets = List.map (fun (n,w) -> n, B.to_bstr (B.zero w)) inputs;
             gets = []; delta_time = 0L })
    in

    server

  let make_sim_obj ~server ~clocks ~resets ~inputs ~outputs = 

    let inputs = List.map (fun (n,b) -> n, ref (B.zero b)) inputs in
    let outputs = List.map (fun (n,b) -> n, ref (B.zero b)) outputs in

    (* clock cycle update *)
    let clocks_1 = List.map (fun (n,_) -> n,"1") clocks in
    let clocks_0 = List.map (fun (n,_) -> n,"0") clocks in
    let get_outputs = List.map (fun (n,v) -> n) outputs in
    let fcycle () = 
      let set_inputs = List.map (fun (n,v) -> n, B.to_bstr !v) inputs in
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
        (fun (n,v) (n',v') -> assert (n = n'); v := B.const (legalise_value v'))
        outputs res
    in

    (* reset update *)
    let resets_1 = List.map (fun (n,_) -> n,"1") resets in
    let resets_0 = List.map (fun (n,_) -> n,"0") resets in
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
      sim_in_ports = inputs;
      sim_out_ports = outputs;
      sim_internal_ports = [];
      sim_reset = freset;
      sim_cycle_check = (fun () -> ());
      sim_cycle_comb0 = fcycle; (* XXX SPLIT ME UP, and this thing might work properly! XXX *)
      sim_cycle_seq = (fun () -> ());
      sim_cycle_comb1 = (fun () -> ());
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
    let server = init_sim (fun () -> compile_and_load_sim ?dump_file circuit) inputs outputs in

    (* create simulation object *)
    let clocks, resets = derive_clocks_and_resets circuit in
 
    (* remove clocks and resets from input ports *)
    let inputs = 
      let cr = clocks @ resets in
      (* inputs without clocks and resets *)
      List.filter (fun (n,_) -> not (List.mem n cr)) inputs 
    in

    let clocks, resets = List.map (fun n -> n,1) clocks, List.map (fun n -> n,1) resets in
    make_sim_obj ~server ~clocks ~resets ~inputs ~outputs

  let load ~clocks ~resets ~inputs ~outputs vvp_file =
    (* initialize server and simulation *)
    let server = init_sim (fun () -> load_sim vvp_file) (clocks@resets@inputs) outputs in

    (* create simulation object *)
    make_sim_obj ~server ~clocks ~resets ~inputs ~outputs

end

