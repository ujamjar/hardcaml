type delta_message = 
  {
    sets : (string * string) list;
    gets : string list;
    delta_time : int64;
  }

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
  (Marshal.from_bytes (Comms.recv server) 0 : response_message)

let cycle server message =
  let _ = control server
    (Run {
      sets = [ "clock", "1" ];
      gets = [];
      delta_time = 0L;
    })
  in
  let _ = control server 
   ( Run {
      sets = message.sets;
      gets = [];
      delta_time = 5L;
    })
  in
  control server
    (Run {
      sets = [ "clock", "0" ];
      gets = message.gets;
      delta_time = 5L;
    })

let start vlog_file = 
  let open Printf in
  (* create server *)
  let server = Comms.create_server net_addr net_port in
  let _ = at_exit (fun _ -> Unix.close server) in
  printf "created server\n%!";
  (* launch simulator *)
  let command = "`opam config var lib`/hardcaml/hardcaml_cosim.sh " ^ vlog_file in
  let _ = Unix.open_process_out command in
  printf "launched cosim\n%!";
  (* wait for connection *)
  let server = Comms.accept_client server in
  printf "accepted connection\n%!";
  (* say hello *)
  let () = Comms.recv_string_is server "hello hardcaml" in
  printf "got hello\n%!";
  let _ = Comms.send_string server "hello vpi" in
  printf "send hello\n%!";
  (* for now, return server *)
  server

