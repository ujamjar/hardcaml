(* Icarus Verilog Cosimulation interface *)

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

val net_addr : string
val net_port : int

module Comms : sig
  open Unix
  val create_client : string -> int -> file_descr
  val create_server : string -> int -> file_descr
  val accept_client : file_descr -> file_descr
  val send : file_descr -> Bytes.t -> int
  val recv : file_descr -> Bytes.t

  val send_string : file_descr -> string -> int
  val recv_string : file_descr -> string
  val recv_string_is : file_descr -> string -> unit
end

val control : Unix.file_descr -> control_message -> response_message
val cycle : Unix.file_descr -> delta_message -> response_message
val start : string -> Unix.file_descr

