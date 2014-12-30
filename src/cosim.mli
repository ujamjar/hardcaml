(* Icarus Verilog Cosimulation interface *)

(* run sets, then gets then schedule next callback at time+delta_time *)
type delta_message = 
  {
    sets : (string * string) list;
    gets : string list;
    delta_time : int64;
  }

(* expected inputs and outputs *)
type init_message = string list 

(* control message *)
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
val write_testbench : ?dump_file:string -> (string -> unit) -> Circuit.t -> unit
val compile : string -> string -> unit
val derive_clocks_and_resets : Circuit.t -> string list * string list
val compile_and_load_sim : ?dump_file:string -> Circuit.t -> unit

module Make(B : Comb.S) : sig
  val make : ?dump_file:string -> Circuit.t -> B.t Cyclesim.Api.cyclesim
end



