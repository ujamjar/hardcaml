(* 
 * hardcaml - hardware design in OCaml
 *
 *   (c) 2014 MicroJamJar Ltd
 *
 * Author(s): andy.ray@ujamjar.com
 * Description: 
 *
 *)

(** Utility functions used by the camlp5 syntax extention. *)

type 'a ports =
    {
        mutable ports : 'a array;
    }

val lookup : ('b -> 'a) -> 'b -> 'a

val set_port : 'a -> 'a ports -> int -> 'a -> unit

val get_port : 'a -> 'a ports -> int -> 'a

val get_ports : 'a ports -> 'a array

val mk_ports : unit -> 'a ports

val check_ports : 'a -> string -> 'a ports -> unit
