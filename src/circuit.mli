(* 
 * hardcaml - hardware design in OCaml
 *
 *   (c) 2014 MicroJamJar Ltd
 *
 * Author(s): andy.ray@ujamjar.com
 * Description: 
 *
 *)

(** Creation and manipulation of hardware circuits *)

exception Failure of string

(** Generation of valid netlist names from signal names *)
module Mangler :
sig

    (** a name mangler is a mapping from strings to the next available integer which should be added to the name to make it unique *)
    type t = (string, int) Hashtbl.t

    (** create a new name mangler *)
    val make : string list -> t

    (** lookup the string in the mangler hash table to see if it exists.  If it does
        not, -1 is returned.  Otherwise the next available index is returned.  Note;
        it is possible that a name constructed with this index already exists! *)
    val lookup : t -> string -> int

    (** adds a name to the mangler, assuming it doesnt exist already - raises an exception if it does *)
    val addfresh : t -> string -> unit

    (** returns a mangled name from the given string.  Note; mangled strings are also
        added to cope with the likes of the following sequence;
      - mangle "hello" -> "hello" 
      - mangle "hello" -> "hello_0" 
      - mangle "hello_0" -> "hello_0_0" *)
    val mangle : t -> string -> string

end

(** circuit data structure *)
type t = 
    {
        circ_name : string; (** circuit name *)
        circ_id_to_sig : Signal.Types.signal Signal.Types.UidMap.t; (** map id's to signals *)
        circ_inputs : Signal.Types.signal list; (** circuit inputs *)
        circ_outputs : Signal.Types.signal list; (** circuit outputs *)
        circ_fanout : Signal.Types.UidSet.t Signal.Types.UidMap.t; (** fanout's of each signal *)
        circ_fanin : Signal.Types.UidSet.t Signal.Types.UidMap.t; (** fanin's of each signal *)
    }

(** create circuit data structure  *)
val make : string -> Signal.Types.signal list -> t

(** null search function *)
val id : ('a -> Signal.Types.signal -> 'a)

(** search circuit starting at given signal *)
val search1 : ('a -> Signal.Types.signal -> 'a) -> ('a -> Signal.Types.signal -> 'a) -> 'a -> Signal.Types.signal -> 'a

(** search circuit from each given signal *)
val search : ('a -> Signal.Types.signal -> 'a) -> ('a -> Signal.Types.signal -> 'a) -> 'a -> Signal.Types.signal list -> 'a 

(** create a set of UID's from each given signal *)
val set_of_signals : Signal.Types.signal list -> Signal.Types.UidSet.t

(** filter on signals *)
val filter : (Signal.Types.signal -> bool) -> Signal.Types.signal list -> Signal.Types.signal list

(** return circuit inputs *)
val inputs : t -> Signal.Types.signal list

(** return circuit outputs *)
val outputs : t -> Signal.Types.signal list

(** return circuit name *)
val name : t -> string

(** create a map of signal uids to mangled names *)
val mangle_names : string list -> string -> t -> (Signal.Types.uid -> int -> string) 

(** is the signal an input to the circuit *)
val is_input : t -> Signal.Types.signal -> bool

(** is the signal an output of the circuit *)
val is_output : t -> Signal.Types.signal -> bool

val signal_of_uid : t -> Signal.Types.uid -> Signal.Types.signal

val signal_map : t -> Signal.Types.signal Signal.Types.UidMap.t

(** construct a set of fanouts from nodes *)
val find_fanout : (Signal.Types.signal -> Signal.Types.signal list) ->
    Signal.Types.signal list -> 
    Signal.Types.UidSet.t Signal.Types.UidMap.t

val find_signals : (Signal.Types.signal -> bool) -> Signal.Types.signal list ->
    Signal.Types.signal list

(** find all signals which match the given name *)
val find_signals_by_name : string -> Signal.Types.signal list ->
    Signal.Types.signal list

(** compare 2 circuits to see if they are the same *)
val structural_compare : ?check_names:bool -> t -> t -> bool

(*
module Plugin :
sig

    type param_elt = 
        | Int of int
        | String of string
        | Float of float
        | Bool of bool
        | List of param_elt list
    type param = string * param_elt

    val get_int : param_elt -> int
    val get_string : param_elt -> string
    val get_float : param_elt -> float
    val get_bool : param_elt -> bool
    val get_list : param_elt -> param_elt list

    type input = string * int
    type get_inputs = param list -> input list
    type get_circuit = param list -> 
                         (string * Signal.Types.signal) list -> 
                         t

    val load : string -> 
               (param list -> param list) ->
               (input list -> input list) ->
               t list

    val register : 
        (param list * get_inputs * get_circuit) list -> unit

end 
*)

module Hierarchy :
sig

    type database
    val empty : unit -> database

    (* take a circuit, and return a (mangled) name for it *)
    val add : ?check_names:bool -> database -> t -> string

    (* return the circuit matching the (mangled) name *)
    val get : database -> string -> t option

end

