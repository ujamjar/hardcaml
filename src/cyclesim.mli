(* 
 * hardcaml - hardware design in OCaml
 *
 *   (c) 2014 MicroJamJar Ltd
 *
 * Author(s): andy.ray@ujamjar.com
 * Description: 
 *
 *)

(** Cycle accurate simulator *)

open Signal.Types

(** circuit scheduler *)
val scheduler : (signal -> signal list) -> signal list -> signal list -> signal list

(** circuit searching *)
val find_elements : Circuit.t -> (signal list * signal list * signal list * signal list * signal list)

(** Cycle based simulator type and API *)
module Api : 
sig

    type task = unit -> unit 

    (** base type of the cycle based simulators *)
    type 'a cyclesim =
        {
            sim_in_ports : (string * 'a ref) list; 
            sim_out_ports : (string * 'a ref) list;
            sim_out_ports_next : (string * 'a ref) list;
            sim_internal_ports : (string * 'a ref) list;
            sim_reset : task;
            sim_cycle_check : task;
            sim_cycle_comb0 : task;
            sim_cycle_seq : task;
            sim_cycle_comb1 : task;
        }

    (** advance by 1 clock cycle (check->comb->seq->comb) *)
    val cycle : 'a cyclesim -> unit

    (** check inputs are valid before a simulation cycle *)
    val cycle_check : 'a cyclesim -> unit

    (** update combinatorial logic before sequential logic *)
    val cycle_comb0 : 'a cyclesim -> unit

    (** update sequential logic *)
    val cycle_seq : 'a cyclesim -> unit

    (** update combinatorial logic after sequential logic *)
    val cycle_comb1 : 'a cyclesim -> unit

    (** reset simulator *)
    val reset : 'a cyclesim -> unit

    (** get input port given a name *)
    val in_port : 'a cyclesim -> string -> 'a ref

    (** get output port given a name *)
    val out_port : 'a cyclesim -> string -> 'a ref
    
    (** get output port given a name *)
    val out_port_next : 'a cyclesim -> string -> 'a ref

    (** get internal port given a name *)
    val internal_port : 'a cyclesim -> string -> 'a ref

    (** get list of input ports *)
    val in_ports : 'a cyclesim -> (string * 'a ref) list

    (** get list of output ports *)
    val out_ports : 'a cyclesim -> (string * 'a ref) list

    (** get list of output ports *)
    val out_ports_next : 'a cyclesim -> (string * 'a ref) list

    (** get list of internal nodes *)
    val internal_ports : 'a cyclesim -> (string * 'a ref) list

end

(** Generate a simulator using the given Bits API *)
module Make : functor (Bits : Comb.S) -> (
sig

    type t

    type cyclesim = t Api.cyclesim

    type get_internal = (Signal.Types.signal -> bool) option
    type run_inst = Signal.Types.instantiation -> t list -> t list
    type get_inst = string -> run_inst option

    (** construct a simulator from a circuit *)
    val make : ?log:(string->unit) -> ?internal:get_internal -> 
        ?inst:get_inst ->
        Circuit.t -> cyclesim

    exception Sim_comparison_failure of int * string * string * string

    (** combine 2 simulators.  The inputs are set on the 1st simulator and
        copied to the 2nd.  Outputs are checked and any differences cause
        a Sim_comparison_failure exception. *)
    val combine_strict : cyclesim -> cyclesim -> cyclesim

    (** combine 2 simulators.  Similar to combine_strict except the 
        simulators may have different sets of input and output ports.
        Copying and checking only occurs on signals which exist in 
        both simulators. *)
    val combine_relaxed : cyclesim -> cyclesim -> cyclesim

    module InstOps : sig
        type add_inst = string -> run_inst -> signal array -> int array -> 
            signal array
        val make : unit -> get_inst * add_inst

        module Real(P : sig val mk : add_inst end) : sig
            module type Real = sig
                val (+:) : signal -> signal -> signal
                val (-:) : signal -> signal -> signal
                val ( *: ) : signal -> signal -> signal
                val (/:) : signal -> signal -> signal
                val (%:) : signal -> signal -> signal
                val ( **: ) : signal -> signal -> signal
                val exp : signal -> signal
                val log : signal -> signal
                val log10 : signal -> signal
                val cos : signal -> signal
                val sin : signal -> signal
                val tan : signal -> signal
                val acos : signal -> signal
                val asin : signal -> signal
                val atan : signal -> signal
                val atan2 : signal -> signal -> signal
                val cosh : signal -> signal
                val sinh : signal -> signal
                val tanh : signal -> signal
                val ceil : signal -> signal
                val floor : signal -> signal
                val abs : signal -> signal
            end
            module Float : Real
            module Double : Real
        end

    end

end with type t = Bits.t)

module Sim_obj_if : sig

  module type S = sig
    type t
    type i = <
      i : int -> unit;
      i32 : int32 -> unit;
      i64 : int64 -> unit;
      d : string -> unit;
      hu : string -> unit;
      hs : string -> unit;
      c : string -> unit;
      ibl : int list -> unit;
      bits : t ref;
    >
    val input : t ref -> i
    type o = <
      i : int;
      s : int;
      i32 : int32;
      s32 : int32;
      i64 : int64;
      s64 : int64;
      str : string;
      bits : t
    >
    val output : t ref -> o
  end

  module Make(B : Comb.S) : S with type t = B.t

end

