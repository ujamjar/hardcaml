(* 
 * hardcaml - hardware design in OCaml
 *
 *   (c) 2014 MicroJamJar Ltd
 *
 * Author(s): andy.ray@ujamjar.com
 * Description: 
 *
 *)

(** {2 transform circuits to a different representation} *)

module type CombBaseGates = sig
  type t
  val width : t -> int
  val const : string -> t
  val empty : t
  val select : t -> int -> int -> t
  val concat : t list -> t
  val wire : int -> t
  val to_int : t -> int
  val to_bstr : t -> string
  val to_string : t -> string
  val (<==) : t -> t -> unit
  val (--) : t -> string -> t
  val (~:) : t -> t 
  val (&:) : t -> t -> t
  val (|:) : t -> t -> t
  val (^:) : t -> t -> t
end

module MakeCombGates(S : CombBaseGates) : Comb.T with type t = S.t
module MakeGates(B : Comb.T)(S : sig
  val (~:) : B.t -> B.t 
  val (&:) : B.t -> B.t -> B.t
  val (|:) : B.t -> B.t -> B.t
  val (^:) : B.t -> B.t -> B.t
end) : Comb.T with type t = B.t

(** comb logic built from And-Invertor graphs *)
module MakeAig(B : Comb.T) : (Comb.T with type t = B.t)
(** comb logic built from NAND gates *)
module MakeNand(B : Comb.T) : (Comb.T with type t = B.t)
(** comb logic built from NOR gates *)
module MakeNor(B : Comb.T) : (Comb.T with type t = B.t)

open Signal.Types

module Signals : 
sig
    (** AIG circuits *)
    module Aig : (Comb.S with type t = signal)
    (** NAND circuits *)
    module Nand : (Comb.S with type t = signal)
    (** NOR circuits *)
    module Nor : (Comb.S with type t = signal)
end

type 'a transform_fn' = (uid -> 'a) -> signal -> 'a
type transform_fn = signal transform_fn'

module type TransformFn' =
sig
  type t
  val transform : t transform_fn'
  val rewrite : t transform_fn' -> signal UidMap.t -> signal list -> t list
  val rewrite_signals : t transform_fn' -> signal list -> t list
end

module type TransformFn =
sig
    (** function which will map signals to a new representation *)
    val transform : transform_fn
end

(** functor to build the function to map a signal to a new combinatorial signal representation *)
module MakePureCombTransform(B : Comb.T) : TransformFn' with type t = B.t

(** functor to build the function to map a signal to a new combinatorial signal representation *)
module MakeCombTransform(B : (Comb.T with type t = signal)) : TransformFn

(** AIG gate mapping *)
module AigTransform : TransformFn
(** NAND gate mapping *)
module NandTransform : TransformFn
(** NOR gate mapping *)
module NorTransform : TransformFn

(** simple copying transform *)
module CopyTransform : TransformFn

(** simplify concatentations and selects *)
module SimplifyBusses : TransformFn

(* simplify constants by sharing *)
module SimplifyConstants : TransformFn

(* constant propagation *)
module ConstantPropagation : TransformFn

(** rewrites the list of signals based on the given function *)
val rewrite_signals : transform_fn -> signal list -> signal list

(** rewrites a circuit *)
val rewrite_circuit : transform_fn -> Circuit.t -> Circuit.t

