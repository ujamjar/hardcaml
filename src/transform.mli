(* 
 * hardcaml - hardware design in OCaml
 *
 *   (c) 2014 MicroJamJar Ltd
 *
 * Author(s): andy.ray@ujamjar.com
 * Description: 
 *
 *)

(** transform circuits to a different representation *)

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

type transform_fn = (uid -> signal) -> signal -> signal

module type TransformFn =
sig
    (** function which will map signals to a new representation *)
    val transform : transform_fn
end

(** functor to build the function to map a signal to a new combinatorial representation *)
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

