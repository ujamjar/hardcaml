(* 
 * hardcaml - hardware design in OCaml
 *
 *   (c) 2014 MicroJamJar Ltd
 *
 * Author(s): andy.ray@ujamjar.com
 * Description: 
 *
 *)

(** Perform simple constant propogation optimisations during circuit generation *)

(** Base combinatorial API *)
module Base : (Comb.T with type t = Signal.Types.signal)

(** Full combinatorial API *)
module Comb : (Comb.S with type t = Signal.Types.signal)
