(* 
 * hardcaml - hardware design in OCaml
 *
 *   (c) 2014 MicroJamJar Ltd
 *
 * Author(s): andy.ray@ujamjar.com
 * Description: 
 *
 *)

(** VCD file generation *)

(** Make vcd generator  from a simulator *)
module Make : functor (S : Comb.S) -> 
(sig

    open Signal.Types
    type t

    type cyclesim = t Cyclesim.Api.cyclesim

    (** wrap a simulator to generate a vcd file *)
    val wrap : (string->unit) -> cyclesim -> cyclesim

end with type t = S.t)

