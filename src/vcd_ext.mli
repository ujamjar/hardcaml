(* 
 * hardcaml - hardware design in OCaml
 *
 *   (c) 2014 MicroJamJar Ltd
 *
 * Author(s): andy.ray@ujamjar.com
 * Description: 
 *
 *)

(** VCD generation for gtkwave*)

(** Drive the gtkwave waveform viewer *)
module Make : functor (S : Comb.S) -> 
(sig

    open Signal.Types
    type t

    type cyclesim = t Cyclesim.Api.cyclesim

    (** wrap a simulator to generate a vcd file *)
    val wrap : out_channel -> cyclesim -> cyclesim

    (** launch gtkwave to view the VCD output interactively *)
    val gtkwave : ?args:string -> cyclesim -> cyclesim

end with type t = S.t)


