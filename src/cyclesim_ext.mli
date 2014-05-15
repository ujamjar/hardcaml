(* 
 * hardcaml - hardware design in OCaml
 *
 *   (c) 2014 MicroJamJar Ltd
 *
 * Author(s): andy.ray@ujamjar.com
 * Description: 
 *
 *)

open Cyclesim

(** Interactive simulator *)
module Interactive : functor (B : Comb.S) -> 
sig

    (** Run interactive simulator *)
    val run : Pervasives.in_channel -> B.t Api.cyclesim -> unit

end

module BinaryIO :
sig

    type t = Bits_ext.Comb.BigarraybitsNativeint.t

    type cyclesim = t Api.cyclesim

    val wrap : 
        ?rdin:Pervasives.in_channel option ->
        ?wrin:Pervasives.out_channel option ->
        ?cmpout:Pervasives.in_channel option ->
        ?cmpfn:(string-> t -> t -> unit) ->
        ?wrout:Pervasives.out_channel option ->
        cyclesim -> cyclesim

end

