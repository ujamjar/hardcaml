(* 
 * hardcaml - hardware design in OCaml
 *
 *   (c) 2014 MicroJamJar Ltd
 *
 * Author(s): andy.ray@ujamjar.com
 * Description: 
 *
 *)

module Base : (Comb.T with type t = Signal.Types.signal)

module Comb : (Comb.S with type t = Signal.Types.signal)
