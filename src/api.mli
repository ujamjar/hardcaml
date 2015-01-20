(* 
 * hardcaml - hardware design in OCaml
 *
 *   (c) 2014 MicroJamJar Ltd
 *
 * Author(s): andy.ray@ujamjar.com
 * Description: 
 *
 *)

(** Pre-built API functors *)

module type S = sig
  module B : Comb.S
  module Comb : module type of Signal.Comb
  module Seq : module type of Signal.Seq
  module S : module type of Cyclesim.Api
  module Cyclesim : module type of Cyclesim.Make(B)
  module Cosim : module type of Cosim.Make(B)
  module Vcd : module type of Vcd.Make(B)
  module Gtkwave : module type of Vcd_ext.Make(B) 
  module Interface : sig
    module Gen : module type of Interface.Gen(B)
    module Gen_cosim : module type of Interface.Gen_cosim(B)
    module Sim : module type of Interface.Sim(B)
  end
  type bits = B.t
  type signal = Comb.t
end

module Make(Bits : Comb.S) : S

include module type of Make(Bits.Comb.IntbitsList)
