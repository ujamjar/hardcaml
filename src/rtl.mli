(* 
 * hardcaml - hardware design in OCaml
 *
 *   (c) 2014 MicroJamJar Ltd
 *
 * Author(s): andy.ray@ujamjar.com
 * Description: 
 *
 *)

module type Rtl_S = sig
    val write : (string -> unit) -> Circuit.t -> unit
end

module Vhdl : Rtl_S
module Verilog : Rtl_S

module Hierarchy : sig
    val write :
        ?transforms:(Transform.transform_fn list) -> Circuit.Hierarchy.database ->
        string -> (string -> Circuit.t -> unit) -> Circuit.t -> unit
end
