(* 
 * hardcaml - hardware design in OCaml
 *
 *   (c) 2014 MicroJamJar Ltd
 *
 * Author(s): andy.ray@ujamjar.com
 * Description: 
 *
 *)

open HardCaml

(** write a verilog netlist to the given channel from the given circuit *)
val write : out_channel -> Circuit.t -> unit

module Testbench :
sig

    val write : out_channel -> string list -> string list -> Circuit.t -> unit

end

module Hierarchy :
sig

    val write : ?transforms:(Transform.transform_fn list) -> 
                Circuit.Hierarchy.database -> 
                string -> Circuit.t -> unit

end

