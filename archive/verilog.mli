(* 
 * hardcaml - hardware design in OCaml
 *
 *   (c) 2014 MicroJamJar Ltd
 *
 * Author(s): andy.ray@ujamjar.com
 * Description: 
 *
 *)

(** Generation of verilog netlists *)

(** write a verilog netlist to the given channel from the given circuit *)
val write : Circuit.t -> Buffer.t

module Testbench :
sig

    val write : string list -> string list -> Circuit.t -> Buffer.t

end

