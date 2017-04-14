(* 
 * hardcaml - hardware design in OCaml
 *
 *   (c) 2014 MicroJamJar Ltd
 *
 * Author(s): andy.ray@ujamjar.com
 * Description: 
 *
 *)

(** VHDL and Verilog netlist generation *)

module type Rtl_S = sig
  (** [write os circuit] writes [circuit] using [os] for output *)
  val write : (string -> unit) -> Circuit.t -> unit
end

module type OrderedString = (Map.OrderedType with type t = string)

(* control mapping of signals to their (various) names *)
module type SignalNaming = sig
    (* identifier case sensitivity *)
    module Case : OrderedString
    (* given a name, turn it into a legal identifier *)
    val prefix : string
    val reserved : string list
    val legalize : string -> string
end

module VerilogNames : SignalNaming
module VhdlNames : SignalNaming

(** VHDL generation *)
module Vhdl : Rtl_S

(** Verilog generation *)
module Verilog : Rtl_S

(** C model generations *)
module C : Rtl_S

(** Generate circuit with hierarchy *)
module Hierarchy : sig
  (** [write ~transforms ~database circuit_name write_module circuit] recursively scans 
      [circuit] and finds all sub-circuits (ie instantiations).  These are looked up in
      [database] and [write_module] is called for each sub-circuit.
      
      Appropriately designed circuits can thus be split over multiple layers of hierarchy
      and files. *)
  val write :
    ?transforms:(Transform.transform_fn list) -> Circuit.Hierarchy.database ->
    string -> (string -> Circuit.t -> unit) -> Circuit.t -> unit
end
