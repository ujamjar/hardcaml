(* 
 * hardcaml - hardware design in OCaml
 *
 *   (c) 2014 MicroJamJar Ltd
 *
 * Author(s): andy.ray@ujamjar.com
 * Description: 
 *
 *)

(** LLVM based simulation *)

module type T = 
sig
    open HardCaml
    open HardCamlX

    (** base simulator type *)
    type cyclesim = Bits_ext.Comb.BigarraybitsNativeint.t Cyclesim.Api.cyclesim

    (** [make circuit] construct a simulator from [circuit] *)
    val make : Circuit.t -> cyclesim

    (** write simulator to bitcode file *)
    val write : string -> Circuit.t -> unit

    (** [load name] loads the bit code file called [name] *)
    val load : string -> cyclesim
end

module type S = 
sig
    open HardCaml

    type t
    type cyclesim = t Cyclesim.Api.cyclesim

    (** construct a simulator from a circuit *)
    val make : Circuit.t -> cyclesim

    (** write simulator to bitcode file *)
    val write : string -> Circuit.t -> unit

    (** load simulator from bitcode file *)
    val load : string -> cyclesim
end

(** Low level LLVM simulator which uses {!Bits_ext.Comb.BigarraybitsNativeint} as
    its interface type. Initial version. Generally works fine (except memories
    are not implemented) however, for large circuits it can take a very, very
    long time to compile. *)
module V1 : T

(** Low level LLVM simulator which uses {!Bits_ext.Comb.BigarraybitsNativeint} as
    its interface type. Broken version - mux's dont work. 
    This represents an initial attempt at the compilation time problem. *)
module V2 : T

(** LLVM simulator generator.  
    Interface is parameterized by the given Bits and simulator type *)
module Make(Base : T)(B : HardCamlX.Bits_ext.S) : (S with type t = B.t)


