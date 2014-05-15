(* 
 * hardcaml - hardware design in OCaml
 *
 *   (c) 2014 MicroJamJar Ltd
 *
 * Author(s): andy.ray@ujamjar.com
 * Description: 
 *
 *)

(** Modules which implement {!modtype: Comb.S} as directly usable data-structures *)

(* exported to build the bigarray types in Bits_ext *)
module type ArraybitsBase =
sig

    type elt (* type of elements *)
    type barray (* type of array *)

    val nbits : int (* bit size of elements *)
    val words : int -> int
    val word : int -> int
    val create : int -> barray (* create array *)
    val mask : int -> elt (* create mask *)
    val mask_bit : int -> elt

    val to_bits : string -> barray
    val to_bstr : int -> barray -> string
    val to_int : barray -> int
    val of_int : int -> int -> barray 

    val zero : elt
    val one : elt

    val get : barray -> int -> elt
    val set : barray -> int -> elt -> unit

    (* operations on elements *)
    val (+.) : elt -> elt -> elt
    val (-.) : elt -> elt -> elt
    val (&.) : elt -> elt -> elt
    val (|.) : elt -> elt -> elt
    val (^.) : elt -> elt -> elt
    val (~.) : elt -> elt 
    val (>>.) : elt -> int -> elt
    val (<<.) : elt -> int -> elt

end

module ArraybitsBuilder(B : ArraybitsBase) : (Comb.T with type t = B.barray * int)
(*
module ArraybitsBuilder(B : ArraybitsBase) : Comb.T  
*)

(** Implemented API's *)
module Comb :
sig

    (** bits described as lists of ints ie [0;1;1;1;0] - width implicit as length of list*)
    module IntbitsList : (Comb.S with type t = int list)

    (** bits described with ocamls ints and a width (<=31) *)
    module Intbits : (Comb.S with type t = int*int)

    (** bits described using int32 and a width (<=32) *)
    module Int32bits : (Comb.S with type t = int32*int)
    
    (** bits described using int64 and a width (<=64) *)
    module Int64bits : (Comb.S with type t = int64*int)
    
    (** bits described using nativeint and a width (max size platform dependant) *)
    module Nativeintbits : (Comb.S with type t = nativeint*int)

    (** bits described using array of int32 *)
    module ArraybitsInt32 : (Comb.S with type t = int32 array * int)

    (** bits described using array of int64 *)
    module ArraybitsInt64 : (Comb.S with type t = int64 array * int)

    (** bits described using array of nativeint *)
    module ArraybitsNativeint : (Comb.S with type t = nativeint array * int)

end

