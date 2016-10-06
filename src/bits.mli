(* 
 * hardcaml - hardware design in OCaml
 *
 *   (c) 2014 MicroJamJar Ltd
 *
 * Author(s): andy.ray@ujamjar.com
 * Description: 
 *
 *)

open Comb

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

(** Implemented API's *)
module Comb :
sig

  (** bits described as lists of ints ie [0;1;1;1;0] - width implicit as length of list*)
  module IntbitsList : (S with type t = int list)

  (** bits described with ocamls ints and a width (<=31) *)
  module Intbits : (S with type t = int*int)

  (** bits described using int32 and a width (<=32) *)
  module Int32bits : (S with type t = int32*int)

  (** bits described using int64 and a width (<=64) *)
  module Int64bits : (S with type t = int64*int)

  (** bits described using nativeint and a width (max size platform dependant) *)
  module Nativeintbits : (S with type t = nativeint*int)

  (** bits described using array of int32 *)
  module ArraybitsInt32 : (S with type t = int32 array * int)

  (** bits described using array of int64 *)
  module ArraybitsInt64 : (S with type t = int64 array * int)

  (** bits described using array of nativeint *)
  module ArraybitsNativeint : (S with type t = nativeint array * int)

end

module Ext : sig

  (* bit's API's with conversions *)

  module Utils_ext : 
  sig
    type ba32 = (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t 
    type ba64 = (int64, Bigarray.int64_elt, Bigarray.c_layout) Bigarray.Array1.t 
    type bani = (nativeint, Bigarray.nativeint_elt, Bigarray.c_layout) Bigarray.Array1.t 

    (** converts a big_int to a binary string *)
    val bstr_of_big_int : int -> Big_int.big_int -> string

    (** converts a binary string to a big int *)
    val big_int_of_bstr : string -> Big_int.big_int

    (** binary Big_int.big_int to array of int32 *)
    val abits_int32_of_big_int : int -> Big_int.big_int -> int32 array

    (** array of int32 to binary Big_int.big_int *)
    val big_int_of_abits_int32 : int32 array -> Big_int.big_int

    (** binary Big_int.big_int to array of int32 *)
    val abits_int64_of_big_int : int -> Big_int.big_int -> int64 array

    (** array of int32 to binary Big_int.big_int *)
    val big_int_of_abits_int64 : int64 array -> Big_int.big_int

    (** binary Big_int.big_int to array of int32 *)
    val abits_nint_of_big_int : int -> Big_int.big_int -> nativeint array

    (** array of int32 to binary Big_int.big_int *)
    val big_int_of_abits_nint : nativeint array -> Big_int.big_int

    (** binary Big_int.big_int to big array of int32 *)
    val babits_int32_of_big_int : int -> Big_int.big_int -> ba32

    (** big array of int32 to binary Big_int.big_int *)
    val big_int_of_babits_int32 : ba32 -> Big_int.big_int

    (** binary Big_int.big_int to big array of int64 *)
    val babits_int64_of_big_int : int -> Big_int.big_int -> ba64

    (** big array of int64 to binary Big_int.big_int *)
    val big_int_of_babits_int64 : ba64 -> Big_int.big_int

    (** binary Big_int.big_int to big array of nativeint *)
    val babits_nint_of_big_int : int -> Big_int.big_int -> bani

    (** big array of native int to binary Big_int.big_int *)
    val big_int_of_babits_nint : bani -> Big_int.big_int

    (** binary string to big array of int32 *)
    val babits_int32_of_bstr : string -> ba32

    (** big array of int32 to binary string *)
    val bstr_of_babits_int32 : int -> ba32 -> string

    (** binary string to big array of int64 *)
    val babits_int64_of_bstr : string -> ba64

    (** big array of int64 to binary string *)
    val bstr_of_babits_int64 : int -> ba64 -> string

    (** binary string to big array of nativeint *)
    val babits_nint_of_bstr : string -> bani

    (** big array of native int to binary string *)
    val bstr_of_babits_nint : int -> bani -> string

    (** get pathname *)
    val filepath : string -> string

    (** get filename, including extension *)
    val filename : string -> string

    (** get filename, excluding extension *)
    val filebase : string -> string

    (** get filename extension, or [""] if none *)
    val fileext : string -> string

  end

  module BigarraybitsInt32_Bits : (S with type t = Utils_ext.ba32 * int)
  module BigarraybitsInt64_Bits : (S with type t = Utils_ext.ba64 * int)
  module BigarraybitsNativeint_Bits : (S with type t = Utils_ext.bani * int)

  module Comb :
  sig

    module type T = 
    sig
      include S

      (** is the data type mutable *)
      val is_mutable : bool

      (** create nativeint based big array from signal (if possible) - mutates input *)
      val to_bani_ptr : t -> Utils_ext.bani -> unit

      (** create signal from nativeint based big array - mutates input *)
      val of_bani_ptr : int -> Utils_ext.bani -> t -> t

      (** convert to Big_int *)
      val to_bigint : t -> Big_int.big_int

      (** convert from Big_int *)
      val of_bigint : int -> Big_int.big_int -> t

    end

    module type S = 
    sig

      include S

      (** is the data type mutable *)
      val is_mutable : bool

      (** create nativeint based big array from signal (if possible) - mutates input *)
      val to_bani_ptr : t -> Utils_ext.bani -> unit

      (** create signal from nativeint based big array - mutates input *)
      val of_bani_ptr : int -> Utils_ext.bani -> t -> t

      (** create nativeint based big array from signal (if possible) *)
      val to_bani : t -> Utils_ext.bani

      (** create signal from nativeint based big array *)
      val of_bani : int -> Utils_ext.bani -> t

      (** convert to Big_int *)
      val to_bigint : t -> Big_int.big_int

      (** convert from Big_int *)
      val of_bigint : int -> Big_int.big_int -> t

    end

    (** Generates the API with conversion functions *)
    module MakeC(Conv : T) : (S with type t = Conv.t)

    module IntbitsList : (S with type t = int list)

    module Intbits : (S with type t = int*int)
    module Int32bits : (S with type t = int32*int)
    module Int64bits : (S with type t = int64*int)
    module Nativeintbits : (S with type t = nativeint*int)

    module ArraybitsInt32 : (S with type t = int32 array * int)
    module ArraybitsInt64 : (S with type t = int64 array * int)
    module ArraybitsNativeint : (S with type t = nativeint array * int)

    module BigarraybitsInt32 : (S with type t = Utils_ext.ba32 * int)
    module BigarraybitsInt64 : (S with type t = Utils_ext.ba64 * int)
    module BigarraybitsNativeint : (S with type t = Utils_ext.bani * int)
  end

end

