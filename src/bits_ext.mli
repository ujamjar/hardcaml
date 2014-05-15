(* 
 * hardcaml - hardware design in OCaml
 *
 *   (c) 2014 MicroJamJar Ltd
 *
 * Author(s): andy.ray@ujamjar.com
 * Description: 
 *
 *)

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

module type T = 
sig
    include Comb.S

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

    include Comb.S

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
module Make(Conv : T) : (S with type t = Conv.t)

module BigarraybitsInt32_Bits : (Comb.S with type t = Utils_ext.ba32 * int)
module BigarraybitsInt64_Bits : (Comb.S with type t = Utils_ext.ba64 * int)
module BigarraybitsNativeint_Bits : (Comb.S with type t = Utils_ext.bani * int)

module Comb :
sig
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


