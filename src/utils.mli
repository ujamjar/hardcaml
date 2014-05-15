(* 
 * hardcaml - hardware design in OCaml
 *
 *   (c) 2014 MicroJamJar Ltd
 *
 * Author(s): andy.ray@ujamjar.com
 * Description: 
 *
 *)

(** Utility functions *)

(** sign designator *)
type signed = Signed | Unsigned

val platform_bits : int

(** x |> f applies f to x *)
val (|>) : 'a -> ('a -> 'b) -> 'b

(** forward composition *)
val (>>) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c

(** reverse composition *)
val (<<) : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b

(** function chaining (avoiding brackets) *)
val ($) : ('a -> 'b) -> 'a -> 'b

(** converts a string to a list of chars *)
val list_of_string : string -> char list

(** Integer to hex character (0..15) *)
val int_of_hchar : char -> int

(** 0 or 1 -> '0' or '1' *)
val int_of_bchar : char -> int

(** converts a binary string to an integer *)
val int_of_bstr : string -> int

(** converts a binary string to an integer *)
val int32_of_bstr : string -> int32

(** converts a binary string to an integer *)
val int64_of_bstr : string -> int64

(** converts a binary string to an integer *)
val nativeint_of_bstr : string -> nativeint

(** converts an int to a binary string *)
val bstr_of_int : int -> int -> string
val bstr_of_int32 : int -> Int32.t -> string
val bstr_of_int64 : int -> Int64.t -> string
val bstr_of_nint : int -> Nativeint.t -> string

(** convert binary string to int bits list *)
val intbitslist_of_bstr : string -> int list

(** convert a list of bits (from type IntbitsList) to binary string *)
val bstr_of_intbitslist : int list -> string

(** Convert a hexidecimal string to an integer *)
val int_of_hstr : string -> int 

(** Convert a string in hexadecimal notation to a binary string. 
    If the hex string is shorter than the required width, and the value 
    is signed, the result is sign extended. *)
val bstr_of_hstr : signed -> int -> string -> string

(** convert a binary string to a hex string *)
val hstr_of_bstr : signed -> string -> string

(** binary string to array of int32 *)
val abits_int32_of_bstr : string -> int32 array

(** array of int32 to binary string *)
val bstr_of_abits_int32 : int -> int32 array -> string

(** binary string to array of int32 *)
val abits_int64_of_bstr : string -> int64 array

(** array of int32 to binary string *)
val bstr_of_abits_int64 : int -> int64 array -> string

(** binary string to array of int32 *)
val abits_nint_of_bstr : string -> nativeint array

(** array of int32 to binary string *)
val bstr_of_abits_nint : int -> nativeint array -> string

(** number of bits required to represent the given int *)
val nbits : int -> int

(** ceil(log(2,n)), n>=0  *)
val clog2 : int -> int

(** 2^n *)
val pow2 : int-> int

(** create list from [0...N] *)
val range : int -> int list

(** select elements from list; head of list first *) 
val lselect : 'a list -> int -> int -> 'a list

(** get even elements of list *)
val leven : 'a list -> 'a list

(** get odd elements of list *)
val lodd : 'a list -> 'a list

(* linit n f : initialise list of n elements *)
val linit : int -> (int -> 'a) -> 'a list

(** create list of pairs from two lists *)
val zip : 'a list -> 'b list -> ('a * 'b) list

(** split list of pairs into two separate lists *)
val unzip : ('a * 'b) list -> ('a list * 'b list) 

(** create pairs from list *)
val pairs : 'a list -> ('a * 'a) list

(** iterate over list with index  *)
val iteri : (int -> 'a -> unit) -> 'a list -> unit

(** map over list with index *)
val mapi : (int -> 'a -> 'b) -> 'a list -> 'b list

(** iterate over two lists calling the given function *)
val iter2 : ('a -> 'b -> unit) -> 'a list -> 'b list -> unit

(** map over two lists calling the given function *)
val map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list

(** memoization of a value *)
val memoize : ('a -> 'b) -> 'a -> 'b

(** split list on power of two boundary *)
val split_pow2 : 'a list -> 'a list * 'a list

