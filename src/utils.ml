(* 
 * hardcaml - hardware design in OCaml
 *
 *   (c) 2014 MicroJamJar Ltd
 *
 * Author(s): andy.ray@ujamjar.com
 * Description: 
 *
 *)

open Astring

exception Failure of string
let failwith str = raise (Failure str)

type signed = Signed | Unsigned

(* detect if we are running on a 32 or 64 bit platform *)
let platform_bits =
    let min = Nativeint.min_int in
    let tst = Nativeint.shift_left 1n 31 in
    if min = tst then 32 else 64

(* some simple composition/pipelining operators *)
let (|>) x f = f x
let (>>) f g x = g (f x)
let (<<) g f x = g (f x)
let ($) f a = f a

(* Conversions *)

let list_of_string s = 
    let len = String.length s in
    let rec str i = 
        if i = len then []
        else s.[i] :: str (i+1)
    in
    str 0

let int_of_hchar c = 
  match c with
  | '0' -> 0
  | '1' -> 1
  | '2' -> 2
  | '3' -> 3
  | '4' -> 4
  | '5' -> 5
  | '6' -> 6
  | '7' -> 7
  | '8' -> 8
  | '9' -> 9
  | 'a' | 'A' -> 10
  | 'b' | 'B' -> 11
  | 'c' | 'C' -> 12
  | 'd' | 'D' -> 13
  | 'e' | 'E' -> 14
  | 'f' | 'F' -> 15
  | _ -> failwith "Invalid hex char"

let int_of_bchar = function
    | '0' -> 0
    | '1' -> 1
    | _ -> failwith ("int_of_bin_char: Invalid binary character encountered")

let t_of_bstr (lsl) (lor) zero one b =
    List.fold_left (fun acc v -> (acc lsl 1) lor (if v='1' then one else zero)) 
        zero (list_of_string b) 

let int_of_bstr = t_of_bstr (lsl) (lor) 0 1
let int32_of_bstr = t_of_bstr Int32.shift_left Int32.logor 0l 1l
let int64_of_bstr = t_of_bstr Int64.shift_left Int64.logor 0L 1L
let nativeint_of_bstr = t_of_bstr Nativeint.shift_left Nativeint.logor 0n 1n

let bstr_of_int w d = 
    let rec b i d =
        if i = w then ""
        else b (i+1) (d asr 1) ^ (if d land 1 = 1 then "1" else "0")
    in
    b 0 d

let bstr_of_int32 w d = 
    let module I = Int32 in
    let rec b i d =
        if i = w then ""
        else b (i+1) (I.shift_right d 1) ^ 
                     (if I.logand d 1l = 1l then "1" else "0")
    in
    b 0 d

let bstr_of_int64 w d = 
    let module I = Int64 in
    let rec b i d =
        if i = w then ""
        else b (i+1) (I.shift_right d 1) ^ 
                     (if I.logand d 1L = 1L then "1" else "0")
    in
    b 0 d

let bstr_of_nint w d = 
    let module I = Nativeint in
    let rec b i d =
        if i = w then ""
        else b (i+1) (I.shift_right d 1) ^ 
                     (if I.logand d 1n = 1n then "1" else "0")
    in
    b 0 d

let rec bstr_of_intbitslist = function
    | [] -> ""
    | h::t -> (if h = 1 then "1" else "0") ^ (bstr_of_intbitslist t)

let intbitslist_of_bstr s = 
    let len = String.length s in
    let rec make i = 
        if i = len then []
        else 
            (if s.[i] = '1' then 1 else 0) :: make (i+1)
    in
    make 0

let int_of_hstr s = 
    let len = String.length s in
    let v = ref 0 in
    for i = 0 to (len-1) do
        v := (!v lsl 4) lor (int_of_hchar s.[i])
    done;
    !v

let ssub v o l = String.Sub.to_string @@ String.sub ~start:o ~stop:(o+l) v

let bstr_of_hstr sign width hex =
    let len = String.length hex in
    let len4 = len * 4 in
    let rec make_string i =
        if i = 0 then ""
        else (make_string (i-1)) ^ (bstr_of_int 4 (int_of_hchar hex.[i-1])) in
    let result = make_string len in 
    if width < len4 then
        ssub result (len4-width) (width)
    else
        (String.v ~len:(width - len4) (fun _ -> (if sign = Signed then result.[0] else '0'))) ^ result

let rec hstr_of_bstr sign s = 
    let hex_of_bin s = 
        match s with
        | "0000" -> "0"
        | "0001" -> "1"
        | "0010" -> "2"
        | "0011" -> "3"
        | "0100" -> "4"
        | "0101" -> "5"
        | "0110" -> "6"
        | "0111" -> "7"
        | "1000" -> "8"
        | "1001" -> "9"
        | "1010" -> "a"
        | "1011" -> "b"
        | "1100" -> "c"
        | "1101" -> "d"
        | "1110" -> "e"
        | "1111" -> "f"
        | _ -> failwith "Invalid string"
    in
    let len = String.length s in
    match len with
    | 0 -> failwith "Invalid string"
    | 1 | 2 | 3 -> 
      hex_of_bin 
        ((if sign = Signed then String.v ~len:(4-len) (fun _ -> s.[0]) 
          else String.v ~len:(4-len) (fun _ -> '0')) ^ s)
    | 4 -> hex_of_bin s
    | _ -> hstr_of_bstr sign (ssub s 0 (len-4)) ^ hex_of_bin (ssub s (len-4) 4)

(* ... *)

let abits_of_bstr' platform_bits (|.) (<<.) zero one get set create b = 
    let width = String.length b in
    let words = (width + platform_bits - 1) / platform_bits in
    let a = create words zero in
    let rec build n =
        let word = n / platform_bits in
        let bit = n mod platform_bits in
        if b.[width - n - 1] = '1' then
            set a (word) ((get a word) |. (one <<. bit));
        if n <> 0 then
            build (n-1)
    in
    build (width-1);
    a

let bstr_of_abits' platform_bits (&.) (<<.) zero one get set width a = 
    if width = 0 then ""
    else
        let b = Bytes.make width '0' in
        let rec build n =
            let word = n / platform_bits in
            let bit = n mod platform_bits in
            if ((get a word) &. (one <<. bit)) <> zero then
                Bytes.set b (width - n - 1) '1';
            if n <> 0 then
                build (n-1)
        in
        build (width-1);
        Bytes.to_string b

(* ... *)

let abits_int32_of_bstr = abits_of_bstr' 32 (Int32.logor) (Int32.shift_left) 0l 1l 
    Array.get Array.set 
    Array.make

let bstr_of_abits_int32 = bstr_of_abits' 32 (Int32.logand) (Int32.shift_left) 0l 1l
    Array.get Array.set 

let abits_int64_of_bstr = abits_of_bstr' 64 (Int64.logor) (Int64.shift_left) 0L 1L 
    Array.get Array.set 
    Array.make

let bstr_of_abits_int64 = bstr_of_abits' 64 (Int64.logand) (Int64.shift_left) 0L 1L
    Array.get Array.set 

let abits_nint_of_bstr = abits_of_bstr' platform_bits (Nativeint.logor) (Nativeint.shift_left) 0n 1n 
    Array.get Array.set 
    Array.make

let bstr_of_abits_nint = bstr_of_abits' platform_bits (Nativeint.logand) (Nativeint.shift_left) 0n 1n
    Array.get Array.set 


(* number + list handling *)

let rec nbits x = 
    if x < 0 then failwith "arg to clog2 must be >= 0";
    match x with 0 | 1 -> 1 | x -> 1 + (nbits (x/2)) 

let clog2 x = 
    if x = 0 then 0
    else nbits (x-1) 

let rec pow2 n = 2 * (if n<=1 then 1 else pow2 (n-1))

let range n = 
    let rec r0 n i = if n = i then [] else i :: (r0 n (i+1)) in
    r0 n 0 

let lselect l lo hi = 
    let rec ls l idx lo hi = 
        if idx > hi then []
        else if idx < lo then ls (List.tl l) (idx+1) lo hi
        else (List.hd l) :: (ls (List.tl l) (idx+1) lo hi) in 
    ls l 0 lo hi 

(* Selects the even elements from a list *)
let leven l = 
  let rec r l n = 
    match l with
    | [] -> []
    | hd :: tl ->
      if (n land 1) = 0 
      then hd :: (r tl (n+1))
      else (r tl (n+1)) in
  r l 0
      
(* Selects the odd elements from a list *)
let lodd l = 
  let rec r l n = 
    match l with
    | [] -> []
    | hd :: tl ->
      if (n land 1) = 1 
      then hd :: (r tl (n+1))
      else (r tl (n+1)) in
  r l 0

let linit n f = 
    Array.init n f |> Array.to_list

let rec zip a b = 
    match a,b with
    | _,[] -> []
    | [],_ -> []
    | a::b,c::d -> (a,c) :: zip b d

(* split list of pairs into two separate lists *)
let unzip l = List.map fst l, List.map snd l

let pairs l = zip (leven l) (lodd l)

let iteri fn l = 
    let rec f i l = 
        match l with
        | [] -> ()
        | h :: t -> fn i h; f (i+1) t
    in
    f 0 l

let mapi fn l = 
    let rec f i l = 
        match l with 
        | [] -> []
        | h :: t -> fn i h :: f (i+1) t
    in
    f 0 l

let rec map2 fn a b =
    match a,b with
    | a::s,b::t -> fn a b :: map2 fn s t
    | _ -> []

let rec iter2 fn a b = 
    match a,b with
    | a::s,b::t -> fn a b; iter2 fn s t
    | _ -> ()

let memoize f = 
    let v = ref None in
    (fun a ->
        match !v with
        | Some(v,a') when a = a' -> v
        | _ ->
            let v' = f a in
            v := Some(v',a);
            v'
    )

let split_pow2 l =
    let len = List.length l in
    let ll = clog2 len in
    match len with
    | 0 -> [],[]
    | 1 -> l,[]
    | 2 -> [List.hd l],List.tl l
    | _ ->
        let ll = 1 lsl (ll-1) in
        lselect l 0 (ll-1), lselect l ll (len-1)


