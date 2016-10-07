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

exception Failure of string
let failure str = raise (Failure str)

(* Base API built using lists on ints.  
   Allows any bit precision and is simple, but slow. *)
module IntbitsListBase = 
struct
  open List

  (* msb first *)
  type t = int list

  let empty = []

  let width x = length x
  let const v = 
    let to_int = function '0' -> 0 | '1' -> 1 | _ -> failwith "invalid constant" in
    let len = String.length v in
    let rec const b i = 
      if len = i then b 
      else const (to_int v.[i] :: b) (i+1)
    in
    rev (const [] 0)

  let to_int x = List.fold_left (fun acc x -> (acc * 2) + x) 0 x

  let concat l = List.concat l

  let select s h l = 
    let rec sel b i = 
      match b with
      | [] -> []
      | hd::tl -> 
        if i > h then []
        else if i >= l then hd :: sel tl (i+1)
        else sel tl (i+1)
    in
    rev (sel (rev s) 0)

  let mux sel vals = 
    let len = length vals in
    let idx = to_int sel in
    nth vals (if idx >= len then len-1 else idx)

  let (&:) = List.map2 (land)
  let (|:) = List.map2 (lor)
  let (^:) = List.map2 (lxor)
  let (~:) = List.map (fun x -> if x = 1 then 0 else 1)

  let (==:) a b = 
    if fold_left2 (fun acc a b -> acc && (a=b)) true a b then [1] else [0]

  let (+:) a b = 
    let fa x y z = (x land y) lor (x land z) lor (y land z), x lxor y lxor z in
    (fst (List.fold_left2 (fun (res,carry) a b -> 
         let carry, sum = fa a b carry in 
         sum::res, carry) ([],0) (rev a) (rev b)))

  let (-:) a b = 
    let fs x y z = ((lnot x) land (y lor z)) lor (x land y land z), (x lxor y) lxor z in
    (fst (List.fold_left2 (fun (res,carry) a b -> 
         let carry, sum = fs a b carry in 
         sum::res, carry) ([],0) (rev a) (rev b)))

  let gnd = const "0"
  let rec repeat s n =
    if n = 0 then [] 
    else concat [s; repeat s (n-1)] 
  let zero = repeat gnd 
  let msb x = select x (width x - 1) (width x - 1)
  let bits = List.map (fun x -> [x])

  let ( *: ) a b = 
    let _,r = List.fold_left (fun (i,acc) b -> 
        let acc = concat [gnd; acc] in
        let a = concat [ gnd ; a ; repeat gnd i ] in
        i+1, (+:) acc ((&:) a (repeat b (width a)))
      ) (0,(zero (width a))) (rev (bits b)) in
    r

  let ( *+ ) a b = 
    let last = (width b) - 1 in
    let _,r = List.fold_left (fun (i,acc) b -> 
        let acc = concat [msb acc; acc] in
        let a = concat [ msb a; a ; repeat gnd i ] in
        i+1, (if i = last then (-:) else (+:)) acc ((&:) a (repeat b (width a)))
      ) (0,(zero (width a))) (rev (bits b)) in
    r

  let (<:) a b =
    let rec less a b = 
      match a,b with
      | [],[] -> false
      | a::t0,b::t1 -> 
        begin
          match a,b with
          | 0,1 -> true
          | 1,0 -> false
          | _ -> less t0 t1
        end
      | _ -> failwith "args not same width"
    in
    if less a b then [1] else [0]   

  let rec to_string b = 
    match b with 
    | [] -> ""
    | h::t -> (if h = 1 then "1" else "0") ^ (to_string t)

  let to_bstr = to_string

  let (<==) a b = ()
  let (--) a b = a
  let wire w = empty 

end

module type IntbitsBuilderType =
sig 
  type t
  val t_of_bstr : string -> t 
  val to_string : t -> string
  val bstr_of_t : int -> t -> string
  val to_int : t -> int
  val zero : t
  val one : t
  val mask : int -> t
  val se : int -> t -> t
  val (lsl) : t -> int -> t 
  val (lsr) : t -> int -> t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val (land) : t -> t -> t
  val (lor) : t -> t -> t
  val (lxor) : t-> t -> t
  val lnot : t -> t
  val (=) : t -> t -> t
  val (<) : t -> t -> t
end

(* Builds the base API over int, int32, int64 and nativeint types using a functor for abstraction *)
module IntApi =
struct

  type t = int
  let t_of_bstr = Utils.int_of_bstr
  let to_string a =  string_of_int a
  let bstr_of_t a = Utils.bstr_of_int a
  let to_int a = a 
  let zero = 0
  let one = 1
  let mask i = 
    if i = (Utils.platform_bits-1) then -1
    else if i = 0 then 0
    else (1 lsl i) - 1
  let se w b =
    let sign = b land (1 lsl (w-1)) in
    if sign <> 0 then b lor ((-1) lsl w) 
    else b
  let (lsl) = (lsl)
  let (lsr) = (lsr)
  let add = (+)
  let sub = (-)
  let mul = ( * )
  let (land) = (land)
  let (lor) = (lor)
  let (lxor) = (lxor)
  let lnot = lnot
  let (=) a b = if a = b then 1 else 0
  let (<) a b = 
    match a<0,b<0 with
    | true,false -> 0
    | false,true -> 1
    | _ -> if a < b then 1 else 0
end

module Int32Api = 
struct 
  open Int32
  open Utils
  type t = int32
  let t_of_bstr = int32_of_bstr
  let to_string = to_string
  let bstr_of_t w s = Utils.bstr_of_int32 w s
  let to_int = to_int
  let zero = 0l
  let one = 1l
  let mask i = 
    if i = 32 then -1l
    else if i = 0 then 0l
    else Int32.sub (shift_left 1l i) 1l
  let se w b =
    let sign = logand b (shift_left 1l (w-1)) in
    if sign <> 0l then logor b (shift_left (-1l) w) 
    else b
  let (lsl) = shift_left 
  let (lsr) = shift_right
  let add = add
  let sub = sub
  let mul = mul
  let (land) = logand
  let (lor) = logor
  let (lxor) = logxor
  let lnot = lognot
  let (=) a b = if a = b then 1l else 0l
  let (<) a b = 
    match a<0l,b<0l with
    | true,false -> 0l
    | false,true -> 1l
    | _ -> if a < b then 1l else 0l
end

module Int64Api = 
struct 
  open Int64
  open Utils
  type t = int64
  let t_of_bstr = int64_of_bstr
  let to_string = to_string
  let to_int = to_int
  let bstr_of_t w s = Utils.bstr_of_int64 w s
  let zero = 0L
  let one = 1L
  let mask i = 
    if i = 64 then -1L
    else if i = 0 then 0L
    else Int64.sub (shift_left 1L i) 1L
  let se w b =
    let sign = logand b (shift_left 1L (w-1)) in
    if sign <> 0L then logor b (shift_left (-1L) w) 
    else b
  let (lsl) = shift_left 
  let (lsr) = shift_right
  let add = add
  let sub = sub
  let mul = mul
  let (land) = logand
  let (lor) = logor
  let (lxor) = logxor
  let lnot = lognot
  let (=) a b = if a = b then 1L else 0L
  let (<) a b = 
    match a<0L,b<0L with
    | true,false -> 0L
    | false,true -> 1L
    | _ -> if a < b then 1L else 0L
end

module NativeintApi = 
struct 
  open Nativeint
  open Utils
  type t = nativeint
  let t_of_bstr = nativeint_of_bstr
  let to_string = to_string
  let to_int = to_int
  let bstr_of_t w s = Utils.bstr_of_nint w s
  let zero = 0n
  let one = 1n
  let mask i = 
    if i = Utils.platform_bits then -1n
    else if i = 0 then 0n
    else Nativeint.sub (shift_left 1n i) 1n
  let se w b =
    let sign = logand b (shift_left 1n (w-1)) in
    if sign <> 0n then logor b (shift_left (-1n) w) 
    else b
  let (lsl) = shift_left 
  let (lsr) = shift_right
  let add = add
  let sub = sub
  let mul = mul
  let (land) = logand
  let (lor) = logor
  let (lxor) = logxor
  let lnot = lognot
  let (=) a b = if a = b then 1n else 0n
  let (<) a b = 
    match a<0n,b<0n with
    | true,false -> 0n
    | false,true -> 1n
    | _ -> if a < b then 1n else 0n
end

module IntbitsBuilder = functor (Intbits : IntbitsBuilderType) ->
struct
  open Intbits

  type t = Intbits.t * int
  let is_mutable = false
  let empty = zero,0
  let width = snd
  let const b =  t_of_bstr b, String.length b
  let concat l = List.fold_left (fun (v,wid) (e,w) -> (v lsl w) lor e, wid + w) (zero,0) l 
  let mux sel l = 
    let len = List.length l in
    let idx = to_int (fst sel) in
    List.nth l (if idx >= len then len-1 else idx)

  let select s h l =
    let w = h - l + 1 in
    let v = fst s in
    (v lsr l) land (mask w), w
  let wire _ = empty
  let (--) a b = a
  let op2 op a b w = (op (fst a) (fst b)) land (mask w), w
  let (&:) a b = op2 (land) a b (width a)
  let (|:) a b = op2 (lor) a b (width a)
  let (^:) a b = op2 (lxor) a b (width a)
  let (~:) a = (lnot (fst a)) land (mask (width a)), width a
  let (+:) a b = op2 add a b (width a)
  let (-:) a b = op2 sub a b (width a)
  let ( *: ) a b = op2 mul a b (width a + width b)
  let se a = se (width a) (fst a), width a
  let ( *+ ) a b = op2 mul (se a) (se b) (width a + width b)
  let (==:) a b = (=) (fst a) (fst b), 1
  let (<:) a b = (<) (fst a) (fst b), 1
  let (<==)  a b = ()
  let to_string a = to_string (fst a)
  let to_int a = to_int (fst a)
  let to_bstr a = bstr_of_t (snd a) (fst a)
end

module IntbitsBase = IntbitsBuilder(IntApi)
module Int32bitsBase = IntbitsBuilder(Int32Api)
module Int64bitsBase = IntbitsBuilder(Int64Api)
module NativeintbitsBase = IntbitsBuilder(NativeintApi)

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

module ArraybitsInt32Api = 
struct

  type elt = int32
  type barray = int32 array

  let nbits = 32
  let words bits = (bits + nbits - 1) / nbits 
  let word bit = bit / nbits
  let create bits = 
    Array.make (words bits) 0l 

  let zero = 0l
  let one = 1l

  let mask n = if n = 0 then -1l else Int32.shift_right_logical (-1l) (nbits-n)
  let mask_bit n = Int32.shift_left 1l n

  let to_bits s = Utils.abits_int32_of_bstr s
  let to_bstr w s = Utils.bstr_of_abits_int32 w s
  let to_int s = Int32.to_int s.(0)
  let of_int bits i = 
    let b = create bits in
    b.(0) <- Int32.of_int i;
    b

  let get a n = a.(n)
  let set a n v = a.(n) <- v

  let (+.) = Int32.add
  let (-.) = Int32.sub
  let (&.) = Int32.logand
  let (|.) = Int32.logor
  let (^.) = Int32.logxor
  let (~.) = Int32.lognot
  let (>>.) = Int32.shift_right_logical
  let (<<.) = Int32.shift_left

end

module ArraybitsInt64Api = 
struct

  type elt = int64
  type barray = int64 array

  let nbits = 64
  let words bits = (bits + nbits - 1) / nbits 
  let word bit = bit / nbits
  let create bits = Array.make (words bits) 0L 

  let zero = 0L
  let one = 1L

  let mask n = if n = 0 then -1L else Int64.shift_right_logical (-1L) (nbits-n)
  let mask_bit n = Int64.shift_left 1L n

  let to_bits s = Utils.abits_int64_of_bstr s
  let to_bstr w s = Utils.bstr_of_abits_int64 w s
  let to_int s = Int64.to_int s.(0)
  let of_int bits i = 
    let b = create bits in
    b.(0) <- Int64.of_int i;
    b

  let get a n = a.(n)
  let set a n v = a.(n) <- v

  let (+.) = Int64.add
  let (-.) = Int64.sub
  let (&.) = Int64.logand
  let (|.) = Int64.logor
  let (^.) = Int64.logxor
  let (~.) = Int64.lognot
  let (>>.) = Int64.shift_right_logical
  let (<<.) = Int64.shift_left

end

module ArraybitsNativeintApi = 
struct

  type elt = nativeint
  type barray = nativeint array

  let nbits = Utils.platform_bits
  let words bits = (bits + nbits - 1) / nbits 
  let word bit = bit / nbits
  let create bits = Array.make (words bits) 0n 

  let zero = 0n
  let one = 1n

  let mask n = if n = 0 then -1n else Nativeint.shift_right_logical (-1n) (nbits-n)
  let mask_bit n = Nativeint.shift_left 1n n

  let to_bits s = Utils.abits_nint_of_bstr s
  let to_bstr w s = Utils.bstr_of_abits_nint w s
  let to_int s = Nativeint.to_int s.(0)
  let of_int bits i = 
    let b = create bits in
    b.(0) <- Nativeint.of_int i;
    b

  let get a n = a.(n)
  let set a n v = a.(n) <- v

  let (+.) = Nativeint.add
  let (-.) = Nativeint.sub
  let (&.) = Nativeint.logand
  let (|.) = Nativeint.logor
  let (^.) = Nativeint.logxor
  let (~.) = Nativeint.lognot
  let (>>.) = Nativeint.shift_right_logical
  let (<<.) = Nativeint.shift_left

end

module ArraybitsBuilder(B : ArraybitsBase) = 
struct

  open B

  type t = B.barray * int
  let is_mutable = true
  let empty = create 0, 0
  let width = snd

  let to_string x = to_bstr (width x) (fst x)
  let to_int x = to_int (fst x)
  let to_bstr x = to_bstr (width x) (fst x)

  let mask s = 
    let width = width s in
    let top_word = word (width-1) in
    let s = fst s in
    let mask = mask (width mod B.nbits) in 
    set s top_word ((get s top_word) &. mask);
    s, width

  let const b = to_bits b, String.length b
  let wire = empty
  let (--) a b = a

  let vdd =  
    let c = create 1 in
    set c 0 (mask_bit 0);
    c,1
  let gnd = create 1,1

  let op2 op a b = 
    let width = width a in
    let words = words width in
    let a = fst a in
    let b = fst b in
    let c = create width in
    for i=0 to words - 1 do
      set c i (op (get a i) (get b i))
    done;
    mask (c, width)

  let (&:) = op2 (&.)
  let (|:) = op2 (|.) 
  let (^:) = op2 (^.)

  let (~:) a = 
    let width = width a in
    let words = words width in
    let a = fst a in
    let c = create width in
    for i=0 to words - 1 do
      set c i (~. (get a i))
    done;
    mask (c, width)

  let bo2 = nbits/2
  let mask_lo = B.mask bo2
  let mask_hi = (B.mask bo2) <<. bo2
  let half_lo x = x &. mask_lo
  let half_hi x = (x &. mask_hi) >>. bo2
  let cbit = B.mask_bit bo2

  let (+:) a b = 
    let carry = ref zero in
    let set_carry x = 
      if zero <> (x &. cbit) then carry := one
      else carry := zero
    in
    op2 (fun a b ->
        let a' = half_lo a in
        let b' = half_lo b in
        let c_lo = a' +. b' +. !carry in
        set_carry c_lo;
        let a' = half_hi a in
        let b' = half_hi b in
        let c_hi = a' +. b' +. !carry in
        set_carry c_hi;
        (c_hi <<. bo2) |. (half_lo c_lo)
      ) a b

  let (-:) a b = 
    let borrow = ref zero in
    let set_borrow x = 
      if zero <> (x &. cbit) then borrow := one
      else borrow := zero
    in
    op2 (fun a b ->
        let a' = half_lo a in
        let b' = half_lo b in
        let c_lo = a' -. b' -. !borrow in
        set_borrow c_lo;
        let a' = half_hi a in
        let b' = half_hi b in
        let c_hi = a' -. b' -. !borrow in
        set_borrow c_hi;
        (c_hi <<. bo2) |. (half_lo c_lo)
      ) a b

  let iterback op a b =
    let words = words (width a) in
    let a = fst a in
    let b = fst b in
    for i=words-1 downto 0 do
      op (get a i) (get b i)
    done

  let (==:) a b = 
    let eq = ref true in
    iterback (fun a b -> if a <> b then eq := false) a b;
    if !eq then vdd
    else gnd

  let (<:) a b =
    let cmp = ref 0 in
    let set_cmp a b =
      if !cmp = 0 then
        (if a > b then cmp := 1 
         else if a < b then cmp := -1)
    in
    iterback (fun a b -> 
        let a' = half_hi a in
        let b' = half_hi b in
        set_cmp a' b';
        let a' = half_lo a in
        let b' = half_lo b in
        set_cmp a' b';
      ) a b;
    if !cmp = -1 then vdd
    else gnd

  let (<==) a b = ()

  let mux sel l = 
    let len = List.length l in
    let idx = to_int sel in
    let idx = if idx >= len then len-1 else idx in
    List.nth l idx

  let wire _ = empty

  (* XXX some of the harder operations *)

  let concat l = 
    let c_width = List.fold_left (fun a b -> a + width b) 0 l in
    let c_words = words c_width in
    let c = create c_width in
    let cat a b = 
      let a_width, a_words = width a, words (width a) in
      let b_width, b_words = width b, words (width b) in
      let a_bits = a_width mod nbits in
      let a,b = fst a, fst b in
      let _ = 
        if a_bits = 0 then
          (* aligned *)
          for i=0 to b_words - 1 do
            set a (a_words + i) (get b i)
          done
        else
          (* not aligned *)
          let merge x y = x |. (y <<. a_bits), y >>. (nbits - a_bits) in 
          for i=0 to b_words -1 do
            let x,y = merge (get a (a_words-1+i)) (get b i) in
            set a (a_words-1+i) x;
            if a_words+i < c_words then
              set a (a_words+i) y
          done
      in
      c, (a_width + b_width)
    in
    let l = List.rev l in
    if l = [] then empty
    else
      let c = cat (c,0) (List.hd l) in
      List.fold_left (fun c a -> cat c a) c (List.tl l)

  let select s h l = 
    let c_width = h-l+1 in
    let c_words = words c_width in
    let s_bits = l mod nbits in
    let c = create c_width in
    let lo_word = word l in
    let hi_word = word h in
    let s = fst s in
    let merge i = 
      let a = get s i in
      let b = if i >= hi_word then zero else get s (i+1) in
      (a >>. s_bits) |. (b <<. (nbits-s_bits))
    in
    let _ = 
      if s_bits = 0 then
        for i=0 to c_words-1 do
          set c i (get s (lo_word + i))
        done
      else
        for i=0 to c_words-1 do
          set c i (merge (lo_word+i))
        done
    in
    mask (c, (h-l)+1)


  (* a few functions introduced a earlier for the multipliers *)
  let (@:) a b = concat [a;b] 
  let consti bits n = of_int bits n, bits
  let zero n = consti n 0 
  let uresize x w = 
    if width x = w then x
    else if width x > w then select x (w-1) 0
    else zero (w - width x) @: x
  let msb a = let n = width a - 1 in select a n n 
  let negate x = zero (width x) -: x 
  let se a = msb a @: a

  let umul n a b = 
    let wc = width a + width b in

    (* we need some API functions that dont exist yet... *)
    let (+:) a b = 
      let w = max (width a) (width b) + 1 in
      let a, b = uresize a w, uresize b w in
      a +: b
    in

    let rec split s =
      if width s <= n then [s]
      else select s (n-1) 0 :: 
           split (select s (width s - 1) n)
    in
    let a, b = split a, split b in

    (* create products and sum equal powers 
       Each partial product is implicitly multiplied by (2^n)^m where m 
       depends on the position within a and b.
       During the outer recursion 'prod_b' we multiply each part of
       b with all of a.  

       This will start by generating m=0,1,2,...

       On the 2nd recursion we use the next part of b and generate
       m=1,2,3... 

       What we do is keep the paritial products form the previous step
       in prod_b, excluding the 1st product (which is fully generated),
       and sum it with the current products.  At the end we will have summed
       all products. For example, if a has 3 parts, and b has 4 parts the
       recursion will be

       m       prev
       [0;1;2] []
       [1;2;3] [1;2]
       [2;3;4] [2;3]
       [3;4;5] [3;4]
    *)
    let sums = 
      let ( * ) a b = consti (n*2) (to_int a * to_int b) in
      let rec map2 f a b = 
        match a,b with
        | a'::a'',b'::b'' -> f a' b' :: map2 f a'' b''
        | [],_ -> b
        | _,[] -> a
      in
      let rec prod_a a b = 
        match a with
        | a' :: a'' -> a' * b :: prod_a a'' b
        | [] -> []
      in
      let rec prod_b a b prev = 
        match b with
        | b' :: b'' ->
          let a' = prod_a a b' in
          let a' = map2 (+:) a' prev in (* add powers from previous step*)
          (* drop lowest power and recurse *)
          List.hd a' :: prod_b a b'' (List.tl a')
        | [] -> prev
      in
      prod_b a b []
    in

    (* add powers *)
    let scale s n = if n=0 then s else s @: zero n in
    let c,_ = 
      List.fold_left (fun (acc,scl) e -> acc +: scale e scl, scl+n) 
        (gnd, 0) sums
    in
    let c = select c (wc - 1) 0 in
    c

  let smul n a b = 
    let to_bool x = to_int x <> 0 in
    let wa, wb = width a, width b in
    let na, nb = to_bool (msb a), to_bool (msb b) in
    let a = if na then negate (se a) else a in
    let b = if nb then negate (se b) else b in
    let c = umul n a b in
    let c = if na=nb then c else negate c in
    select c (wa+wb-1) 0

  let ( *: ) = umul 15
  let ( *+ ) = smul 15

end

module ArraybitsInt32Base = ArraybitsBuilder(ArraybitsInt32Api)
module ArraybitsInt64Base = ArraybitsBuilder(ArraybitsInt64Api)
module ArraybitsNativeintBase = ArraybitsBuilder(ArraybitsNativeintApi)

module Comb =
struct
  module IntbitsList = Make(IntbitsListBase)
  module Intbits = Make(IntbitsBase)
  module Int32bits = Make(Int32bitsBase)
  module Int64bits = Make(Int64bitsBase)
  module Nativeintbits = Make(NativeintbitsBase)
  module ArraybitsInt32 = Make(ArraybitsInt32Base)
  module ArraybitsInt64 = Make(ArraybitsInt64Base)
  module ArraybitsNativeint = Make(ArraybitsNativeintBase)
end

module Ext = struct

  module Utils_ext = 
  struct
    type ba32 = (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t 
    type ba64 = (int64, Bigarray.int64_elt, Bigarray.c_layout) Bigarray.Array1.t 
    type bani = (nativeint, Bigarray.nativeint_elt, Bigarray.c_layout) Bigarray.Array1.t 

    let platform_bits = Utils.platform_bits

    (** Converts a big int to a binary string *)
    let bstr_of_big_int width b = 

      let b = ref b in
      let one = Big_int.unit_big_int in
      let two = Big_int.big_int_of_int 2 in

      let str = String.make width '0' in
      let len = String.length str in

      let rec make_string i = 
        if i = 0 then ""
        else (
          let q,m = Big_int.quomod_big_int !b two in
          b := q;
          (make_string (i-1)) ^ (if Big_int.eq_big_int m one then "1" else "0")
        ) in
      make_string len 

    let big_int_of_bstr b = 
      let zero = Big_int.zero_big_int in
      let len = String.length b in
      let rec make v n = 
        if n=len then v 
        else
          let s = 
            if b.[n] = '1' then 1
            else 0
          in
          let s = Big_int.add_int_big_int s (Big_int.mult_int_big_int 2 v) in
          make s (n+1)
      in
      make zero 0

    (* ... *)

    let ba_create kind len init = 
      let b = Bigarray.Array1.create kind Bigarray.c_layout len in
      for i=0 to len-1 do
        b.{i} <- init
      done;
      b

    let abits_of_big_int' nbits (&.) (|.) (~.) (<<.) mone zero one get set create of_bi w b =
      let div = Big_int.big_int_of_int (1 lsl (nbits/2)) in 
      let a = create ((w+nbits-1)/nbits) zero in
      let rec make v i bits = 
        if bits <= 0 then a
        else 
          let q,m = Big_int.quomod_big_int v div in
          let m = of_bi m in
          let _ = 
            if i mod 2 = 0 then set a (i/2) m
            else set a (i/2) ((get a (i/2)) |. (m <<. (nbits/2)))
          in
          make q (i+1) (bits-(nbits/2))
      in
      let mask s = 
        let top_word = (w-1) / nbits in
        let bits = w mod nbits in
        let mask = if bits=0 then mone else ~. (mone <<. bits) in
        set s top_word ((get s top_word) &. mask); 
        s
      in
      mask (make b 0 w)

    let big_int_of_abits' nbits (&.) (<<.) (>>.) mone one get length to_bi b = 
      let mask = mone >>. (nbits/2) in
      let bit = one <<. (nbits/2) in
      let multf = to_bi bit in 
      let words = length b in
      let rec make n v = 
        if n < 0 then v
        else
          let vi = to_bi (((get b n) >>. (nbits/2)) &. mask) in
          let v = Big_int.add_big_int vi (Big_int.mult_big_int v multf) in
          let vi = to_bi ((get b n) &. mask) in
          let v = Big_int.add_big_int vi (Big_int.mult_big_int v multf) in
          make (n-1) v
      in
      make (words-1) Big_int.zero_big_int

    (* ... *)

    let abits_int32_of_big_int = abits_of_big_int'
        32 Int32.logand Int32.logor Int32.lognot Int32.shift_left 
        (-1l) 0l 1l
        Array.get Array.set Array.make Big_int.int32_of_big_int

    let abits_int64_of_big_int = abits_of_big_int'
        64 Int64.logand Int64.logor Int64.lognot Int64.shift_left 
        (-1L) 0L 1L
        Array.get Array.set Array.make Big_int.int64_of_big_int

    let abits_nint_of_big_int = abits_of_big_int'
        platform_bits Nativeint.logand Nativeint.logor Nativeint.lognot Nativeint.shift_left 
        (-1n) 0n 1n
        Array.get Array.set Array.make Big_int.nativeint_of_big_int

    let big_int_of_abits_int32 = big_int_of_abits'
        32 Int32.logand Int32.shift_left Int32.shift_right_logical
        (-1l) 1l
        Array.get Array.length Big_int.big_int_of_int32

    let big_int_of_abits_int64 = big_int_of_abits'
        64 Int64.logand Int64.shift_left Int64.shift_right_logical
        (-1L) 1L
        Array.get Array.length Big_int.big_int_of_int64

    let big_int_of_abits_nint = big_int_of_abits'
        platform_bits Nativeint.logand Nativeint.shift_left Nativeint.shift_right_logical
        (-1n) 1n
        Array.get Array.length Big_int.big_int_of_nativeint

    (* ... *)

    let babits_int32_of_big_int = abits_of_big_int'
        32 Int32.logand Int32.logor Int32.lognot Int32.shift_left 
        (-1l) 0l 1l
        Bigarray.Array1.get Bigarray.Array1.set (ba_create Bigarray.int32) Big_int.int32_of_big_int

    let babits_int64_of_big_int = abits_of_big_int'
        64 Int64.logand Int64.logor Int64.lognot Int64.shift_left 
        (-1L) 0L 1L
        Bigarray.Array1.get Bigarray.Array1.set (ba_create Bigarray.int64) Big_int.int64_of_big_int

    let babits_nint_of_big_int = abits_of_big_int'
        platform_bits Nativeint.logand Nativeint.logor Nativeint.lognot Nativeint.shift_left 
        (-1n) 0n 1n
        Bigarray.Array1.get Bigarray.Array1.set (ba_create Bigarray.nativeint) Big_int.nativeint_of_big_int

    let big_int_of_babits_int32 = big_int_of_abits'
        32 Int32.logand Int32.shift_left Int32.shift_right_logical
        (-1l) 1l
        Bigarray.Array1.get Bigarray.Array1.dim Big_int.big_int_of_int32

    let big_int_of_babits_int64 = big_int_of_abits'
        64 Int64.logand Int64.shift_left Int64.shift_right_logical
        (-1L) 1L
        Bigarray.Array1.get Bigarray.Array1.dim Big_int.big_int_of_int64

    let big_int_of_babits_nint = big_int_of_abits'
        platform_bits Nativeint.logand Nativeint.shift_left Nativeint.shift_right_logical
        (-1n) 1n
        Bigarray.Array1.get Bigarray.Array1.dim Big_int.big_int_of_nativeint

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

    let babits_int32_of_bstr = abits_of_bstr' 32 (Int32.logor) (Int32.shift_left) 0l 1l 
        Bigarray.Array1.get Bigarray.Array1.set 
        (ba_create Bigarray.int32)

    let bstr_of_babits_int32 = bstr_of_abits' 32 (Int32.logand) (Int32.shift_left) 0l 1l
        Bigarray.Array1.get Bigarray.Array1.set 

    let babits_int64_of_bstr = abits_of_bstr' 64 (Int64.logor) (Int64.shift_left) 0L 1L 
        Bigarray.Array1.get Bigarray.Array1.set 
        (ba_create Bigarray.int64)

    let bstr_of_babits_int64 = bstr_of_abits' 64 (Int64.logand) (Int64.shift_left) 0L 1L
        Bigarray.Array1.get Bigarray.Array1.set 

    let babits_nint_of_bstr = abits_of_bstr' platform_bits (Nativeint.logor) (Nativeint.shift_left) 0n 1n 
        Bigarray.Array1.get Bigarray.Array1.set 
        (ba_create Bigarray.nativeint)

    let bstr_of_babits_nint = bstr_of_abits' platform_bits (Nativeint.logand) (Nativeint.shift_left) 0n 1n
        Bigarray.Array1.get Bigarray.Array1.set 

    (* filename operations *)

    let filepath f = Filename.dirname f
    let filename f = Filename.basename f
    let filebase f = try Filename.chop_extension (filename f) with _ -> filename f
    let fileext f = 
      let f = filename f in
      let b = filebase f in
      if b = f then ""
      else
        String.sub f (String.length b) (String.length f - 1)

  end

  module IntbitsList_Conv = 
  struct
    include Comb.IntbitsList
    let is_mutable = false
    let to_bani_ptr x b = 
      let c = Utils_ext.babits_nint_of_bstr (to_bstr x) in
      for i=0 to Bigarray.Array1.dim c - 1 do
        b.{i} <- c.{i}
      done
    let of_bani_ptr w b _ = const (Utils_ext.bstr_of_babits_nint w b)
    let to_bigint s = Utils_ext.big_int_of_bstr (Utils.bstr_of_intbitslist s)
    let of_bigint w s = Utils.intbitslist_of_bstr (Utils_ext.bstr_of_big_int w s)
  end

  module Intbits_Conv = 
  struct
    include Comb.Intbits
    let is_mutable = false
    let to_nint = Nativeint.of_int
    let of_nint = Nativeint.to_int
    let to_bint = Big_int.big_int_of_int
    let of_bint = Big_int.int_of_big_int
    let to_bani_ptr a b =
      b.{0} <- to_nint (fst a)
    let of_bani_ptr w b _ = of_nint (b.{0}), w
    let to_bigint s = to_bint (fst s)
    let of_bigint w s = of_bint s, w
  end
  module Int32bits_Conv = 
  struct
    include Comb.Int32bits
    let is_mutable = false
    let to_nint = Nativeint.of_int32
    let of_nint = Nativeint.to_int32
    let to_bint = Big_int.big_int_of_int32
    let of_bint = Big_int.int32_of_big_int
    let to_bani_ptr a b =
      b.{0} <- to_nint (fst a)
    let of_bani_ptr w b _ = of_nint (b.{0}), w
    let to_bigint s = to_bint (fst s)
    let of_bigint w s = of_bint s, w
  end
  module Int64bits_Conv = 
  struct
    include Comb.Int64bits
    let is_mutable = false
    (* cant find a way to do this with 64 bit precision *)
    let to_nint = Int64.to_nativeint 
    let of_nint = Int64.of_nativeint
    let to_bint = Big_int.big_int_of_int64
    let of_bint = Big_int.int64_of_big_int
    let to_bani_ptr a b =
      b.{0} <- to_nint (fst a)
    let of_bani_ptr w b _ = of_nint (b.{0}), w
    let to_bigint s = to_bint (fst s)
    let of_bigint w s = of_bint s, w
  end
  module Nativeintbits_Conv = 
  struct
    include Comb.Nativeintbits
    let is_mutable = false
    let to_nint x = x
    let of_nint x = x
    let to_bint = Big_int.big_int_of_nativeint
    let of_bint = Big_int.nativeint_of_big_int
    let to_bani_ptr a b =
      b.{0} <- to_nint (fst a)
    let of_bani_ptr w b _ = of_nint (b.{0}), w
    let to_bigint s = to_bint (fst s)
    let of_bigint w s = of_bint s, w
  end

  module ArraybitsInt32_Conv = 
  struct
    include Comb.ArraybitsInt32
    let is_mutable = true

    let to_bani_ptr a b = 
      if Utils.platform_bits = 32 then
        for i=0 to Bigarray.Array1.dim b - 1 do
          b.{i} <- Nativeint.of_int32 a.(i)
        done
      else
        for i=0 to Bigarray.Array1.dim b - 1 do
          let x = Nativeint.of_int32 a.((i*2)+0) in
          let y = Nativeint.of_int32 a.((i*2)+1) in
          b.{i} <- Nativeint.logor (Nativeint.shift_left y 32) x
        done

    let of_bani_ptr w b t =
      let _ = 
        if Utils.platform_bits = 32 then 
          for i=0 to Array.length t - 1 do
            t.(i) <- Nativeint.to_int32 b.{i}
          done
        else
          for i=0 to Array.length t - 1 do
            let x = b.{i/2} in
            let x = 
              if i mod 2 = 0 then Nativeint.to_int32 x 
              else Nativeint.to_int32 (Nativeint.shift_right_logical x 32)
            in
            t.(i) <- x
          done
      in
      t

    let to_bigint s = Utils_ext.big_int_of_abits_int32 s
    let of_bigint w s = Utils_ext.abits_int32_of_big_int w s

    let to_bani_ptr a b = to_bani_ptr (fst a) b
    let of_bani_ptr w b a = of_bani_ptr w b (fst a), w
    let to_bigint s = to_bigint (fst s)
    let of_bigint w s = of_bigint w s, w

  end

  module ArraybitsInt64_Conv = 
  struct
    include Comb.ArraybitsInt64
    let is_mutable = true

    let to_bani_ptr a b = 
      if Utils.platform_bits = 64 then
        for i=0 to Bigarray.Array1.dim b - 1 do
          b.{i} <- Int64.to_nativeint a.(i)
        done
      else
        for i=0 to Array.length a - 1 do
          let x = Int64.to_nativeint a.(i) in
          let y = Int64.to_nativeint (Int64.shift_right_logical a.(i) 32) in
          b.{(i*2)+0} <- x;
          b.{(i*2)+1} <- y
        done

    let of_bani_ptr w b t = 
      let _ = 
        if Utils.platform_bits = 64 then
          for i=0 to Array.length t - 1 do
            t.(i) <- Int64.of_nativeint b.{i}
          done
        else
          for i=0 to Array.length t - 1 do
            if i mod 2 = 0 then
              t.(i) <- Int64.of_nativeint b.{i}
            else
              t.(i) <- 
                Int64.logor
                  t.(i) 
                  (Int64.shift_left (Int64.of_nativeint b.{i}) 32)

          done
      in
      t

    let to_bigint s = Utils_ext.big_int_of_abits_int64 s
    let of_bigint w s = Utils_ext.abits_int64_of_big_int w s

    let to_bani_ptr a b = to_bani_ptr (fst a) b
    let of_bani_ptr w b a = of_bani_ptr w b (fst a), w
    let to_bigint s = to_bigint (fst s)
    let of_bigint w s = of_bigint w s, w
  end

  module ArraybitsNativeint_Conv = 
  struct
    include Comb.ArraybitsNativeint
    let is_mutable = true

    let to_bani_ptr a b = 
      for i=0 to Bigarray.Array1.dim b - 1 do
        b.{i} <- a.(i)
      done

    let of_bani_ptr w b t = 
      for i=0 to Array.length t - 1 do
        t.(i) <- b.{i}
      done;
      t

    let to_bigint s = Utils_ext.big_int_of_abits_nint s
    let of_bigint w s = Utils_ext.abits_nint_of_big_int w s

    let to_bani_ptr a b = to_bani_ptr (fst a) b
    let of_bani_ptr w b a = of_bani_ptr w b (fst a), w
    let to_bigint s = to_bigint (fst s)
    let of_bigint w s = of_bigint w s, w
  end

  module BigarraybitsInt32Api = 
  struct

    type elt = int32
    type barray = Utils_ext.ba32

    let nbits = 32
    let words bits = (bits + nbits - 1) / nbits 
    let word bit = bit / nbits
    let create bits = 
      let words = words bits in
      let ba = 
        Bigarray.Array1.create 
          Bigarray.int32 Bigarray.c_layout words
      in 
      for i=0 to words-1 do
        ba.{i} <- 0l
      done;
      ba

    let zero = 0l
    let one = 1l

    let mask n = if n = 0 then -1l else Int32.shift_right_logical (-1l) (nbits-n)
    let mask_bit n = Int32.shift_left 1l n

    let to_bits s = Utils_ext.babits_int32_of_bstr s
    let to_bstr w s = Utils_ext.bstr_of_babits_int32 w s
    let to_int s = Int32.to_int s.{0}
    let of_int bits i =
      let b = create bits in
      b.{0} <- Int32.of_int i;
      b

    let get a n = a.{n}
    let set a n v = a.{n} <- v

    let (+.) = Int32.add
    let (-.) = Int32.sub
    let (&.) = Int32.logand
    let (|.) = Int32.logor
    let (^.) = Int32.logxor
    let (~.) = Int32.lognot
    let (>>.) = Int32.shift_right_logical
    let (<<.) = Int32.shift_left

  end

  module BigarraybitsInt32_Bits = 
    Make(ArraybitsBuilder(BigarraybitsInt32Api))

  module BigarraybitsInt32_Conv = 
  struct
    include BigarraybitsInt32_Bits
    let is_mutable = true

    let to_bani_ptr a b = 
      if Utils.platform_bits = 32 then
        for i=0 to Bigarray.Array1.dim b - 1 do
          b.{i} <- Nativeint.of_int32 a.{i}
        done
      else
        for i=0 to Bigarray.Array1.dim b - 1 do
          let x = Nativeint.of_int32 a.{(i*2)+0} in
          let y = Nativeint.of_int32 a.{(i*2)+1} in
          b.{i} <- Nativeint.logor (Nativeint.shift_left y 32) x
        done

    let of_bani_ptr w b t = 
      let _ = 
        if Utils.platform_bits = 32 then 
          for i=0 to Bigarray.Array1.dim t - 1 do
            t.{i} <- Nativeint.to_int32 b.{i}
          done
        else
          for i=0 to Bigarray.Array1.dim t - 1 do
            let x = b.{i/2} in
            let x = 
              if i mod 2 = 0 then Nativeint.to_int32 x 
              else Nativeint.to_int32 (Nativeint.shift_right_logical x 32)
            in
            t.{i} <- x
          done
      in
      t

    let to_bigint s = Utils_ext.big_int_of_babits_int32 s
    let of_bigint w s = Utils_ext.babits_int32_of_big_int w s

    let to_bani_ptr a b = to_bani_ptr (fst a) b
    let of_bani_ptr w b a = of_bani_ptr w b (fst a), w
    let to_bigint s = to_bigint (fst s)
    let of_bigint w s = of_bigint w s, w
  end

  module BigarraybitsInt64Api = 
  struct

    type elt = int64
    type barray = Utils_ext.ba64

    let nbits = 64
    let words bits = (bits + nbits - 1) / nbits 
    let word bit = bit / nbits
    let create bits = 
      let words = words bits in
      let ba = 
        Bigarray.Array1.create 
          Bigarray.int64 Bigarray.c_layout words 
      in
      for i=0 to words-1 do
        ba.{i} <- 0L
      done;
      ba

    let zero = 0L
    let one = 1L

    let mask n = if n = 0 then -1L else Int64.shift_right_logical (-1L) (nbits-n)
    let mask_bit n = Int64.shift_left 1L n

    let to_bits s = Utils_ext.babits_int64_of_bstr s
    let to_bstr w s = Utils_ext.bstr_of_babits_int64 w s
    let to_int s = Int64.to_int s.{0}
    let of_int bits i =
      let b = create bits in
      b.{0} <- Int64.of_int i;
      b

    let get a n = a.{n}
    let set a n v = a.{n} <- v

    let (+.) = Int64.add
    let (-.) = Int64.sub
    let (&.) = Int64.logand
    let (|.) = Int64.logor
    let (^.) = Int64.logxor
    let (~.) = Int64.lognot
    let (>>.) = Int64.shift_right_logical
    let (<<.) = Int64.shift_left

  end

  module BigarraybitsInt64_Bits = 
    Make(ArraybitsBuilder(BigarraybitsInt64Api))

  module BigarraybitsInt64_Conv = 
  struct
    include BigarraybitsInt64_Bits
    let is_mutable = true

    let to_bani_ptr a b = 
      if Utils.platform_bits = 64 then
        for i=0 to Bigarray.Array1.dim b - 1 do
          b.{i} <- Int64.to_nativeint a.{i}
        done
      else
        for i=0 to Bigarray.Array1.dim a - 1 do
          let x = Int64.to_nativeint a.{i} in
          let y = Int64.to_nativeint (Int64.shift_right_logical a.{i} 32) in
          b.{(i*2)+0} <- x;
          b.{(i*2)+1} <- y
        done

    let of_bani_ptr w b t = 
      let _ = 
        if Utils.platform_bits = 64 then
          for i=0 to Bigarray.Array1.dim t - 1 do
            t.{i} <- Int64.of_nativeint b.{i}
          done
        else
          for i=0 to Bigarray.Array1.dim t - 1 do
            if i mod 2 = 0 then
              t.{i} <- Int64.of_nativeint b.{i}
            else
              t.{i} <- 
                Int64.logor
                  t.{i} 
                  (Int64.shift_left (Int64.of_nativeint b.{i}) 32)

          done
      in
      t

    let to_bigint s = Utils_ext.big_int_of_babits_int64 s
    let of_bigint w s = Utils_ext.babits_int64_of_big_int w s

    let to_bani_ptr a b = to_bani_ptr (fst a) b
    let of_bani_ptr w b a = of_bani_ptr w b (fst a), w
    let to_bigint s = to_bigint (fst s)
    let of_bigint w s = of_bigint w s, w
  end

  module BigarraybitsNativeintApi = 
  struct

    type elt = nativeint
    type barray = Utils_ext.bani

    let nbits = Utils.platform_bits
    let words bits = (bits + nbits - 1) / nbits 
    let word bit = bit / nbits
    let create bits = 
      let words = words bits in
      let ba = 
        Bigarray.Array1.create 
          Bigarray.nativeint Bigarray.c_layout words in
      for i=0 to words-1 do
        ba.{i} <- 0n
      done;
      ba

    let zero = 0n
    let one = 1n

    let mask n = if n = 0 then -1n else Nativeint.shift_right_logical (-1n) (nbits-n)
    let mask_bit n = Nativeint.shift_left 1n n

    let to_bits s = Utils_ext.babits_nint_of_bstr s
    let to_bstr w s = Utils_ext.bstr_of_babits_nint w s
    let to_int s = Nativeint.to_int s.{0}
    let of_int bits i =
      let b = create bits in
      b.{0} <- Nativeint.of_int i;
      b

    let get a n = a.{n}
    let set a n v = a.{n} <- v

    let (+.) = Nativeint.add
    let (-.) = Nativeint.sub
    let (&.) = Nativeint.logand
    let (|.) = Nativeint.logor
    let (^.) = Nativeint.logxor
    let (~.) = Nativeint.lognot
    let (>>.) = Nativeint.shift_right_logical
    let (<<.) = Nativeint.shift_left

  end

  module BigarraybitsNativeint_Bits = 
    Make(ArraybitsBuilder(BigarraybitsNativeintApi))

  module BigarraybitsNativeint_Conv = 
  struct
    include BigarraybitsNativeint_Bits
    let is_mutable = true

    let to_bani_ptr a b = 
      for i=0 to Bigarray.Array1.dim b - 1 do
        b.{i} <- a.{i}
      done

    let of_bani_ptr w b t = 
      for i=0 to Bigarray.Array1.dim t - 1 do
        t.{i} <- b.{i}
      done;
      t

    let to_bigint s = Utils_ext.big_int_of_babits_nint s
    let of_bigint w s = Utils_ext.babits_nint_of_big_int w s

    let to_bani_ptr a b = to_bani_ptr (fst a) b
    let of_bani_ptr w b a = of_bani_ptr w b (fst a), w
    let to_bigint s = to_bigint (fst s)
    let of_bigint w s = of_bigint w s, w
  end

  module Comb = 
  struct

    module type T = 
    sig
      include S
      val is_mutable : bool
      val to_bani_ptr : t -> Utils_ext.bani -> unit
      val of_bani_ptr : int -> Utils_ext.bani -> t -> t
      val to_bigint : t -> Big_int.big_int
      val of_bigint : int -> Big_int.big_int -> t
    end

    module type S = 
    sig
      include S
      val is_mutable : bool
      val to_bani_ptr : t -> Utils_ext.bani -> unit
      val of_bani_ptr : int -> Utils_ext.bani -> t -> t
      val to_bani : t -> Utils_ext.bani
      val of_bani : int -> Utils_ext.bani -> t
      val to_bigint : t -> Big_int.big_int
      val of_bigint : int -> Big_int.big_int -> t
    end

    module MakeC(Conv : T) =
    struct

      include Conv
      open Utils

      let to_bani s = 
        let width = width s in
        let words = (width + platform_bits - 1) / platform_bits in
        let b = Bigarray.Array1.create Bigarray.nativeint
            Bigarray.c_layout words in
        Conv.to_bani_ptr s b;
        b

      let of_bani width b = 
        let t = if is_mutable then zero width else empty in
        Conv.of_bani_ptr width b t

    end

    module IntbitsList = MakeC(IntbitsList_Conv)

    module Intbits = MakeC(Intbits_Conv)
    module Int32bits = MakeC(Int32bits_Conv)
    module Int64bits = MakeC(Int64bits_Conv)
    module Nativeintbits = MakeC(Nativeintbits_Conv)

    module ArraybitsInt32 = MakeC(ArraybitsInt32_Conv)
    module ArraybitsInt64 = MakeC(ArraybitsInt64_Conv)
    module ArraybitsNativeint = MakeC(ArraybitsNativeint_Conv)

    module BigarraybitsInt32 = MakeC(BigarraybitsInt32_Conv)
    module BigarraybitsInt64 = MakeC(BigarraybitsInt64_Conv)
    module BigarraybitsNativeint = MakeC(BigarraybitsNativeint_Conv)
  end

end

module Raw = struct

  module Make(B : ArraybitsBase) = 
  struct

      open B

      type t = 
          {
              data : B.barray;
              width : int;
          }
      let empty = 
          {
              data = create 0;
              width = 0;
          }
      let width s = s.width

      let to_string x = to_bstr (width x) x.data
      let to_int x = to_int x.data
      let to_bstr x = to_bstr (width x) x.data

      let mask s = 
          let width = s.width in
          let top_word = word (width-1) in
          let mask = mask (width mod B.nbits) in 
          set s.data top_word ((get s.data top_word) &. mask)

      let copy t f = 
          assert (width t = width f);
          for i=0 to (words t.width) - 1 do
              set t.data i (get f.data i)
          done

      let const b = 
          {
              data = to_bits b;
              width = String.length b
          }

      let wire _ = empty
      let (--) a b = a

      let vdd = const "1"
      let gnd = const "0"

      let op2 op c a b = 
          let width = width a in
          let words = words width in
          for i=0 to words - 1 do
              set c.data i (op (get a.data i) (get b.data i))
          done;
          mask c

      let (&:) = op2 (&.)
      let (|:) = op2 (|.) 
      let (^:) = op2 (^.)

      let (~:) c a = 
          let width = width a in
          let words = words width in
          let a = a.data in
          for i=0 to words - 1 do
              set c.data i (~. (get a i))
          done;
          mask c

      let bo2 = nbits/2
      let mask_lo = B.mask bo2
      let mask_hi = (B.mask bo2) <<. bo2
      let half_lo x = x &. mask_lo
      let half_hi x = (x &. mask_hi) >>. bo2
      let cbit = B.mask_bit bo2

      let (+:) c a b = 
          let carry = ref zero in
          let set_carry x = 
              if zero <> (x &. cbit) then carry := one
              else carry := zero
          in
          op2 (fun a b ->
              let a' = half_lo a in
              let b' = half_lo b in
              let c_lo = a' +. b' +. !carry in
              set_carry c_lo;
              let a' = half_hi a in
              let b' = half_hi b in
              let c_hi = a' +. b' +. !carry in
              set_carry c_hi;
              (c_hi <<. bo2) |. (half_lo c_lo)
          ) c a b

      let (-:) c a b = 
          let borrow = ref zero in
          let set_borrow x = 
              if zero <> (x &. cbit) then borrow := one
              else borrow := zero
          in
          op2 (fun a b ->
              let a' = half_lo a in
              let b' = half_lo b in
              let c_lo = a' -. b' -. !borrow in
              set_borrow c_lo;
              let a' = half_hi a in
              let b' = half_hi b in
              let c_hi = a' -. b' -. !borrow in
              set_borrow c_hi;
              (c_hi <<. bo2) |. (half_lo c_lo)
          ) c a b

      let iterback op a b =
          let words = words (width a) in
          let a = a.data in
          let b = b.data in
          for i=words-1 downto 0 do
              op (get a i) (get b i)
          done

      let (==:) c a b = 
          let eq = ref true in
          iterback (fun a b -> if a <> b then eq := false) a b;
          if !eq then copy c vdd
          else copy c gnd

      let (<:) c a b =
          let cmp = ref 0 in
          let set_cmp a b =
              if !cmp = 0 then
                  (if a > b then cmp := 1 
                   else if a < b then cmp := -1)
          in
          iterback (fun a b -> 
              let a' = half_hi a in
              let b' = half_hi b in
              set_cmp a' b';
              let a' = half_lo a in
              let b' = half_lo b in
              set_cmp a' b';
          ) a b;
          if !cmp = -1 then copy c vdd
          else copy c gnd

      let (<==) a b = ()

      let mux c sel l = 
          let len = List.length l in
          let idx = to_int sel in
          let idx = if idx >= len then len-1 else idx in
          copy c (List.nth l idx)

      let concat c l = 
          let c_width = List.fold_left (fun a b -> a + width b) 0 l in
          assert (c_width = width c);
          let c_words = words c_width in
          let cat a b = 
              let a_width, a_words = width a, words (width a) in
              let b_width, b_words = width b, words (width b) in
              let a_bits = a_width mod nbits in
              let a,b = a.data, b.data in
              let _ = 
                  if a_bits = 0 then
                      (* aligned *)
                      for i=0 to b_words - 1 do
                          set a (a_words + i) (get b i)
                      done
                  else
                      (* not aligned *)
                      let merge x y = x |. (y <<. a_bits), y >>. (nbits - a_bits) in 
                      for i=0 to b_words -1 do
                          let x,y = merge (get a (a_words-1+i)) (get b i) in
                          set a (a_words-1+i) x;
                          if a_words+i < c_words then
                              set a (a_words+i) y
                      done
              in
              { c with width = (a_width + b_width) }
          in
          let l = List.rev l in
          if l = [] then ()
          else
              let c = cat { c with width = 0 } (List.hd l) in
              ignore (List.fold_left (fun c a -> cat c a) c (List.tl l))

      let select c s h l = 
          let c_width = h-l+1 in
          assert (c_width = width c);
          let c_words = words c_width in
          let s_bits = l mod nbits in
          let lo_word = word l in
          let hi_word = word h in
          let s = s.data in
          let merge i = 
              let a = get s i in
              let b = if i >= hi_word then zero else get s (i+1) in
              (a >>. s_bits) |. (b <<. (nbits-s_bits))
          in
          let _ = 
              if s_bits = 0 then
                  for i=0 to c_words-1 do
                      set c.data i (get s (lo_word + i))
                  done
              else
                  for i=0 to c_words-1 do
                      set c.data i (merge (lo_word+i))
                  done
          in
          mask c

    (* cheat *)
    module M = ArraybitsBuilder(B)

    let ( *: ) c a b = 
      let d,w = M.((a.data,a.width) *: (b.data,b.width)) in
      copy c { data=d; width=w }

    let ( *+ ) c a b = 
      let d,w = M.((a.data,a.width) *+ (b.data,b.width)) in
      copy c { data=d; width=w }

  end

end

