(* 
 * hardcaml - hardware design in OCaml
 *
 *   (c) 2014 MicroJamJar Ltd
 *
 * Author(s): andy.ray@ujamjar.com
 * Description: 
 *
 *)

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
    module IntbitsList = Comb.Make(IntbitsListBase)
    module Intbits = Comb.Make(IntbitsBase)
    module Int32bits = Comb.Make(Int32bitsBase)
    module Int64bits = Comb.Make(Int64bitsBase)
    module Nativeintbits = Comb.Make(NativeintbitsBase)
    module ArraybitsInt32 = Comb.Make(ArraybitsInt32Base)
    module ArraybitsInt64 = Comb.Make(ArraybitsInt64Base)
    module ArraybitsNativeint = Comb.Make(ArraybitsNativeintBase)
end

