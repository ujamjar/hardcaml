(* 
 * hardcaml - hardware design in OCaml
 *
 *   (c) 2014 MicroJamJar Ltd
 *
 * Author(s): andy.ray@ujamjar.com
 * Description: 
 *
 *)

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

module type T = 
sig
    include Comb.S
    val is_mutable : bool
    val to_bani_ptr : t -> Utils_ext.bani -> unit
    val of_bani_ptr : int -> Utils_ext.bani -> t -> t
    val to_bigint : t -> Big_int.big_int
    val of_bigint : int -> Big_int.big_int -> t
end

module type S = 
sig
    include Comb.S
    val is_mutable : bool
    val to_bani_ptr : t -> Utils_ext.bani -> unit
    val of_bani_ptr : int -> Utils_ext.bani -> t -> t
    val to_bani : t -> Utils_ext.bani
    val of_bani : int -> Utils_ext.bani -> t
    val to_bigint : t -> Big_int.big_int
    val of_bigint : int -> Big_int.big_int -> t
end

module Make(Conv : T) =
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

module IntbitsList_Conv = 
struct
    include Bits.Comb.IntbitsList
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
    include Bits.Comb.Intbits
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
    include Bits.Comb.Int32bits
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
    include Bits.Comb.Int64bits
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
    include Bits.Comb.Nativeintbits
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
    include Bits.Comb.ArraybitsInt32
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
    include Bits.Comb.ArraybitsInt64
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
    include Bits.Comb.ArraybitsNativeint
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
    Comb.Make(Bits.ArraybitsBuilder(BigarraybitsInt32Api))

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
    Comb.Make(Bits.ArraybitsBuilder(BigarraybitsInt64Api))

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
    Comb.Make(Bits.ArraybitsBuilder(BigarraybitsNativeintApi))

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
    module IntbitsList = Make(IntbitsList_Conv)

    module Intbits = Make(Intbits_Conv)
    module Int32bits = Make(Int32bits_Conv)
    module Int64bits = Make(Int64bits_Conv)
    module Nativeintbits = Make(Nativeintbits_Conv)

    module ArraybitsInt32 = Make(ArraybitsInt32_Conv)
    module ArraybitsInt64 = Make(ArraybitsInt64_Conv)
    module ArraybitsNativeint = Make(ArraybitsNativeint_Conv)

    module BigarraybitsInt32 = Make(BigarraybitsInt32_Conv)
    module BigarraybitsInt64 = Make(BigarraybitsInt64_Conv)
    module BigarraybitsNativeint = Make(BigarraybitsNativeint_Conv)
end

