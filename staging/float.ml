(* 
 * hardcaml - hardware design in OCaml
 *
 *   (c) 2014 MicroJamJar Ltd
 *
 * Author(s): andy.ray@ujamjar.com
 * Description: 
 *
 *)

module type FloatSpec = sig
    val exponent_bits : int
    val mantissa_bits : int
end

module Float32 : FloatSpec = struct
    let exponent_bits = 8
    let mantissa_bits = 23
end        

module Float64 : FloatSpec = struct
    let exponent_bits = 11
    let mantissa_bits = 52
end

module type Float = sig
    (* base signal type *)
    type bt
    (* floating point number. *)
    type t
    val negate : t -> t
    val (+:) : t -> t -> t
    val (-:) : t -> t -> t
    val ( *: ) : t -> t -> t
    val (==:) : t -> t -> bt
    val (<>:) : t -> t -> bt
    val (<:) : t -> t -> bt
    val (>:) : t -> t -> bt
    val (<=:) : t -> t -> bt
    val (>=:) : t -> t -> bt
end

module type FloatBase = sig

    include Float

    val sign : t -> bt
    val exponent : t -> bt
    val mantissa : t -> bt

    type ordered_float =
        {
            a_lt_b : bt;
            exp_diff : bt;
            min : t;
            max : t;
            renorm : bt;
            state : string;
        }

    val fmin : ordered_float -> t
    val fmax : ordered_float -> t

    module Const : sig
        val max_exp : int
        val bias : int
        val max_adj_exp : int
        val min_adj_exp : int
        val bits : int
        val zero : t
        val min_denormal : t
        val max_denormal : t
    end

    val to_float : t -> float
    val of_float : float -> t
    val of_signal : bt -> t
    val to_signal : t -> bt
    val to_string : t -> string
    val pretty_printer : Format.formatter -> t -> unit
    val pretty_printer_ord : Format.formatter -> ordered_float -> unit

    val fmux2 : bt -> t -> t -> t
    val order : (t * t) -> ordered_float
    val align : ordered_float -> ordered_float
    val add : ordered_float -> ordered_float
    val leading_ones : bt -> bt
    val leading_zeros : bt -> bt
    val normalize : ordered_float -> ordered_float
    val round_mantissa : ordered_float -> ordered_float
    val clip_exponent : ordered_float ->ordered_float
    val check_nan : t -> t

    val fadd : t -> t -> t
    val test_fadd : t -> t -> t * t * 
        ordered_float * ordered_float * ordered_float *
        ordered_float * ordered_float * ordered_float * 
        t * t

    val mul_mantissa : t -> t -> bt * bt
    val mul_exponent : t -> t -> bt -> bt
end

(* Basic floating point numbers.  No special logic makes sure that
 * zeros, denormals, nans or infs work.
 *
 * WRT to zeros we may be able to rely on a property that forces the
 *     mantissa to zero for us.  Will need to carefully check though
 *     and it may only work for the adder.
 *
 * We should document the bit growth through the algorithms to show they
 * are (1) safe and (2) exactly what boundaries are chopped and where
 * during overflow and rounding.
 *
 * The tricky issue;
 *
 * We perform various types of maths on the exponent and mantissa.  The next
 * step is to normalize then round which affects both exp and mant.  The
 * main problem is we need to normalise, then round, then potentially normalise 
 * again.  We need to generate some test cases to show exactly what we need to
 * do and how we can optimise it.  
 *
 * This problem would seem to happen when the mantissa is basically all ones.
 * Otherwise the rounding bit wont propogate all the way to the top and it wont
 * change the top bit.
 *
 *
 * *)
module Make(B : Comb.S)(S : FloatSpec)(R : Fixed.Round(B).Unsigned) = struct

    open B
    open Utils
    
    module O = Fixed.Overflow(B)

    type bt = B.t

    (* floating point number. *)
    type t = 
        {
            (* sign *)
            sign : B.t;
            (* exponent *)
            exponent : B.t;
            (* without leading 1 *)
            mantissa : B.t;
            (* is zero - assumes mantissa and exponent are also 0 *)
            zero : B.t;
            (* XXX *)
            guard_bits : int;
        }

    let sign f = f.sign
    let exponent f = f.exponent
    let mantissa f = f.mantissa

    type ordered_float = 
        {
            a_lt_b : B.t;
            exp_diff : B.t;
            min : t;
            max : t;
            renorm : B.t;
            state : string;
        }

    let fmin f= f.min
    let fmax f = f.max

    module Const = struct
        let max_exp = (1 lsl S.exponent_bits) - 1
        let bias = (1 lsl (S.exponent_bits-1)) - 1
        let max_adj_exp = max_exp - bias
        let min_adj_exp = 0 - bias
        let bits = S.exponent_bits + S.mantissa_bits + 1
        let zero = 
            {
                sign = B.gnd;
                exponent = zero S.exponent_bits;
                mantissa = zero S.mantissa_bits;
                zero = B.vdd;
                guard_bits = 0;
            }
        let min_denormal = 
            {
                sign = B.gnd;
                exponent = B.zero S.exponent_bits;
                mantissa = one S.mantissa_bits;
                zero = B.vdd; (* it sort of is *)
                guard_bits = 0;
            }
        let max_denormal = 
            {
                sign = B.gnd;
                exponent = B.zero S.exponent_bits;
                mantissa = ones S.mantissa_bits;
                zero = B.vdd; (* it sort of is *)
                guard_bits = 0;
            }
    end

    (* convert to a 64 bit double *)
    let to_float f = 
        let selm n x = 
            if width x = n then x
            else if width x < n then x @: zero (n - width x)
            else select x (width x - 1) (width x - n)
        in
        let s = Int64.of_int (B.to_int f.sign) in
        let e = Int64.of_int (B.to_int f.exponent - Const.bias) in
        let e = Int64.(add e 1023L) in (* add double bias *) 
        let e = if e > 2047L then 2047L else e in (* clip to max exponent *)
        let m = B.to_int64 (selm 52 f.mantissa) in
        Int64.float_of_bits Int64.(
            logor
                (shift_left s 63) 
                (logor (shift_left e 52)
                        m)
        )

    let of_float f' = 
        let f = Int64.bits_of_float f' in
        let s = Int64.(to_int (logand (shift_right f 63) 1L)) in
        (* assumed to be in range of the target float format *)
        let e = Int64.(to_int (logand (shift_right f 52) 2047L) - 1023) in
        let e = min Const.max_adj_exp (max Const.min_adj_exp e) in
        let e = e + Const.bias in
        let m = Int64.(logand f (sub (shift_left 1L 52) 1L)) in
        let m = B.consti64 52 m in
        let m = 
            if S.mantissa_bits <= 52 then 
                select m 51 (52-S.mantissa_bits)
            else
                m @: zero (S.mantissa_bits - 52)
        in
        {
            sign = B.consti 1 s;
            exponent = B.consti S.exponent_bits e;
            mantissa = m;
            zero = if f' = 0. then B.vdd else B.gnd;
            guard_bits = 0;
        }

    let of_signal s = 
        let eo = S.mantissa_bits in
        let so = eo + S.exponent_bits in
        let sign = select s so so in
        let exponent = select s (eo+S.exponent_bits-1) eo in
        let mantissa = select s (S.mantissa_bits-1) 0 in
        {
            sign = sign;
            exponent = exponent;
            mantissa = mantissa;
            zero = exponent ==:. 0;
            guard_bits = 0;
        }

    let to_signal f = f.sign @: f.exponent @: f.mantissa

    let to_fixed_signal f = 
        let m = vdd @: f.mantissa in
        let m = uresize m ((1 lsl S.exponent_bits)+S.mantissa_bits+1) in
        log_shift sll m f.exponent

    let to_string f = 
        Printf.sprintf "|%d|%s|%s|"
            (B.to_int f.sign)
            (B.to_string f.exponent)
            (B.to_string f.mantissa)

    let pretty_printer ff f = 
        let f' = to_float f in
        Format.fprintf ff "@[%d %*s |%2s|%s|%3s| %+.5e %+.5f.2^%i %s@]" 
            (B.to_int f.sign)
            (S.exponent_bits + 2) (B.to_string f.exponent) 
            (B.to_string (select_e f.mantissa (width f.mantissa-1) (f.guard_bits+S.mantissa_bits))) 
            (B.to_string (select_e f.mantissa (f.guard_bits+S.mantissa_bits-1) (f.guard_bits)))
            (B.to_string (select_e f.mantissa (f.guard_bits-1) (0)))
            f' (fst (frexp f')) (snd (frexp f'))
            (if B.to_int f.zero = 1 then "z" else "")

    let pretty_printer_ord ff f = 
        Format.fprintf ff "@[a<b:%i diff:%i min:%i max:%i renorm:%i state:%s" 
            (B.to_int f.a_lt_b) (B.to_int f.exp_diff)
            (B.to_int f.min.exponent - Const.bias) 
            (B.to_int f.max.exponent - Const.bias)
            (B.to_int f.renorm)
            f.state;
        Format.fprintf ff "@;";
        pretty_printer ff f.min;
        Format.fprintf ff "@;";
        pretty_printer ff f.max;
        Format.fprintf ff "@]"

    let fmux2 s a b = 
        {
            sign = mux2 s a.sign b.sign;
            exponent = mux2 s a.exponent b.exponent;
            mantissa = mux2 s a.mantissa b.mantissa;
            zero = mux2 s a.zero b.zero;
            guard_bits = 0;
        }

    (* get the (absolute) min and max of a and b.
     * calculate the difference in exponents (always positive,
     * no bit growth).
     * "special case where 2**exp_bits < mantissa_bits"???  *)
    let order (a,b) = 
        let a_lt_b = 
            (a.exponent @: a.mantissa) <: (b.exponent @: b.mantissa) 
        in
        let a,b = fmux2 a_lt_b a b, fmux2 a_lt_b b a in
        let exp_diff = 
            let n = nbits (S.mantissa_bits+3) in
            let diff = b.exponent -: a.exponent in
            if n < width a.exponent then 
                (* this deals with a special case where the exponent isnt
                 * big enough to actually shift all the mantissa bits,
                 * which I am not sure makes much sense anyway *)
                O.Unsigned.Saturate.f 0 n diff
            else diff 
        in
        {
            a_lt_b = a_lt_b;
            exp_diff = exp_diff;
            min = a;
            max = b;
            renorm = B.empty;
            state = "ordered";
        }

    (* align the mantissas, add hidden 1, generate guard bits 
     * 3 guard bits will be added at the bottom. 
     * the hidden bit will be added at the top. 
     * hidden bit controlled by zero flag *)
    let align f = 
        let sft = log_shift srl 
            ((B.(~:) f.min.zero) @: f.min.mantissa @: zero 2) f.exp_diff 
        in
        let mask = 
            let m = S.mantissa_bits in
            let mask n = 
                if n <= 2 then zero m 
                else
                    let n = n - 2 in
                    concat_e [ zero (m - n); ones n ]
            in
            mux_init f.exp_diff (m+3) mask
        in
        let guard = reduce (|:) (bits (mask &: f.min.mantissa)) in
        { f with 
            min = { f.min with mantissa = sft @: guard; guard_bits=3; };
            max = { f.max with 
                        mantissa = (B.(~:) f.max.zero) @: f.max.mantissa @: zero 3; 
                        guard_bits=3 };
            state = "aligned";
        }

    (* add or subtract the mantissas.  
     * adds one extra bit of precision at the top *)
    let add f = 
        let negative = f.min.sign ^: f.max.sign in
        let a',b' = (ue f.min.mantissa), (ue f.max.mantissa) in
        let a' = mux2 negative (~: a') (a') in
        let m = a' +: b' +: uresize negative (width a') in 
        { f with max = { f.max with mantissa = m; };
                 state = "added"; }

    let split_pow2 v = 
        let w = width v in
        if w = 0 then empty,empty
        else if w = 1 then v,empty
        else
            let l = 1 lsl ((clog2 w) - 1) in
            select v (w-1) (w-l), select v (w-l-1) 0

    (* count number of leading ones.
     * this uses a tree of adders rather than basic gates
     * which the old version used.  Check smaller implementation.
     * Could it also be done with parallel prefix trees? *)
    let rec leading_ones v = 
        let x, y = split_pow2 v in
        if x=empty && y=empty then failwith "leading_ones empty"
        else if y=empty then x
        else
            let x = leading_ones x in
            let y = leading_ones y in
            if width x = width y then
                let sum = (ue x) +: (ue y) in
                mux2 (msb x) sum (ue x)
            else
                let y = uresize y (width x) in
                let sum = x +: y in
                mux2 (msb x) sum x

    (* count number of leading zeros *)
    let leading_zeros v = leading_ones (~: v)

    (* normalize mantissa and update the exponent
     * drops top bits of mantissa
     * grows exponent by 1 bit. *)
    let normalize f = 
        let lz = leading_zeros f.max.mantissa in
        let mz = lz ==:. S.mantissa_bits + 5 in
        let m = log_shift sll f.max.mantissa lz in
        let m = select m (width m - 2) 2 @: (bit m 0 |: bit m 1) in
        let e = (ue f.max.exponent) -: (uresize lz (S.exponent_bits+1) -:. 1) in
        let e = mux2 mz (zero (S.exponent_bits+1)) e in (* zero mantissa *)
        { f with max = {f.max with mantissa = m;
                                   exponent = e; 
                                   zero = mz; } ;
                 renorm = lz;
                 state = "normalized";
        }

    (* round the mantissa.
     * seems to assume no posibility of causing overflow 
     * on mantissa during rounding (because of shift?)
     * *)
    let round_mantissa f = 
        let m = R.f 3 f.max.mantissa in
        let m = select m (S.mantissa_bits-1) 0 in
        { f with max = { f.max with mantissa = m; guard_bits=0; };
                 state = "mantissa-rounded"; }

    (* clip exponent (top and bottom) *)
    let clip_exponent f = 
        { f with max = { f.max with exponent = 
                                        O.Unsigned.Saturate.f 0 
                                            S.exponent_bits f.max.exponent };
                 state = "exponent-clipped";

        }

    (* debug; try to make nan's look IEEE x86 style - to be replaced with
     * proper implementation and removed from here *)
    let check_nan f = 
        { f with mantissa = mux2 (f.exponent ==: (ones S.exponent_bits))
                                ((reverse (one S.mantissa_bits)) |: f.mantissa)
                                f.mantissa; }

    let fadd a b = 
        (a,b) |> order
              |> align
              |> add
              |> normalize
              |> round_mantissa
              |> clip_exponent
              |> fmax
              |> check_nan (* XXX *)

    let test_fadd a b = 
        let a0 = order (a,b) in
        let a1 = align a0 in
        let a2 = add a1 in
        let a3 = normalize a2 in
        let a4 = round_mantissa a3 in
        let a5 = clip_exponent a4 in
        let a6 = fmax a5 in
        let a7 = check_nan a6 in
        a,b,a0,a1,a2,a3,a4,a5,a6,a7

    let negate a = { a with sign = ~: (a.sign); }

    let fsub a b = fadd a (negate b)

    let mul_mantissa a b = 
        let m = (vdd @: a.mantissa) *: (vdd @: b.mantissa) in
        let msb = msb m in
        let m = mux2 msb 
            (select m (width m - 2) (width m - 1 - S.mantissa_bits))
            (select m (width m - 3) (width m - 2 - S.mantissa_bits))
        in
        msb, m

    let mul_exponent a b norm = 
        let norm = uresize norm (S.exponent_bits+1) -:. Const.bias in
        let e = (ue a.exponent) +: (ue b.exponent) +: norm in
        let e = O.Unsigned.Saturate.f 0 S.exponent_bits e in
        e

    let fmul a b = 
        let norm, m = mul_mantissa a b in
        let e = mul_exponent a b norm in
        {
            sign = a.sign ^: b.sign;
            exponent = e;
            mantissa = m;
            zero = B.gnd;
            guard_bits = 0;
        }

    (* comparisons 
     * compare exponent and mantissa in the general case.  Special cases
     * are when exponent == 0 or exponent == 2^N-1
     * for exponent == 0 I dont think there's much to do even with denormals.
     * for the max exponent, then the mantissa no longer important and could
     * contain random bits.
     *)

    let eq a b = to_signal a ==: to_signal b 

    let lt a b = 
        let lt = (a.exponent @: a.mantissa) <: (b.exponent @: b.mantissa) in
        let eq = (a.exponent @: a.mantissa) ==: (b.exponent @: b.mantissa) in
        let gt = ~: (lt |: eq) in
        mux (a.sign @: b.sign) [ lt; a.sign; a.sign; gt ]
    
    (* operators *)

    let (+:) = fadd
    let (-:) = fsub
    let ( *: ) = fmul
    let (==:) = eq
    let (<>:) a b = ~: (a ==: b)
    let (<:) = lt
    let (>:) a b = b <: a
    let (<=:) a b = ~: (a >: b)
    let (>=:) a b = ~: (a <: b)

end

module Tagged(B : Comb.S)(F : FloatBase) = struct

    type bt = B.t
    type t = 
        {
            tag : B.t;
            f : F.t;
        }

    open B

    let fadd a b = 
        let open Utils in
        (*let is_zero = memoize (fun t -> t ==:. 0) in
        let is_norm = memoize (fun t -> t ==:. 1) in
        let is_nan = memoize (fun t -> t ==:. 2) in
        let is_inf = memoize (fun t -> t ==:. 3) in*)
        let c = 
          (a.f,b.f) |> F.order
                    |> F.align
                    |> F.add
                    |> F.normalize
                    |> F.round_mantissa
                    |> F.clip_exponent
                    |> F.fmax
                    (*|> F.check_nan (* XXX *)*)
        in
        {
            tag = a.tag;
            f = c;
        }

    let negate a = a
    let (+:) a b = fadd a b
    let (-:) a b = a
    let ( *: ) a b = a
    let (<:) a b = B.vdd
    let (<=:) a b = B.vdd
    let (>:) a b = B.vdd
    let (>=:) a b = B.vdd

    let (<>:) a b = B.vdd
    let (==:) a b = B.vdd
end


