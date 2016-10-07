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

module type T = 
sig
    type t
    val empty : t
    val width : t -> int
    val const : string -> t
    val concat : t list -> t
    val mux : t -> t list -> t
    val select : t -> int -> int -> t
    val wire : int -> t
    
    val (--) : t -> string -> t
    val (&:) : t -> t -> t
    val (|:) : t -> t -> t
    val (^:) : t -> t -> t
    val (~:) : t -> t
    val (+:) : t -> t -> t
    val (-:) : t -> t -> t
    val ( *: ) : t -> t -> t
    val ( *+ ) : t -> t -> t
    val (==:) : t -> t -> t
    val (<:) : t -> t -> t
    val (<==) : t -> t -> unit
    val to_string : t -> string
    val to_int : t -> int
    val to_bstr : t -> string
end

module type S =
sig
   
    type t 
    val empty : t
    val ( -- ) : t -> string -> t
    val width : t -> int
    val constb : string -> t
    val consti : int -> int -> t
    val consti32 : int -> int32 -> t
    val consti64 : int -> int64 -> t
    val consthu : int -> string -> t
    val consths : int -> string -> t
    val constd : int -> string -> t
    val constv : string -> t
    val constibl : int list -> t
    val const : string -> t
    val concat : t list -> t
    val concat_e : t list -> t
    val ( @: ) : t -> t -> t
    val vdd : t
    val gnd : t
    val zero : int -> t
    val ones : int -> t
    val one : int -> t
    val select : t -> int -> int -> t
    val select_e : t -> int -> int -> t
    val bit : t -> int -> t
    val msb : t -> t
    val lsbs : t -> t
    val lsb : t -> t
    val msbs : t -> t
    val drop_bottom : t -> int -> t
    val drop_top : t -> int -> t
    val sel_bottom : t -> int -> t
    val sel_top : t -> int -> t
    val insert : t:t -> f:t -> int -> t
    val sel : t -> (int * int) -> t
    val mux : t -> t list -> t
    val mux2 : t -> t -> t -> t
    val mux_init : t -> int -> (int -> t) -> t
    val cases : t -> t -> (int * t) list -> t
    val pmux : (t * t) list -> t -> t
    val pmuxl : (t * t) list -> t
    val pmux1h : (t * t) list -> t
    val ( &: ) : t -> t -> t
    val ( &:. ) : t -> int -> t
    val (&&:) : t -> t -> t
    val ( |: ) : t -> t -> t
    val ( |:. ) : t -> int -> t
    val (||:) : t -> t -> t
    val ( ^: ) : t -> t -> t
    val ( ^:. ) : t -> int -> t
    val ( ~: ) : t -> t
    val ( +: ) : t -> t -> t
    val ( +:. ) : t -> int -> t
    val ( -: ) : t -> t -> t
    val ( -:. ) : t -> int -> t
    val negate : t -> t
    val ( *: ) : t -> t -> t
    val ( *+ ) : t -> t -> t
    val ( ==: ) : t -> t -> t
    val (==:.) : t -> int -> t
    val ( <>: ) : t -> t -> t
    val (<>:.) : t -> int -> t
    val ( <: ) : t -> t -> t
    val (<:.) : t -> int -> t
    val lt : t -> t -> t
    val ( >: ) : t -> t -> t
    val (>:.) : t -> int -> t
    val ( <=: ) : t -> t -> t
    val (<=:.) : t -> int -> t
    val ( >=: ) : t -> t -> t
    val (>=:.) : t -> int -> t
    val ( <+ ) : t -> t -> t
    val (<+.) : t -> int -> t
    val ( >+ ) : t -> t -> t
    val (>+.) : t -> int -> t
    val ( <=+ ) : t -> t -> t
    val (<=+.) : t -> int -> t
    val ( >=+ ) : t -> t -> t
    val (>=+.) : t -> int -> t
    val to_string : t -> string
    val to_int : t -> int
    val to_sint : t -> int
    val to_int32 : t -> int32
    val to_sint32 : t -> int32
    val to_int64 : t -> int64
    val to_sint64 : t -> int64
    val to_bstr : t -> string
    val bits : t -> t list
    val to_array : t -> t array
    val of_array : t array -> t
    val wire : int -> t
    val wireof : t -> t
    val ( <== ) : t -> t -> unit
    val assign : t -> t -> unit
    val input : string -> int -> t
    val output : string -> t -> t
    val clock : t
    val reset : t
    val clear : t
    val enable : t
    val repeat : t -> int -> t
    val split : t -> t * t
    val sll : t -> int -> t
    val srl : t -> int -> t
    val sra : t -> int -> t
    val log_shift : (t -> int -> t) -> t -> t -> t
    val uresize : t -> int -> t
    val sresize : t -> int -> t
    val ue : t -> t
    val se : t -> t
    val reduce : ('a -> 'a -> 'a) -> 'a list -> 'a
    val reverse : t -> t 
    val mod_counter : int -> t -> t
    val tree : int -> ('a list -> 'a) -> 'a list -> 'a
    val binary_to_onehot : t -> t
    val onehot_to_binary : t -> t
    val binary_to_gray : t -> t
    val gray_to_binary : t -> t
    val srand : int -> t

    module type TypedMath = sig
        type v
        val of_signal : t -> v
        val to_signal : v -> t
        val (+:) : v -> v -> v
        val (-:) : v -> v -> v
        val ( *: ) : v -> v -> v
        val (<:) : v -> v -> v
        val (>:) : v -> v -> v
        val (<=:) : v -> v -> v
        val (>=:) : v -> v -> v
        val (==:) : v -> v -> v
        val (<>:) : v -> v -> v
        val resize : v -> int -> v
    end

    module Unsigned : TypedMath 
    module Signed : TypedMath
    module Uop : TypedMath with type v := t
    module Sop : TypedMath with type v := t

end

exception Failure of string
let failwith str = raise (Failure str)

module Make = functor (Bits:T) ->
struct

    open Utils

    type t = Bits.t

    let empty = Bits.empty
    
    let (--) a b = Bits.(--) a b

    let width = Bits.width 
    
    (* constant generation *) 
    let constb b = 
        String.iter (function '0' | '1' -> () | _ -> failwith ("invalid binary constant " ^ b)) b;
        Bits.const b 
    let consti l v = constb (bstr_of_int l v) 
    let consti32 l v = constb (bstr_of_int32 l v) 
    let consti64 l v = constb (bstr_of_int64 l v) 
    let consthu l v = constb (bstr_of_hstr Unsigned l v) 
    let consths l v = constb (bstr_of_hstr Signed l v) 
    let constibl b = constb (Utils.bstr_of_intbitslist b)

    let concat s = 
        begin
        List.iter (fun x -> if x = empty then failwith "Can't concat empty signal") s;
        if s = [] then failwith "cant concat empty list";
        end;
        Bits.concat s 
    let (@:) a b = concat [a;b]
    let concat_e s = Bits.concat (List.filter ((<>) Bits.empty) s)
    
    let vdd = constb "1" -- "vdd"
    let gnd = constb "0" -- "gnd"
    let zero w = if w = 0 then empty else constb (String.v ~len:w (fun _ -> '0'))
    let ones w = if w = 0 then empty else constb (String.v ~len:w (fun _ -> '1'))
    let one w = 
        match w with
        | 0 -> empty
        | 1 -> vdd
        | _ -> (zero (w-1)) @: vdd

    let select a hi lo =
        if hi >= width a || lo >= width a || hi < 0 || lo < 0 || hi < lo then
            failwith ("part select [" ^ string_of_int hi ^ ":" ^ 
                                        string_of_int lo ^ "] is out of bounds [" ^ 
                                        string_of_int (width a - 1) ^ ":0]");
        if lo = 0 && hi = (width a-1) then a
        else Bits.select a hi lo

    let select_e a hi lo = 
        try select a hi lo
        with _ -> Bits.empty

    let msb a = select a (width a - 1) (width a - 1)
    let lsbs a = select a (width a - 2) 0
    let lsb a = select a 0 0
    let msbs a = select a (width a - 1) 1
    let bit s n = select s n n

    let drop_bottom x n = select x (width x - 1) n
    let drop_top x n = select x (width x - 1 - n) 0
    let sel_bottom x n = select x (n-1) 0
    let sel_top x n = select x (width x - 1) (width x - n)
    let insert ~t ~f n = 
      let wt, wf = width t, width f in
      if n < 0 then failwith "insert <0"
      else if wt < (wf + n) then failwith "insert overflow"
      else if wt = wf && n = 0 then f
      else if n=0 then select t (wt - 1) wf @: f
      else if wt = (wf + n) then f @: select t (wt - wf - 1) 0
      else select t (wt - 1) (wf + n) @: f @: select t (n-1) 0

    let sel x (h,l) = select x h l

    (* error checking *)   
    let assert_widths_same l msg = 
        let w = width (List.hd l) in
        List.iter (fun a ->
            if width a <> w then
            begin
                let ws = List.fold_left 
                    (fun a s -> a ^ " " ^ (string_of_int (width s)))
                    "" l
                in
                failwith (msg ^ ":" ^ ws)
            end
        ) l

    let assert_width signal w msg =
        if width signal <> w then
            failwith msg

    let assert_width_or_empty signal w msg = 
        if signal <> empty then
            assert_width signal w msg

    let assert_vdd_gnd_empty signal msg = 
        if not (signal = gnd || signal = vdd || signal = empty) then
            failwith msg

    let assert_width_one signal msg = 
        if not ((width signal) = 1) then
            failwith msg

    let op_int_right op a b = op a (consti (width a) b)

    (* mux *)
    let mux sel l = 
        let els = List.length l in
        let max_els = 1 lsl (width sel) in
        assert_widths_same l "All inputs to a mux must be the same width";
        if els > max_els then failwith "too many inputs to mux";
        if els < 2 then failwith "mux must have at least 2 inputs";
        Bits.mux sel l
    
    let mux2 sel a b = 
        assert_width_one sel "select argument to mux2 must be 1 bit";
        mux sel [b;a]

    let mux_init sel n f = mux sel (Array.to_list (Array.init n f))

    let cases sel default l = 
      let max = 1 + List.fold_left (fun acc (i,_) -> max i acc) 0 l in
      let a = Array.make max default in
      let () = List.iter (fun (i,x) -> a.(i) <- x) l in
      if 1 lsl (width sel) = max then
        mux sel (Array.to_list a)
      else
        mux sel (Array.to_list a @ [default])

    (* logical *)
    let (&:) a b = 
        assert_widths_same [a;b] "Operands to &: must be the same width";
        Bits.(&:) a b
    
    let (|:) a b = 
        assert_widths_same [a;b] "Operands to |: must be the same width";
        Bits.(|:) a b
    
    let (^:) a b = 
        assert_widths_same [a;b] "Operands to ^: must be the same width";
        Bits.(^:) a b
    
    let (~:) = Bits.(~:)
    
    let ( &:. ) a b = op_int_right (&:) a b
    let ( |:. ) a b = op_int_right (|:) a b
    let ( ^:. ) a b = op_int_right (^:) a b

    (* arithmetic *)
    let (+:) a b = 
        assert_widths_same [a;b] "Operands to +: must be the same width";
        Bits.(+:) a b
    
    let (-:) a b = 
        assert_widths_same [a;b] "Operands to -: must be the same width";
        Bits.(-:) a b
    
    let (+:.) a b = op_int_right (+:) a b
    let (-:.) a b = op_int_right (-:) a b

    let negate a = (zero (width a)) -: a

    let ( *: ) = Bits.( *: )
    
    let ( *+ ) = Bits.( *+ )

    (* comparison *)
    let (==:) a b = 
        assert_widths_same [a;b] "Operands to ==: must be the same width";
        Bits.(==:) a b
    
    let (<>:) a b = 
        assert_widths_same [a;b] "Operands to <>: must be the same width";
        ~: (a ==: b)

    let (<:) a b = 
        assert_widths_same [a;b] "Operands to <: must be the same width";
        Bits.(<:) a b

    let lt = (<:)
    
    let (>:) a b = b <: a
    let (<=:) a b = ~: (a >: b)
    let (>=:) a b = ~: (a <: b)

    let (<+) a b =
        let f a = (~: (msb a)) @: (lsbs a) in
        if width a = 1 then a &: (~: b)
        else (f a) <: (f b)

    let (>+) a b =
        let f a = (~: (msb a)) @: (lsbs a) in
        if width a = 1 then b &: (~: a)
        else (f a) >: (f b)

    let (<=+) a b =
        let f a = (~: (msb a)) @: (lsbs a) in
        if width a = 1 then ~: (a >+ b)
        else (f a) <=: (f b)

    let (>=+) a b =
        let f a = (~: (msb a)) @: (lsbs a) in
        if width a = 1 then ~: (a <+ b)
        else (f a) >=: (f b)
 
    let (==:.) a b = op_int_right (==:) a b
    let (<>:.) a b = op_int_right (<>:) a b
    let (<:.) a b = op_int_right (<:) a b
    let (>:.) a b = op_int_right (>:) a b
    let (<=:.) a b = op_int_right (<=:) a b
    let (>=:.) a b = op_int_right (>=:) a b
    let (<+.) a b = op_int_right (<+) a b
    let (>+.) a b = op_int_right (>+) a b
    let (<=+.) a b = op_int_right (<=+) a b
    let (>=+.) a b = op_int_right (>=+) a b

    let to_string a = Bits.to_string a
    let to_int a = Bits.to_int a
    let to_bstr a = Bits.to_bstr a

    let rec bits s = 
        if width s = 0 then []
        else if width s = 1 then [s]
        else msb s :: bits (lsbs s)

    let bits s = 
        let a = Array.init (width s) (fun i -> bit s i) in
        List.rev (Array.to_list a)

    let to_array b = Array.of_list (List.rev (bits b))
    let of_array l = concat (List.rev (Array.to_list l))

    let wire = Bits.wire

    let (<==) = Bits.(<==) 
    let assign = Bits.(<==)

    let wireof s = 
        let x = wire (width s) in
        x <== s;
        x

    let input name width = wire width -- name

    let output name s = 
        let w = wire (width s) -- name in
        w <== s;
        w

    let clock = (wire 1) -- "clock"
    let reset = (wire 1) -- "reset"
    let clear = (wire 1) -- "clear"
    let enable = (wire 1) -- "enable"

    (*
    let rec repeat s n = 
        if n = 0 then empty
        else if n = 1 then s
        else
            concat [s; repeat s (n-1)]

    *)

    (* a smarter repeat function which generates log2 as much code *)
    let repeat s n = 
        match n with
        | 0 -> empty
        | 1 -> s
        | _ ->
            let rec build pwr rep_s res_s n = 
                if n = 0 then res_s
                else if pwr land n <> 0 then 
                    build (pwr*2) (rep_s @: rep_s) (concat_e [rep_s; res_s]) (n-pwr)
                else
                    build (pwr*2) (rep_s @: rep_s) res_s n
            in
            build 1 s empty n

    let split s = 
        let w = width s in
        select s (w-1) (w/2), select s ((w/2)-1) 0

    let sll a shift = 
        if shift < 0 then failwith ("Expecting positive shift amount");
        if shift = 0 then a
        else if shift >= (width a) then zero (width a)
        else concat [ (select a ((width a) - 1 - shift) 0); (zero shift) ]
                                    
    let srl a shift = 
        if shift < 0 then failwith ("Expecting positive shift amount");
        if shift = 0 then a
        else if shift >= (width a) then zero (width a)
        else concat [ (zero shift); (select a ((width a) - 1) shift) ]

    let sra a shift = 
        if shift < 0 then failwith ("Expecting positive shift amount");
        if shift = 0 then a
        else if shift >= (width a) then repeat (msb a) (width a)
        else concat [ (repeat (msb a) shift); (select a ((width a) - 1) shift) ]
                                    
    let log_shift op a b  = 
        let rec sft a n = 
            if n = width b then a
            else 
                let s = mux2 (bit b n) (op a (1 lsl n)) a in
                sft s (n+1)
            in
        sft a 0

    let uresize s w =
        let x = width s in
        if w = x then s
        else if w > x then concat[ (repeat gnd (w-x)); s ]
        else select s (w-1) 0

    let sresize s w =
        let x = width s in
        if w = x then s
        else if w > x then concat[ (repeat (msb s) (w-x)); s ]
        else select s (w-1) 0

    let ue s = uresize s ((width s)+1)
    let se s = sresize s ((width s)+1)

    let to_sint a = to_int (sresize a (Utils.platform_bits-1))

    let reduce op s = 
        match List.length s with
        | 0 -> failwith "Cant reduce no signals"
        | _ -> List.fold_left (fun acc x -> op acc x) (List.hd s) (List.tl s)

    let (||:) a b = (reduce (|:) (bits a)) |: (reduce (|:) (bits b))
    let (&&:) a b = (reduce (|:) (bits a)) &: (reduce (|:) (bits b))

    let pmux list last = 
      (List.fold_left (fun f (c, d) -> (fun s -> f (mux2 c d s))) (fun s -> s) list) last

    let pmuxl list = snd (reduce (fun (s0,d0) (s1,d1) -> (s0 |: s1), mux2 s0 d0 d1) list)

    let pmux1h list = reduce (|:) (List.map (fun (s,d) -> sresize s (width d) &: d) list)

    let reverse a = concat (List.rev (bits a))

    let mod_counter max c = 
        let w = width c in
        let lmax = 1 lsl w in
        if lmax = (max + 1) then c +: (one w)
        else mux2 (c ==: (consti w max)) (zero w) (c +: (one w))

    (** creates a tree of operations.  The arity of the operator is configurable *)
    let rec tree arity ops l =
        let split l n =
            let (lh,ll,_) = List.fold_left (fun (l0,l1,m) e ->
                if m < n then ((e::l0),l1,m+1) else (l0,e::l1,m+1)) ([],[],0) l 
            in
            (List.rev lh, List.rev ll) 
        in
        let rec t0 l =
            let l0,l1 = split l arity in
            if l1 = [] then [ ops l0 ]
            else (ops l0) :: (t0 l1) 
        in
        match l with
        | [] -> failwith "Invalid list given to tree"
        | [a] -> a
        | _ -> tree arity ops (t0 l) 
 

    let binary_to_onehot s = 
        let rec build = function
            | [] -> []
            | a :: [] -> [a; ~:a]
            | a :: b -> 
                List.map ((&:) a) (build b) @ 
                List.map ((&:) (~: a)) (build b)
        in
        concat (build (bits s)) 

    let onehot_to_binary x =
        let n = Utils.nbits (width x - 1) in
        let x = List.rev (bits x) in
        let rec f i = 
            if i=n then []
            else
                let rec g j = function
                    | [] -> []
                    | h::t -> begin
                        let c = j land (1 lsl i) <> 0 in
                        if c then h :: g (j+1) t
                        else g (j+1) t
                    end
                in
                let g = g 0 x in
                match g with
                | [] -> gnd :: f (i+1)
                | _ -> reduce (|:) g :: f (i+1)
        in
        concat List.(rev (f 0))

    let binary_to_gray b = b ^: (srl b 1)
    
    let gray_to_binary b = 
      let ue x = uresize x (width b) in
      let rec f b mask = 
        let b = b ^: (ue mask) in
        if width mask = 1 then b
        else f b (msbs mask)
      in
      f b (msbs b)

    let ssub v off len = String.Sub.to_string @@ String.sub ~start:off ~stop:(off+len) v
    let smake len c = String.v ~len (fun _ -> c)

    (* complex constant generators *)
    let rec constd bits v = 
        let l = String.length v in
        let decimal v = 
            match v with
            | '0' -> consti 4 0
            | '1' -> consti 4 1
            | '2' -> consti 4 2
            | '3' -> consti 4 3
            | '4' -> consti 4 4
            | '5' -> consti 4 5
            | '6' -> consti 4 6
            | '7' -> consti 4 7
            | '8' -> consti 4 8
            | '9' -> consti 4 9
            | _ -> failwith "constd: invalid char in const"
        in
        let (+:) a b = 
            let w = max (width a) (width b) + 1 in
            let a, b = uresize a w, uresize b w in
            a +: b
        in
        let ten = consti 4 10 in
        if l=0 then failwith "constd: string lenth=0"
        else
            if v.[0] = '-' then
                zero bits -: (constd bits (ssub v 1 (l-1)))
            else
                (* convert *)
                let rec sum i mulfac prod = 
                    if i<0 then prod
                    else 
                        sum (i-1) (mulfac *: ten) 
                            (prod +: (decimal v.[i] *: mulfac))
                in
                uresize
                    (sum (l-1) (consti 1 1) (consti 1 0))
                    bits

    let constv s = 
        let slen, sval = 
            let rec split2 n c s t = 
                if t.[n] = c then
                    s, ssub t (n + 1) (String.length t - n - 1)
                else
                    split2 (n+1) c (s ^ (smake 1 t.[n])) t
            in
            let s0,s1 = 
                try split2 0 '\'' "" s
                with _ -> failwith ("Invalid constant " ^ s)
            in
            if String.length s0 < 1 || String.length s1 < 1 then
                failwith ("Invalid constant " ^ s);
            s0,s1
        in
        let len = int_of_string slen in
        let ctrl = sval.[0] in
        let sval = ssub sval 1 (String.length sval - 1) in
        match ctrl with
        | 'd' -> constd len sval
        | 'x' | 'h' -> consthu len sval
        | 'X' | 'H' -> consths len sval
        | 'b' -> 
          let slen = String.length sval in
          if slen < len then constb ((smake (len-slen) '0' ) ^ sval)
          else if slen > len then constb (ssub sval (slen-len) len)
          else constb sval
        | 'B' ->
          let slen = String.length sval in
          if slen < len then constb ((smake (len-slen) sval.[0]) ^ sval)
          else if slen > len then constb (ssub sval (slen-len) len)
          else constb sval
        | _ -> failwith ("Invalid verilog style constant bad control character " ^ s)

    let const b = 
        try
            try constv b
            with _ -> constb b
        with _ -> failwith ("Cant convert constant " ^ b ^ " using verilog or binary formatting")

    let rec srand bits = 
        if bits <= 16 then consti bits (Random.int (1 lsl bits))
        else
            consti 16 (Random.int (1 lsl 16)) @: srand (bits-16)

    let to_int32' resize x = 
        let x = resize x 32 in
        let a = Int32.of_int (to_int (select x 15  0)) in
        let b = Int32.of_int (to_int (select x 31 16)) in
        fst (List.fold_left 
            (fun (acc,n) a -> Int32.(logor (shift_left a n) acc),n+16)
            (0l,0) [a;b])

    let to_int64' resize x = 
        let x = resize x 64 in
        let a = Int64.of_int (to_int (select x 15  0)) in
        let b = Int64.of_int (to_int (select x 31 16)) in
        let c = Int64.of_int (to_int (select x 47 32)) in
        let d = Int64.of_int (to_int (select x 63 48)) in
        fst (List.fold_left 
            (fun (acc,n) a -> Int64.(logor (shift_left a n) acc),n+16)
            (0L,0) [a;b;c;d])

    let to_int32 = to_int32' uresize
    let to_sint32 = to_int32' sresize
    let to_int64 = to_int64' uresize
    let to_sint64 = to_int64' sresize

    module type TypedMath = sig
        type v
        val of_signal : t -> v
        val to_signal : v -> t
        val (+:) : v -> v -> v
        val (-:) : v -> v -> v
        val ( *: ) : v -> v -> v
        val (<:) : v -> v -> v
        val (>:) : v -> v -> v
        val (<=:) : v -> v -> v
        val (>=:) : v -> v -> v
        val (==:) : v -> v -> v
        val (<>:) : v -> v -> v
        val resize : v -> int -> v
    end

    (* general arithmetic on unsigned signals.  operands and results are resized
     * to fit a appropriate *)
    module Unsigned = struct
        type v = t
        let of_signal s = s
        let to_signal s = s
        let resize s i = uresize s i

        let re size op a b = 
            let wa,wb = width a, width b in
            let w = size wa wb in
            let a, b = resize a w, resize b w in
            op a b
        let re0 = re max
        let re1 = re (fun a b -> (max a b) + 1)

        let (+:) = re1 (+:)
        let (-:) = re1 (-:)
        let ( *: ) = ( *: )
        let (<:) = re0 (<:)
        let (>:) = re0 (>:)
        let (<=:) = re0 (<=:)
        let (>=:) = re0 (>=:)
        let (==:) = re0 (==:) 
        let (<>:) = re0 (<>:)
    end

    module Signed = struct
        type v = t
        let of_signal s = s
        let to_signal s = s
        let resize s i = sresize s i

        let re size op a b = 
            let wa,wb = width a, width b in
            let w = size wa wb in
            let a, b = resize a w, resize b w in
            op a b
        let re0 = re max
        let re1 = re (fun a b -> (max a b) + 1)

        let (+:) = re1 (+:)
        let (-:) = re1 (-:)
        let ( *: ) = ( *+ )
        let (<:) = re0 (<+)
        let (>:) = re0 (>+)
        let (<=:) = re0 (<=+)
        let (>=:) = re0 (>=+)
        let (==:) = re0 (==:) 
        let (<>:) = re0 (<>:)
    end

    module Uop = Unsigned
    module Sop = Signed

end


