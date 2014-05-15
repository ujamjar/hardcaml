(* 
 * hardcaml - hardware design in OCaml
 *
 *   (c) 2014 MicroJamJar Ltd
 *
 * Author(s): andy.ray@ujamjar.com
 * Description: 
 *
 *)

(* array bits builder with pre allocated arrays for a less
   memory hungy simulator.

    XXX do as much setup as possible before the operation which will be 
        done repeatedly within the simulator

*)

module RawBuilder(B : Bits.ArraybitsBase) = 
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

end

