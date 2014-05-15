(* 
 * hardcaml - hardware design in OCaml
 *
 *   (c) 2014 MicroJamJar Ltd
 *
 * Author(s): andy.ray@ujamjar.com
 * Description: 
 *
 *)

module Base = struct

    open Signal.Types
    open Utils
    include Signal.Comb

    module B = Bits.Comb.IntbitsList
    
    let cv s = B.const (const_value s)
    let eqs s n = 
        let d = B.(==:) (cv s) (B.consti (width s) n) in
        B.to_int d = 1
    let cst b = const (B.to_string b)

    let (+:) a b = 
        match is_const a, is_const b with
        | true,true -> cst (B.(+:) (cv a) (cv b))
        | true,false when eqs a 0 -> b (* 0+b *)
        | false,true when eqs b 0 -> a (* a+0 *)
        | _ -> (+:) a b

    let (-:) a b = 
        match is_const a, is_const b with
        | true,true -> cst (B.(-:) (cv a) (cv b))
        (* | true,false when eqs a 0 -> b *)
        | false,true when eqs b 0 -> a (* a-0 *)
        | _ -> (-:) a b

    
    let ( *: ) a b = 
        let w = width a + width b in
        let is_pow2 d = List.fold_left (fun a b -> a+b) 0 (cv d) = 1 in
        let pow_bit d = List.fold_left 
            (fun (n,m) b -> n+1,if b=1 then n else m) (0,0) 
            (cv d |> List.rev) 
            |> snd
        in
        let opt d c =
            if eqs c 0 then zero w 
            else if eqs c 1 then (zero (width c)) @: d
            else if is_pow2 c then 
                let p = pow_bit c in
                if p = 0 then uresize d w 
                else uresize (d @: (zero p)) w
            else ( *: ) a b
        in
        match is_const a, is_const b with
        | true,true -> cst (B.( *: ) (cv a) (cv b)) 
        | true,false -> opt b a
        | false,true -> opt a b
        | _ -> ( *: ) a b

    let ( *+ ) a b =
        match is_const a, is_const b with
        | true,true -> cst (B.( *+ ) (cv a) (cv b)) 
        (* | we could do certain optimisations here *)
        | _ -> ( *+ ) a b

    let (&:) a b = 
        let opt d c = 
            if eqs c 0 then zero (width a)
            else if eqs c (-1) then d
            else (&:) a b
        in
        match is_const a, is_const b with
        | true,true -> cst (B.(&:) (cv a) (cv b)) 
        | true,false -> opt b a
        | false,true -> opt a b
        | _ -> (&:) a b

    let (|:) a b = 
        let opt d c = 
            if eqs c 0 then d
            else if eqs c (-1) then ones (width a)
            else (|:) a b
        in
        match is_const a, is_const b with
        | true,true -> cst (B.(|:) (cv a) (cv b)) 
        | true,false -> opt b a
        | false,true -> opt a b
        | _ -> (|:) a b

    let (^:) a b = 
        match is_const a, is_const b with
        | true,true -> cst (B.(^:) (cv a) (cv b)) 
        | _ -> (^:) a b

    let (==:) a b = 
        match is_const a, is_const b with
        | true,true -> cst (B.(==:) (cv a) (cv b)) 
        | _ -> (==:) a b

    let (~:) a = 
        match is_const a with
        | true -> cst (B.(~:) (cv a)) 
        | _ -> (~:) a

    let (<:) a b = 
        match is_const a, is_const b with
        | true,true -> cst (B.(<:) (cv a) (cv b)) 
        | _ -> (<:) a b

    let concat l = 
        let rec f l nl = 
            match l with
            | [] -> List.rev nl
            | h :: t when is_const h -> begin
                match nl with
                | h' :: t' when is_const h' -> 
                    f t (cst (B.concat [cv h'; cv h]) :: t')
                | _ -> f t (h :: nl)
            end
            | h :: t -> f t (h :: nl)
        in
        concat (f l [])

    (*
    let is_rom els = 
        List.fold_left (fun b s -> b && is_const s) true els

    let opt_rom sel els = 
        let len = List.length els in
        let len' = 1 lsl (width sel) in
        let els =  
            if len' <> len then
                let e = List.nth els (len'-1) in
                els @ linit (len'-len) (fun _ -> e)
            else
                els
        in
        mux sel els
    *)

    let mux sel els = 
        let len = List.length els in
        (*let len' = 1 lsl (width sel) in*)
        if is_const sel then
            let x = B.to_int (cv sel) in
            let x = min x (len-1) in (* clip select *)
            List.nth els x
        (*else if is_rom els && len <= len' then
            opt_rom sel els*)
        else
            mux sel els

    let select d h l =
        if is_const d then cst (B.select (cv d) h l)
        else if l=0 && h = width d - 1 then d
        else select d h l

end

module Comb = Comb.Make(Base)

