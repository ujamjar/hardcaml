(* 
 * hardcaml - hardware design in OCaml
 *
 *   (c) 2014 MicroJamJar Ltd
 *
 * Author(s): andy.ray@ujamjar.com
 * Description: 
 *
 *)


(* generate the gate level Comb.T api, based on the 4 basic boolean operators *)
module MakeGates(B : Comb.T)(S : sig
    val (~:) : B.t -> B.t 
    val (&:) : B.t -> B.t -> B.t
    val (|:) : B.t -> B.t -> B.t
    val (^:) : B.t -> B.t -> B.t
end) =
struct
    open B
    include S

    (* utils *)
    let vdd = const "1"
    let gnd = const "0"
    let bits_lsb x = 
        let w = width x in
        Array.to_list (Array.init w (fun i -> select x i i))
    let bits_msb x = List.rev (bits_lsb x)
    let reduce_bits def op a = List.fold_left op def (bits_lsb a)
    let repeat s n = 
        if n <= 0 then empty
        else concat (Array.to_list (Array.init n (fun _ -> s)))
    let concat_e l = 
        let x = List.filter ((<>)empty) l in
        if x = [] then empty
        else concat x

    let ripple_carry_adder cin a b =
        let fa cin a b = 
            let sum = (a ^: b) ^: cin in
            let carry = (a &: b) |: (b &: cin) |: (cin &: a) in
            sum, carry
        in
        let a = bits_lsb a in
        let b = bits_lsb b in
        let sum,_ = List.fold_left2 (fun (sum_in,carry_in) a b -> 
            let sum,carry_out = fa carry_in a b in
            sum::sum_in, carry_out) ([],cin) a b
        in
        concat sum

    (** addition *)
    let (+:) a b = ripple_carry_adder gnd a b
    
    (** subtraction *)
    let (-:) a b = ripple_carry_adder vdd a (~: b) 
    
    (** unsigned multiplication *)
    let ( *: ) a b = 
        let _,r = List.fold_left (fun (i,acc) b -> 
            let acc = concat_e [gnd; acc] in
            let a = concat_e [ gnd ; a ; repeat gnd i ] in
            i+1, (+:) acc ((&:) a (repeat b (width a)))
        ) (0,(repeat gnd (width a))) (bits_lsb b) in
        r
    
    (** signed multiplication *)
    let ( *+ ) a b = 
        let last = (width b) - 1 in
        let msb x = select x (width x - 1) (width x -1 ) in
        let _,r = List.fold_left (fun (i,acc) b -> 
            let acc = concat_e [msb acc; acc] in
            let a = concat_e [ msb a; a ; repeat gnd i ] in
            i+1, (if i = last then (-:) else (+:)) acc ((&:) a (repeat b (width a)))
        ) (0,(repeat gnd (width a))) (bits_lsb b) in
        r

    (** equality *)
    let (==:) a b = 
        let eq = (~: (a &: (~: b))) &: (~: ((~: a) &: b)) in
        reduce_bits vdd (&:) eq
    
    (** less than *)
    let (<:) a b = 
        let w = width a in
        let a,b = concat [gnd;a], concat [gnd;b] in
        let d = a -: b in
        select d w w
    
    (** multiplexer *)
    let mux s d = 
        let mux2 sel a b = 
            assert (width sel = 1);
            let s = repeat sel (width a) in
            (s &: a) |: ((~: s) &: b) 
        in
        let d' = List.hd (List.rev d) in
        (* generate the 'n' input mux structure 'bottom-up'.
         * it works from the lsb of the select signal.  Pairs
         * from the data list are mux'd together and we recurse 
         * until the select is complete.  Proper 'default' handling 
         * is included with the '[a]' case in 'zip' *)
        let rec build s d = 
            match s with 
            | [] -> List.hd d
            | s::s' ->
                let rec zip l = 
                    match l with
                    | [] -> []
                    | [a] -> [mux2 s d' a] 
                    (* | [a] -> [a] simpler *)
                    | a::b::tl -> mux2 s b a :: zip tl
                in
                build s' (zip d)
        in
        build (bits_lsb s) d
end

module MakeAig(B : Comb.T) = 
struct
    include B
    module S = MakeGates(B)(struct
        let (~:) = B.(~:)
        let (&:) = B.(&:)
        let (|:) a b = ~: ((~: a) &: (~: b))
        let (^:) a b = ~: ( (~: (a &: (~: b))) &: (~: ((~: a) &: b)) )
    end)
    include S
end

module MakeNand(B : Comb.T) = 
struct
    include B
    module S = MakeGates(B)(struct
        let nand a b = B.(~:) (B.(&:) a b)
        let (~:) a = nand a a
        let (&:) a b = ~: (nand a b)
        let (|:) a b = nand (~: a) (~: b)
        let (^:) a b =
            let c = nand a b in
            nand (nand a c) (nand b c)
    end)
    include S
end

module MakeNor(B : Comb.T) = 
struct
    include B
    module S = MakeGates(B)(struct
        let nor a b = B.(~:) (B.(|:) a b)
        let (~:) a = nor a a
        let (&:) a b = nor (~: a) (~: b)
        let (|:) a b = ~: (nor a b)
        let (^:) a b = nor (nor (~: a) (~: b)) (nor a b)
    end)
    include S
end

open Signal.Types
open Signal.Comb
open Signal.Seq

module Signals = 
struct
    module Aig = Comb.Make(MakeAig(Signal.Comb))
    module Nand = Comb.Make(MakeNand(Signal.Comb))
    module Nor = Comb.Make(MakeNor(Signal.Comb))
end

type transform_fn = (uid -> signal) -> signal -> signal

module type TransformFn =
sig
    val transform : transform_fn
end

module MakeCombTransform(B : (Comb.T with type t = signal)) = 
struct 

    open Utils

    let transform find signal = 
        let dep n = find (uid (List.nth (deps signal) n)) in
        let new_signal = 
            match signal with
            | Signal_op(id,op) ->
            begin
                let op2 op = op (dep 0) (dep 1) in
                match op with
                | Signal_add -> op2 B.(+:)
                | Signal_sub -> op2 B.(-:)
                | Signal_mulu -> op2 B.( *: )
                | Signal_muls -> op2 B.( *+ )
                | Signal_and -> op2 B.(&:)
                | Signal_or -> op2 B.(|:)
                | Signal_xor -> op2 B.(^:)
                | Signal_eq -> op2 B.(==:)
                | Signal_not -> B.(~:) (dep 0)
                | Signal_lt -> op2 B.(<:)
                | Signal_cat -> B.concat (deps signal |> List.map (find << uid))
                | Signal_mux -> 
                    let sel = List.hd (deps signal) |> (find << uid) in
                    let cases = List.tl (deps signal) |> List.map (find << uid) in
                    B.mux sel cases
            end
            | Signal_empty -> B.empty
            | Signal_wire(id,d) -> 
                let w = B.wire (width signal) in
                if !d <> empty then B.(<==) w ((find << uid) !d);
                w
            | Signal_const(_,b) -> B.const b
            | Signal_select(id,h,l) -> B.select (dep 0) h l
            | Signal_reg(id,r) -> 
                reg 
                    { r with (* note; level constants are copied *)
                        reg_clock = (find << uid) r.reg_clock;
                        reg_reset = (find << uid) r.reg_reset;
                        reg_reset_value = (find << uid) r.reg_reset_value;
                        reg_clear = (find << uid) r.reg_clear;
                        reg_clear_value = (find << uid) r.reg_clear_value;
                        reg_enable = (find << uid) r.reg_enable
                    }
                    empty
                    (dep 0) 
            | Signal_mem(_,_,r,m) -> 
                let d' = dep 0 in
                let w' = dep 1 in
                let r' = dep 2 in
                let we' = (find << uid) r.reg_enable in
                memory
                    m.mem_size
                    { r with (* note; level constants are copied *)
                        reg_clock = (find << uid) r.reg_clock;
                        reg_reset = (find << uid) r.reg_reset;
                        reg_reset_value = (find << uid) r.reg_reset_value;
                        reg_clear = (find << uid) r.reg_clear;
                        reg_clear_value = (find << uid) r.reg_clear_value;
                        reg_enable = B.empty;
                    } we' w' d' r'
            | Signal_inst(id,uid',i) -> 
                let inputs = List.map (fun (name, input) -> (name, find (uid input))) i.inst_inputs in
                Signal_inst(
                    make_id (width signal) (List.map snd inputs),
                    new_id(),
                    { i with
                        inst_inputs = inputs;
                    })
        in
        (* apply names *)
        if new_signal = B.empty then
            new_signal
        else
            List.fold_left (fun s n -> s -- n) new_signal (names signal)

end

module CopyTransform = MakeCombTransform(Signal.Comb)

module SimplifyBusses = 
struct

    open Utils

    let rec get_bit s n = 
        match s with
        | Signal_op(_,op) when op = Signal_cat ->
            let d = deps s in
            let rec find lo s = 
                match s with
                | [] -> failwith "out of range"
                | h::t ->
                    if n >= lo && n < (lo + width h) then
                        bit h (n-lo)
                    else
                        find (lo + width h) t
            in
            find 0 d
        | _ -> failwith "expecting concatentation"


    let transform find signal =
        let dep d n = List.nth (deps d) n in
        match signal with
        (* simplify chained selects *)
        | Signal_select(id,h,l) -> 
        begin
            let d = find (uid (dep signal 0)) in
            match d with
            | Signal_select(id,h',l') ->
                let d = dep d 0 in
                select d (h+l') (l+l')
            (*    
            | Signal_op(_,op) when op = Signal_cat && width signal = 1 ->
                (* 1 bit select into a concatenation ... this doesnt always seem
                 * to work...? Not sure why, but if enabled simulations fail. *)
                get_bit d l 
            *)
            | _ -> CopyTransform.transform find signal
        end
        (* flatten concatentations *)
        | Signal_op(_,op) when op = Signal_cat->
        begin
            let d = List.map (find << uid) (deps signal) in
            (concat << List.flatten) (List.map (fun d ->
                match d with
                | Signal_op(_,op) when op = Signal_cat -> deps d
                | _ -> [d]) d)
        end 
        | _ -> CopyTransform.transform find signal
        
end

module SimplifyConstants = 
struct

    module StringMap = Map.Make(String)

    let transform =
        let map = ref StringMap.empty in
        let tryfind b m = try Some(StringMap.find b !map) with _ -> None in
        let f b = 
            match tryfind b !map with
            | None ->
                let s = const b in
                map := StringMap.add b s !map;
                s
            | Some(x) -> 
                x
        in 
        fun find signal ->
            match signal with
            | Signal_const(_,b) -> f b
            | _ -> CopyTransform.transform find signal

end

module ConstantPropagation_old = 
struct
    module B = Bits.Comb.IntbitsList
    open Utils

    let transform find signal = 
        let cpy () = CopyTransform.transform find signal in
        let dep n = find (uid (List.nth (deps signal) n)) in
        let cv s = B.const (const_value s) in
        let eqs s n = 
            let d = B.(==:) (cv s) (B.consti (width s) n) in
            B.to_int d = 1
        in
        let cst b = const (B.to_string b) in
        let new_signal = 
            match signal with
            | Signal_op(id,op) ->
            begin
                match op with
                | Signal_add -> 
                begin
                    let d0,d1 = dep 0, dep 1 in
                    match is_const d0, is_const d1 with
                    | true,true -> cst (B.(+:) (cv d0) (cv d1)) 
                    | true,false when eqs d0 0 -> d1 (* x+0=x *)
                    | false,true when eqs d1 0 -> d0 (* 0+x=x *)
                    | _ -> 
                        (* errr; doesnt this resize the vector??? 
                        if (uid d0) = (uid d1) then d0 @: gnd (* a+a=a<<1 *)
                        else *) 
                            cpy()
                end
                | Signal_sub -> 
                begin
                    let d0,d1 = dep 0, dep 1 in
                    match is_const d0, is_const d1 with
                    | true,true -> cst (B.(-:) (cv d0) (cv d1))
                    | false,true when eqs d1 0 -> d0 (* x-0=x *)
                    | _ -> 
                        if (uid d0) = (uid d1) then zero (width signal) (* x-x=0 *)
                        else cpy()
                end
                | Signal_mulu -> 
                begin
                    let d0,d1 = dep 0, dep 1 in
                    let is_pow2 d = List.fold_left (fun a b -> a+b) 0 (cv d) = 1 in
                    let pow_bit d = List.fold_left 
                        (fun (n,m) b -> n+1,if b=1 then n else m) (0,0) 
                        (cv d |> List.rev) 
                        |> snd
                    in
                    let opt d c =
                        if eqs c 0 then zero (width signal)
                        else if eqs c 1 then (zero (width c)) @: d
                        else if is_pow2 c then 
                            let p = pow_bit c in
                            if p = 0 then uresize d (width signal)
                            else uresize (d @: (zero p)) (width signal)
                        else cpy()
                    in
                    match is_const d0, is_const d1 with
                    | true,true -> cst (B.( *: ) (cv d0) (cv d1)) 
                    | true,false -> opt d1 d0
                    | false,true -> opt d0 d1
                    | _ -> cpy()
                end
                | Signal_muls -> 
                begin
                    let d0,d1 = dep 0, dep 1 in
                    match is_const d0, is_const d1 with
                    | true,true -> cst (B.( *+ ) (cv d0) (cv d1)) 
                    (* | we could do certain optimisations here *)
                    | _ -> cpy()
                end
                | Signal_and -> 
                begin
                    let d0,d1 = dep 0, dep 1 in
                    let opt d c = 
                        if eqs c 0 then zero (width signal)
                        else if eqs c (-1) then d
                        else cpy()
                    in
                    match is_const d0, is_const d1 with
                    | true,true -> cst (B.(&:) (cv d0) (cv d1)) 
                    | true,false -> opt d1 d0
                    | false,true -> opt d0 d1
                    | _ -> cpy()
                end
                | Signal_or -> 
                begin
                    let d0,d1 = dep 0, dep 1 in
                    let opt d c = 
                        if eqs c 0 then d
                        else if eqs c (-1) then ones (width signal)
                        else cpy()
                    in
                    match is_const d0, is_const d1 with
                    | true,true -> cst (B.(|:) (cv d0) (cv d1)) 
                    | true,false -> opt d1 d0
                    | false,true -> opt d0 d1
                    | _ -> cpy()
                end
                | Signal_xor -> 
                begin
                    let d0,d1 = dep 0, dep 1 in
                    match is_const d0, is_const d1 with
                    | true,true -> cst (B.(^:) (cv d0) (cv d1)) 
                    | _ -> cpy()
                end
                | Signal_eq -> 
                begin
                    let d0,d1 = dep 0, dep 1 in
                    match is_const d0, is_const d1 with
                    | true,true -> cst (B.(==:) (cv d0) (cv d1)) 
                    | _ -> cpy()
                end
                | Signal_not -> 
                begin
                    let d = dep 0 in
                    if is_const d then cst (B.(~:) (cv d)) 
                    else cpy()
                end
                | Signal_lt -> 
                begin
                    let d0,d1 = dep 0, dep 1 in
                    match is_const d0, is_const d1 with
                    | true,true -> cst (B.(<:) (cv d0) (cv d1)) 
                    | _ -> cpy()
                end
                | Signal_cat -> 
                begin
                    (* all elements are constants (note, there might be some
                     * value to doing this as adjacent constants, but the
                     * analysis is a bit harder *)
                    let is_const = List.fold_left
                        (fun b d -> b && is_const (find (uid d))) 
                        true (deps signal)
                    in
                    if is_const then
                        cst
                            (B.concat 
                                (List.map (fun d -> cv (find (uid d))) 
                                (deps signal)))
                    else
                        cpy()
                end
                | Signal_mux -> 
                begin
                    (* - select signal is constant grab the element
                     * - all cases are the same??? *)
                    let sel = dep 0 in
                    let els = List.tl (deps signal) in
                    if is_const sel then
                        let x = B.to_int (cv sel) in
                        let len = List.length els in
                        let x = min x (len-1) in (* clip select *)
                        find (uid (List.nth els x))
                    else
                        cpy()
                end
            end
            | Signal_select(id,h,l) -> 
            begin
                (* - constant
                 * - indices match size of signal *)
                let d = dep 0 in
                if is_const d then cst (B.select (cv d) h l)
                else if width d = width signal then d
                else cpy()
            end
            | _ -> cpy()
        in
        assert (width new_signal = width signal); (* this must always hold *)
        (* apply names *)
        if new_signal = empty then
            new_signal
        else
            List.fold_left (fun s n -> s -- n) new_signal (names signal)

end

(* TODO: actually test me - somewhat copied from the ConstantPropagation_old
 * version *)
module ConstantPropagation = MakeCombTransform(Const_prop.Base)

module NandTransform = MakeCombTransform(Signals.Nand)
module NorTransform = MakeCombTransform(Signals.Nor)
module AigTransform = MakeCombTransform(Signals.Aig)

open Circuit
open Utils

let copy_names s t = 
    List.fold_left (fun t n -> t -- n) t (names s)

let partition compare set = 
    UidSet.fold (fun k (tr,fl) -> 
        if compare k then UidSet.add k tr, fl
        else tr, UidSet.add k fl) set (UidSet.empty,UidSet.empty)

let rewrite fn id_to_sig outputs = 
    let idv k v = k in
    let set_of_map f map = UidMap.fold (fun k v s -> UidSet.add (f k v) s) map UidSet.empty in 
    let set_of_list f l = List.fold_left (fun s v -> UidSet.add (f v) s) UidSet.empty [] in

    let partition compare set = 
        UidSet.fold (fun k (tr,fl) -> 
            if compare k then UidSet.add k tr, fl
            else tr, UidSet.add k fl) set (UidSet.empty,UidSet.empty)
    in

    let find uid = UidMap.find uid id_to_sig in
    
    (*let partition_const = partition (find >> is_const) in*)
    let partition_wire = partition (find >> is_wire) in

    let partition_ready ready remaining = 
        let ready s = 
            let s = find s in (* look up the signal *)
            let dep_set = set_of_list uid (deps s) in
            UidSet.subset dep_set ready
        in
        let new_ready, not_ready = partition ready remaining in
        if UidSet.cardinal new_ready = 0 then failwith "Could not schedule anything"
        else new_ready, not_ready
    in

    (*
    let all_set = set_of_map idv id_to_sig in
    let const_set,remaining_set = partition_const all_set in
    let wire_set,remaining_set = partition_wire remaining_set in
    let ready_set = UidSet.union wire_set const_set in
    *)
    let all_set = set_of_map idv id_to_sig in
    let wire_set,remaining_set = partition_wire all_set in
    let ready_set = wire_set in

    (* copy the wires and constants.  We potentially dont need to do this for
     * the constants, but the wires must be done this way to break combinatorial
     * dependancies *)
    let map = 
        UidSet.fold (fun uid map ->
            let signal = find uid in
            match signal with
            | Signal_wire(_) -> UidMap.add uid (copy_names signal (wire (width signal))) map
            (*| Signal_const(_) -> UidMap.add uid signal map*)
            | _ -> failwith "unexpected signal"
        ) ready_set UidMap.empty
    in

    (* now recursively rewrite nodes as they become ready *)
    let rec rewrite map ready remaining = 
        if UidSet.cardinal remaining = 0 then map
        else
            let find_new map uid = UidMap.find uid map in
            let new_ready, new_remaining = partition_ready ready remaining in
            (* rewrite the ready nodes *)
            let map = 
                UidSet.fold (fun uid map ->
                    let old_signal = find uid in
                    let new_signal = fn (find_new map) old_signal in
                    UidMap.add uid new_signal map
                ) new_ready map
            in
            rewrite map (UidSet.union ready new_ready) new_remaining
    in

    let map = rewrite map ready_set remaining_set in

    (* reattach all wires *)
    UidSet.iter (fun uid' ->
        let o = UidMap.find uid' id_to_sig in
        let n = UidMap.find uid' map in
        match o with 
        | Signal_wire(id,d) -> 
            if !d <> empty then
                let d = UidMap.find (uid !d) map in
                n <== d
        | _ -> failwith "expecting a wire") wire_set;
    (* find new outputs *)
    let outputs = 
        List.map (fun signal -> UidMap.find (uid signal) map) 
            outputs
    in
    outputs

let rewrite_signals fn signals = 
    let id_to_sig = Circuit.search
        (fun map signal -> UidMap.add (uid signal) signal map)
        Circuit.id UidMap.empty signals
    in
    rewrite fn id_to_sig signals

let rewrite_circuit fn c = 
    Circuit.make (Circuit.name c) 
        (rewrite fn 
            (Circuit.signal_map c)
            (Circuit.outputs c))

(* combinatorial pipelining 
 *
 *  1) schedule in stages
 *  2) trace back and accumulate actual timing for each stage
 *  3) insert registers at stages where timing budget is exceeded
 *  4) we might them be able to shift the registers across stages
 *     depending on interstage dependancies - the aim would be to
 *     reduce register usage within the timing budget, if possible.
 *
 *)

(* we can do either breadth first of depth first search from the inputs
 * and outputs *)

(* deps : uid -> uid set (of dependancies)
 * visited : uid set (of visited nodes)
 *)
(*
let rec bfs level deps visited f g a s = 
    (* find *)
    (* create a set of all depenancies *)
    let deps' = 
        UidSet.fold (fun u s ->
            UidSet.union (deps u) s) UidSet.empty s
    in
    (* next set of visited *)
    let visited' = UidSet.union visited deps' in
    (* new nodes we havent yet visited *)
    let vnew = UidSet.diff visited' visited in

    if UidSet.cardinal vnew = 0 then
        a
    else
        (* now search all deps *)
        let a = f level s vnew a in
        let a = bfs (level+1) deps visited' f g a vnew in
        let a = g level s vnew a in
        a

let rec dfs level deps visited f g a s = 
    let visited' = UidSet.add s visited in
    let a = UidSet.fold (fun v a -> dfs (level+1) deps visited) (deps s) a in
    let 
*)

