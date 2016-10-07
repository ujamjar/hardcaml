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
let failwith str = raise (Failure str)

module Types = 
struct

    type signal_op = 
        | Signal_add
        | Signal_sub
        | Signal_mulu
        | Signal_muls
        | Signal_and
        | Signal_or
        | Signal_xor
        | Signal_eq
        | Signal_not
        | Signal_lt
        | Signal_cat
        | Signal_mux

    type uid = int64
    module UidMap = Map.Make(Int64)
    module UidSet = Set.Make(Int64)

    type signal_id = 
        {
            s_id : uid;
            mutable s_names : string list;
            s_width : int;
            mutable s_deps : signal list
        }

    and signal =
        | Signal_empty
        | Signal_const of signal_id * string
        | Signal_op of signal_id * signal_op
        | Signal_wire of signal_id * signal ref
        | Signal_select of signal_id * int * int
        | Signal_reg of signal_id * register
        | Signal_mem of signal_id * uid * register * memory
        | Signal_inst of signal_id * uid * instantiation

    (* These types are used to define a particular type of register 
     * as per the following template, where each part is optional;
     *
     * always @(?edge clock, ?edge reset)
     *   if (reset == reset_level) d <= reset_value;
     *   else if (clear == clear_level) d <= clear_value;
     *   else if (enable) d <= ...;
     *
     *)
    and register = 
        {
            reg_clock : signal;             (* clock *)
            reg_clock_level : signal;       (* active clock edge *)
            reg_reset : signal;             (* asynchronous reset *)
            reg_reset_level : signal;       (* asynchronous reset level *)
            reg_reset_value : signal;       (* asychhronous reset value *)
            reg_clear : signal;             (* synchronous reset *)
            reg_clear_level : signal;       (* synchronous reset level *)
            reg_clear_value : signal;       (* sychhronous reset value *)
            reg_enable : signal;            (* global system enable *) 
        }

    and memory = 
        {
            mem_size : int;
            mem_read_address : signal;
            mem_write_address : signal;
        }

    and instantiation = 
        {
            inst_name : string;                          (* name of circuit *)
            inst_generics : (string * parameter) list;   (* [ "ram_type" => ParamString("auto"); ] *)
            inst_inputs : (string * signal) list;        (* name and input signal *)
            inst_outputs : (string * (int * int)) list;  (* name, width and low index of output *)
            inst_lib : string;
            inst_arch : string;
        }

    and parameter = 
        | ParamString of string
        | ParamInt of int
        | ParamFloat of float
        | ParamBool of bool

    let rec uid s = 
        match s with
        | Signal_empty -> 0L
        | Signal_const(s,_)
        | Signal_select(s,_,_) 
        | Signal_reg(s,_)
        | Signal_mem(s,_,_,_)
        | Signal_wire(s,_)
        | Signal_inst(s,_,_)
        | Signal_op(s,_) -> s.s_id

    let rec deps s =
        match s with 
        | Signal_empty | Signal_const _ -> []
        | Signal_select(s,_,_)
        | Signal_reg(s,_)
        | Signal_mem(s,_,_,_)
        | Signal_inst(s,_,_) 
        | Signal_op(s,_) -> s.s_deps
        | Signal_wire(_,s) -> [!s]

    let rec names s =  
        match s with 
        | Signal_empty -> failwith "Cannot get names of 'empty' signal"
        | Signal_const(s,_) 
        | Signal_select(s,_,_) 
        | Signal_reg(s,_)
        | Signal_mem(s,_,_,_)
        | Signal_wire(s,_)
        | Signal_inst(s,_,_)
        | Signal_op(s,_) -> s.s_names

    let rec width s = 
        match s with
        | Signal_empty -> 0
        | Signal_const(s,_)
        | Signal_select(s,_,_) 
        | Signal_reg(s,_)
        | Signal_mem(s,_,_,_)
        | Signal_wire(s,_)
        | Signal_inst(s,_,_) 
        | Signal_op(s,_) -> s.s_width
 
    let is_reg = function Signal_reg(_) -> true | _ -> false
    let is_const = function Signal_const(_) -> true | _ -> false
    let is_select = function Signal_select(_) -> true | _ -> false
    let is_wire = function Signal_wire(_) -> true | _ -> false
    let is_op op = function Signal_op(_,o) -> o=op | _ -> false
    let is_mem = function Signal_mem(_) -> true | _ -> false
    let is_inst = function Signal_inst(_) -> true | _ -> false

    let const_value = function Signal_const(_,v) -> v | _ -> failwith "Constant expected"

    let new_id, reset_id = 
        let id = ref 1L in
        let new_id() = 
            let x = !id in
            id := (Int64.add (!id) 1L);
            x
        in
        let reset_id() = 
            id := 1L
        in
        new_id, reset_id

    let make_id w deps = 
        {
            s_id = new_id();
            s_names = [];
            s_width = w;
            s_deps = deps;
        }

    let depo signal = 
        let o = List.nth (deps signal) in
        let tl() = List.tl (deps signal) in
        object
            method sel = o 0
            method op1 = o 0
            method op2 = o 0, o 1
            method data = tl()
            (* other useful methods? *)
        end

    let string_of_op = function 
        | Signal_add -> "add"
        | Signal_sub -> "sub"
        | Signal_mulu -> "mulu"
        | Signal_muls -> "muls"
        | Signal_and -> "and"
        | Signal_or -> "or"
        | Signal_xor -> "xor"
        | Signal_eq -> "eq"
        | Signal_not -> "not"
        | Signal_lt -> "lt"
        | Signal_cat -> "cat"
        | Signal_mux -> "mux"

    let to_string signal =
        let names s = List.fold_left (fun a s -> if a = "" then s else a ^ "," ^ s) "" (names s) in
        let deps s = List.fold_left (fun a s -> 
            let s = Int64.to_string (uid s) in 
            if a = "" then s else a ^ "," ^ s) "" (deps s)
        in
        let sid s =
            "id:" ^ Int64.to_string (uid s) ^ " bits:" ^ string_of_int (width s) ^ 
            " names:" ^ names s ^ " deps:" ^ deps s ^ "" in
        match signal with
        | Signal_empty -> "Signal_empty" 
        | Signal_const(s,v) -> "Signal_const[" ^ sid signal ^ "] = " ^ v
        | Signal_op(s,o) -> "Signal_op[" ^ sid signal ^ "] = " ^ string_of_op o
        | Signal_wire(s,d) -> "Signal_wire[" ^ sid signal ^ "] -> " ^ Int64.to_string (uid !d)
        | Signal_select(s,h,l) -> "Signal_select[" ^ sid signal ^ "] " ^ string_of_int h ^ ".." ^ string_of_int l
        | Signal_reg(s,r) -> "Signal_reg[" ^ sid signal ^ "]"
        | Signal_mem(s,_,r,m) -> "Signal_mem[" ^ sid signal ^ "]"
        | Signal_inst(s,_,i) -> "Signal_inst" ^ sid signal ^ "]"

    let structural_compare
        ?(check_names=true) ?(check_deps=true) a b =
        let rec structural_compare 
            s a b = 
            if UidSet.mem (uid a) s then true
            else
                let s = UidSet.add (uid a) s in
                (* check we have the same type of node *)
                let typ() = 
                    match a,b with
                    | Signal_empty,Signal_empty -> 
                        true
                    | Signal_const(_,a),Signal_const(_,b) -> 
                        a=b
                    | Signal_select(_,h0,l0),Signal_select(_,h1,l1) -> 
                        (h0=h1) && (l0=l1)
                    | Signal_reg(_,_),Signal_reg(_,_) -> 
                        true
                    | Signal_mem(_,_,_,m0),Signal_mem(_,_,_,m1) -> 
                        m0.mem_size=m1.mem_size
                    | Signal_wire(_,_),Signal_wire(_,_) -> 
                        true
                    | Signal_inst(_,_,i0),Signal_inst(_,_,i1) -> 
                        (i0.inst_name=i1.inst_name) &&
                        (i0.inst_generics=i1.inst_generics) &&
                        (i0.inst_outputs=i1.inst_outputs)
                        (* inst_inputs=??? *)
                    | Signal_op(_,o0),Signal_op(_,o1) -> 
                        o0=o1
                    | _ -> false
                in
                let wid () = width a = width b in
                let names () = 
                    if check_names then try names a = names b with _ -> true 
                    else true
                in 
                let deps () =
                    if check_deps then 
                        try
                            List.fold_left2 
                                (fun b x y -> b && (structural_compare s x y)) 
                                true (deps a) (deps b)
                        with _ ->
                            false
                    else
                        false
                in
                typ() && wid() && names() && deps()
        in
        structural_compare UidSet.empty a b

end

module Base = 
struct
    open Types

    (* TODO: instantiations, memories *)

    type t = Types.signal

    let is_mutable = false

    let width = width

    let to_string = to_string

    let to_int signal = 
        if is_const signal then Utils.int_of_bstr (const_value signal)
        else failwith "signal does not support to_int unless it's a constant"
    let to_bstr signal = 
        if is_const signal then const_value signal
        else failwith "signal does not support to_bstr unless it's a constant"
    let to_bani_ptr _ _ = failwith "signal does not support to_bani"
    let of_bani_ptr _ _ _ = failwith "signal does not support of_bani"
    let to_bigint _ = failwith "signal does not support to_bigint"
    let of_bigint _ _ = failwith "signal does not support of_bigint"

    let empty = Signal_empty

    module StringMap = Map.Make(String)

    (* XXX warning!!! internal state is kept here - reset_id will no longer work *)
    let const = 
        let optimise = false in
        if not optimise then
            (fun b -> Signal_const(make_id (String.length b) [], b))
        else
            let map = ref StringMap.empty in
            let tryfind b m = try Some(StringMap.find b !map) with _ -> None in
            let f b = 
                match tryfind b !map with
                | None ->
                    let s = Signal_const(make_id (String.length b) [], b) in
                    map := StringMap.add b s !map;
                    s
                | Some(x) -> x
            in 
            f

    let wire w = Signal_wire(make_id w [], ref Signal_empty)
    let (--) signal name = 
        match signal with
        | Signal_empty -> failwith "Cannot name empty signal"
        | Signal_const(s,_)
        | Signal_op(s,_)
        | Signal_reg(s,_)
        | Signal_select(s,_,_)
        | Signal_mem(s,_,_,_)
        | Signal_inst(s,_,_)
        | Signal_wire(s,_) ->
            s.s_names <- name :: s.s_names;
        signal
    let op2 op len a b = Signal_op(make_id len [a;b], op)
    let concat a = 
        (* automatically concatenate successive constants *)
        let rec optimise_consts l = 
            match l with
            | [] -> []
            | a::[] -> [a]
            | a::b::tl ->
                if (is_const a) && (is_const b) then
                    optimise_consts ((const (const_value a ^ const_value b)) :: tl)
                else
                    a :: optimise_consts (b::tl)
        in
        let a = optimise_consts a in
        match a with 
        | [a] -> a
        | _ -> 
            let len = List.fold_left (fun acc a -> acc + width a) 0 a in
            Signal_op(make_id len a, Signal_cat)
    let select a hi lo = Signal_select(make_id (hi-lo+1) [a], hi, lo)
    let (+:) a b = op2 Signal_add (width a) a b 
    let (-:) a b = op2 Signal_sub (width a) a b
    let ( *: ) a b = op2 Signal_mulu (width a + width b) a b
    let ( *+ ) a b = op2 Signal_muls (width a + width b) a b 
    let (&:) a b = op2 Signal_and (width a) a b
    let (|:) a b = op2 Signal_or (width a) a b
    let (^:) a b = op2 Signal_xor (width a) a b
    let (~:) a = Signal_op(make_id (width a) [a], Signal_not)
    let (==:) a b = op2 Signal_eq 1 a b
    let (/=:) a b = ~: (a ==: b)
    let (<:) a b = op2 Signal_lt 1 a b

    let mux sel l =
        Signal_op(make_id (width (List.hd l)) (sel::l), Signal_mux)

    let (<==) a b = 
        match a with
        | Signal_wire(s,d) ->
            if !d <> empty then
                failwith "Wire has already been assigned to";
            if width a <> width b then
                failwith ("Assigning to wire of a different width: " ^ 
                    string_of_int (width a) ^ " " ^ 
                    string_of_int (width b)); 
            d := b
        | _ ->
            failwith "Only wires may be assigned to"

end

module Comb_make = Comb.Make
module Comb = Comb.Make(Base)

module Const_prop = struct

  module Base = struct

    open Types
    open Utils
    include Comb

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

  module Comb = Comb_make(Base)

end

module Seq = 
struct 
    open Types
    open Comb

    (* register with async reset *)
    let r_async = 
        {
            reg_clock = clock;
            reg_clock_level = vdd;
            reg_reset = reset;
            reg_reset_level = vdd;
            reg_reset_value = empty;
            reg_clear = empty;
            reg_clear_level = empty;
            reg_clear_value = empty;
            reg_enable = empty;
        }

    (* register with sync clear *)
    let r_sync = 
        {
            reg_clock = clock;
            reg_clock_level = vdd;
            reg_reset = empty;
            reg_reset_level = empty;
            reg_reset_value = empty;
            reg_clear = clear;
            reg_clear_level = vdd;
            reg_clear_value = empty;
            reg_enable = empty;
        }

    let r_full = 
        {
            reg_clock = clock;
            reg_clock_level = vdd;
            reg_reset = reset;
            reg_reset_level = vdd;
            reg_reset_value = empty;
            reg_clear = clear;
            reg_clear_level = vdd;
            reg_clear_value = empty;
            reg_enable = empty;
        }

    let r_none = 
        {
            reg_clock = clock;
            reg_clock_level = vdd;
            reg_reset = empty;
            reg_reset_level = empty;
            reg_reset_value = empty;
            reg_clear = empty;
            reg_clear_level = empty;
            reg_clear_value = empty;
            reg_enable = empty;
        }

    (* error checking *)   
    let assert_widths_same l msg = 
        let w = width (List.hd l) in
        List.iter (fun a ->
            if width a <> w then
                failwith msg
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

    let form_spec spec enable d = 
        (* XXX add checks back *)
        assert_width spec.reg_clock 1 "Clock signal to register must be 1 bit";
        assert_vdd_gnd_empty spec.reg_clock_level "Clock level signal to register must be 1 bit or empty";
        assert_width_or_empty spec.reg_reset 1 "Reset signal to register must be 1 bit or empty";
        assert_vdd_gnd_empty spec.reg_reset_level "Reset level signal to register must be 1 bit or empty";
        assert_width_or_empty spec.reg_reset_value (width d) "Reset value to register must be same width or empty";
        assert_width_or_empty spec.reg_clear 1 "Clear signal to register must be 1 bit or empty";
        assert_vdd_gnd_empty spec.reg_clear_level "Clear level signal to register must be 1 bit or empty";
        assert_width_or_empty spec.reg_clear_value (width d) "Clear value to register must be same width or empty";
        assert_width_or_empty spec.reg_enable 1 "Global enable signal to register must be 1 bit or empty"; 
        { spec with
            reg_clock_level = if spec.reg_clock_level = empty then vdd else spec.reg_clock_level;
            reg_reset_level = if spec.reg_reset_level = empty then vdd else spec.reg_reset_level;
            reg_reset_value = if spec.reg_reset_value = empty then zero (width d) else spec.reg_reset_value;
            reg_clear_value = if spec.reg_clear_value = empty then zero (width d) else spec.reg_clear_value;
            reg_enable = 
                let e0 = if spec.reg_enable = empty then vdd else spec.reg_enable in
                let e1 = if enable = empty then vdd else enable in
                if e0 = vdd && e1 = vdd then vdd
                else if e0 = vdd then e1
                else if e1 = vdd then e0
                else e0 &: e1  
        }

    let reg spec enable d = 
        let spec = form_spec spec enable d in
        let deps = 
            [
                spec.reg_clock; 
                spec.reg_reset; spec.reg_reset_value; spec.reg_reset_level;
                spec.reg_clear; spec.reg_clear_value; spec.reg_clear_level;
                spec.reg_enable
            ]
        in
        Signal_reg(make_id (width d) (d :: deps), spec)

    let reg_fb spec enable width f = 
        let d = wire width in
        let q = reg spec enable d in
        d <== (f q);
        q

    let rec pipeline n spec enable d = 
        if n=0 then d
        else reg spec enable (pipeline (n-1) spec enable d)

    let memory ~size ~spec ~we ~w ~d ~r = 
        assert (width we = 1);
        assert (size <= (1 lsl width w));
        assert (size > 0);
        assert (width w = width r);
        let spec = form_spec spec we d in
        let deps =
            [
                d; w; r; we; 
                spec.reg_clock; 
                spec.reg_reset; spec.reg_reset_value; spec.reg_reset_level;
                spec.reg_clear; spec.reg_clear_value; spec.reg_clear_level;
                spec.reg_enable
            ] 
        in
        Signal_mem(make_id (width d) deps,
            new_id(),
            spec,
            {
                mem_size = size;
                mem_write_address = w;
                mem_read_address = r;
            })

    let ram_rbw ~size ~spec ~we ~wa ~d ~re ~ra = 
        reg spec re (memory size spec we wa d ra)

    let ram_wbr ~size ~spec ~we ~wa ~d ~re ~ra = 
        memory size spec we wa d (reg spec re ra)

    (* simple dual port RAM's with 1:N or N:1 bus resizing *)
    let ram_1xn ?(ram=ram_rbw) ?(ram_type=r_none) ?(reg_type=r_none) 
        ~we ~w ~d ~re ~r = 
        if width w = width r then
            ram (1 lsl width w) r_none we w d re r
        else if width r > width w then
            (* output bus is smaller than input, which we implement with a mux *)
            let ln = width r - width w in
            let n = 1 lsl ln in
            let q = ram ~size:(1 lsl width w) ~spec:ram_type ~we ~wa:w ~d ~re 
                        ~ra:(select r (width r - 1) ln) 
            in
            let rr = reg reg_type re (select r (ln-1) 0) in
            let wq = width d / n in
            let rec words m = 
                if n = m then []
                else select q ((wq * (m+1)) - 1) (wq * m) :: words (m+1)
            in
            mux rr (words 0)
        else
            (* output bus is bigger than input, which we implement with multiple banks *)
            let ln = width w - width r in
            let n = 1 lsl ln in
            let rec banks m = 
                if m = n then []
                else
                    let sel = select w (ln-1) 0 ==:. m in
                    ram ~size:(1 lsl width r) ~spec:ram_type 
                        ~we:(we &: sel)
                        ~wa:(select w (width w - 1) ln) 
                        ~d:d ~re:re ~ra:r :: banks (m+1)
            in
            concat (List.rev (banks 0))

end

module Instantiation = 
struct

    open Types
    open Comb

    type instobj = < i : string -> signal; o : string -> signal >

    let (==>) a b = a,b

    let inst ?(lib="work") ?(arch="rtl") name generics inputs outputs = 
        let width = List.fold_left (fun a (_,i) -> a+i) 0 outputs in
        let deps = List.map snd inputs in
        let outputs,_ = List.fold_left (fun (o,a) (n,w) -> (n,(w,a))::o, a+w) ([],0) outputs in
        let signal = Signal_inst(
            make_id width deps,
            new_id(),
            {
                inst_name = name;
                inst_generics = generics;
                inst_inputs = inputs;
                inst_outputs = outputs;
                inst_lib = lib;
                inst_arch = arch;
            })
        in
        let find name = 
            let w,o = List.assoc name outputs in
            select signal (o+w-1) o
        in
        object
            method i name = List.assoc name inputs
            method o name = find name
        end

end

module Guarded = 
struct

    open Types

    type statement = 
        | G_assign of guarded_var_i * Types.signal
        | G_if of Types.signal * statement list * statement list
        | G_switch of Types.signal * ((Types.signal * (statement list)) list)
        | G_none
    and statements = statement list
    and guarded_var_i =
        {
            (* wire that is assigned to *)
            gvar_wire : Types.signal;
            (* default, if no assignment *)
            gvar_default : Types.signal;
            (* output *)
            gvar_q : Types.signal;
        }
    and 'a case = 'a * statements
    and 'a cases = 'a case list

    class variable (var:guarded_var_i) = 
        object
            method g = var
            method q = var.gvar_q
        end

    let uid_of_var w = Types.uid (w.gvar_wire)

    module AssignOrd = 
    struct
        type t = guarded_var_i
        let compare a b = compare (uid_of_var a) (uid_of_var b)
    end
    module S = Set.Make(AssignOrd)

    (* api *)

    let g_wire' def = 
        let wire = Comb.wire (Comb.width def) in
        {
            gvar_wire = wire;
            gvar_default = def;
            gvar_q = wire;
        }
    let g_wire def = new variable(g_wire' def)

    let g_reg' spec enable w =
        let wire = Comb.wire w in
        let reg = Seq.reg spec enable wire in
        {
            gvar_wire = wire;
            gvar_default = reg;
            gvar_q = reg;
        }
    let g_reg spec enable w = new variable(g_reg' spec enable w)

    let g_var' spec enable w =
        let wire = Comb.wire w in
        let reg = Seq.reg spec enable wire in
        {
            gvar_wire = wire;
            gvar_default = reg;
            gvar_q = wire;
        }
    let g_var spec enable w = new variable(g_var' spec enable w)

    let g_pipeline' size spec enable w =
        match size with
        | 0 ->
            (* use a wire - need to derive the default value *)
            if spec.reg_reset_value = Comb.empty then
                g_wire' (Comb.zero w)
            else
                g_wire' spec.reg_reset_value
        | _ ->
            let r = g_reg' spec enable w in
            (* delay the output by the pipeline length, minus 1 *)
            { r with
                gvar_q = Seq.pipeline (size-1) spec enable r.gvar_q }

    let g_pipeline size spec enable w =
        new variable(g_pipeline' size spec enable w)

    let g_if sel on_true on_false = 
        G_if(sel, on_true, on_false)

    let g_elif c t f = [ g_if c t f ]

    let g_when sel on_true = g_if sel on_true []

    let g_unless sel on_false = g_if sel [] on_false

    let g_switch sel cases = 
        G_switch(sel, cases)

    let g_proc s = g_if Comb.vdd s []

    let ($==) a b = 
        if Comb.width (a#q) <> Comb.width b then
            failwith "Width of LHS and RHS are not the same in guarded assign";
        G_assign(a#g, b)

    let ($==.) a b = 
        a $== Comb.consti (Comb.width (a#q)) b

    (* compilation *)
(*
    open Printf

    let rec pretty_print i g = 
        let id i = String.make i ' ' in
        let rec pp i g = 
            match g with
            | G_assign(_,_) -> printf "%svar <= ?\n" (id i)
            | G_if(_,t,f) ->
            begin
                printf "%sif (?) {\n" (id i);
                pps (i+1) t;
                printf "%s} else {\n" (id i);
                pps (i+1) f;
                printf "%s}\n" (id i)
            end
            | G_switch(_,c) ->
            begin
                printf "%sswitch (_) {\n" (id i);
                List.iter (fun (m,c) ->
                    printf "%scase _ {\n" (id i);
                    pps (i+1) c;
                    printf "%s}\n" (id i)
                ) c;
                printf "%s}\n" (id i)
            end
            | G_none ->
                printf "%sNONE;\n" (id i)
        and pps i g = 
            List.iter (fun g -> pp i g) g
        in
        pps i g
*)

    let list_of_set s = 
        S.fold (fun e l -> e :: l) s [] 

    let rec find_targets set g = 
        List.fold_left (fun set g ->
            match g with
            | G_assign(g, _) -> S.add g set
            | G_if(_,t,f) -> 
                let set = find_targets set t in
                find_targets set f
            | G_switch(_,cases) ->
                List.fold_left (fun set case -> find_targets set (snd case)) set cases
            | G_none -> failwith "G_none should only be used internally"
        ) set g
    
    let filter_by_target tgt g = 
        let filter_none = List.filter ((<>) G_none) in
        let rec filter g = 
            match g with
            | G_assign(v, _) -> if uid_of_var v = tgt then g else G_none
            | G_if(s,t,f) -> G_if(s, filters t, filters f)
            | G_switch(sel,cases) -> G_switch(sel, List.map (fun (m,c) -> m,filters c) cases)
            | G_none -> failwith "G_none should only be used internally"
        and filters g = filter_none (List.map filter g) in
        let rec optimise g = 
            match g with
            | G_if(s,t,f) -> 
                let t = optimises t in
                let f = optimises f in
                if t=[] && f=[] then G_none
                else G_if(s,t,f)
            | G_switch(sel,cases) -> 
                let cases = List.map (fun (m,c) -> m, optimises c) cases in
                let cases = List.filter (fun (_,c) -> c <> []) cases in
                if cases=[] then G_none
                else G_switch(sel, cases)
            | _ -> g
        and optimises g = filter_none (List.map optimise g) in
        optimises (filters g)

    let rec compile_mux def g = 
        match g with
        | [] -> def
        | h::t ->
            let def = 
                match h with
                | G_if(s, t, f) ->
                    let s = Comb.reduce Comb.(|:) (Comb.bits s) in
                    let t = compile_mux def t in
                    let f = compile_mux def f in
                    Comb.mux s [f; t]
                | G_assign(_,d) -> d
                | G_switch(sel,cases) -> 
                     (* XXX optimization XXX *)
                     let rec build = function 
                        | [] -> def
                        | (mtch,case) :: t -> 
                            Comb.mux (Comb.(==:) sel mtch) [ build t; compile_mux def case ]
                     in
                     build cases
                | G_none -> failwith "G_none should only be used internally"
            in
            compile_mux def t

    let compile g = 
        (* split by targets *)
        let tgts = list_of_set (find_targets S.empty g) in
        let g = List.map 
            (fun tgt -> tgt, filter_by_target (uid_of_var tgt) g) 
            tgts
        in
        List.iter 
            (fun (tgt,g) -> 
                (*pretty_print 0 g;
                printf "--------------------------------------\n";*)
                Comb.(<==) tgt.gvar_wire (compile_mux tgt.gvar_default g)
            ) g
    
    (* statemachines based on a switch statement ie
      
        let _, switch, next_state = 
            statemachine r_sync enable [ "idle"; "run"] 
        in 

        compile [
            switch [
                "idle", [
                    g_if (start) [
                        next_state "run"
                    ] []
                ]
                "run", [
                    b $== ...
                    next_state "idle"
                ]
            ]
        ]
     *)

    let statemachine rspec enable states = 
        (* assign a value to each state *)
        let nstates = List.length states in
        let ls = Utils.clog2 nstates in
        let states = Utils.mapi (fun i s -> s, Comb.consti ls i) states in
        (* state variable *)
        let state_var = g_reg rspec enable ls in 
        (* update state *)
        let state_val s = 
            try List.assoc s states 
            with _ -> 
                (* report that we couldn't find the state.  We cant show which
                 * one, as we don't know it's type (even if it will generally be 
                 * a string *)
                failwith "couldn't find state"
        in
        let next_state s = state_var $== (state_val s) in
        let switch cases = 
            g_switch (state_var#q) 
                (List.map (fun (s, c) -> state_val s, c) cases)
        in
        state_var, switch, next_state


end

module Multiram = struct
  (* XXX can't remember if I tested this properly... *)
  open Comb

  let rec bin2oh s = 
    let (&::) a b = repeat a (width b) &: b in
    if width s = 1 then s @: ~: s 
    else 
        ((((msb s)) &:: bin2oh (lsbs s)) @: 
      ((~: (msb s)) &:: bin2oh (lsbs s)))


  let rec oh2bin s = 
    let pairs s = 
      let s = if width s mod 2 = 0 then s else gnd @: s in
      let b = List.rev (bits s) in
      Utils.zip (Utils.leven b) (Utils.lodd b)
    in
    let enc2_1 (a, b) = (b, a |: b) in
    if width s = 1 then gnd
    else if width s = 2 then bit s 1
    else
      let s, p = Utils.unzip (List.map enc2_1 (pairs s)) in
      oh2bin (concat (List.rev p)) @: reduce (|:) s

  let rec oh2bin_p s = 
    let w = width s in
    let l = Utils.nbits (w-1) in
    let rec f b i = 
      match b with
      | [] -> empty (* shouldnt get here *)
      | h::[] -> consti l i
      | h::t -> mux2 h (consti l i) (f t (i+1))
    in
    f (List.rev (bits s)) 0

  (* lvt multiport ram *)

  type 'a write = 
    {
      we : 'a;
      wd : 'a;
      wa : 'a;
    }
  type 'a read = 
    {
      re : 'a;
      ra : 'a;
    }
  type ram = size:int -> we:t -> wa:t -> d:t -> re:t -> ra:t -> t

  let ram_1wr ~ram ~size ~wr ~rd = 
    (* 1 write, n read ports *)
    Array.map 
      (fun rd ->
        ram ~size 
          ~we:wr.we ~wa:wr.wa ~d:wr.wd 
          ~re:rd.re ~ra:rd.ra) rd

  let lvt ~priority_write ~size ~spec ~wr ~rd = 
    let n_wr, n_rd = Array.length wr, Array.length rd in
    let bin2oh we s = Array.map ((&:) we) (Array.of_list (List.rev (bits (bin2oh s)))) in
    let we1h = Array.map (fun wr -> bin2oh wr.we wr.wa) wr in
    let regs = Array.init size (fun i -> 
      let wes = Array.init n_wr (fun j -> we1h.(j).(i)) in
      let we = reduce (|:) (Array.to_list wes) in
      let oh2bin = if priority_write then oh2bin_p else oh2bin in
      let d = oh2bin (concat (List.rev (Array.to_list wes))) in
      Seq.reg spec we d)
    in
    let regs = Array.to_list regs in
    Array.map (fun rd -> mux rd.ra regs) rd

  let ram ?(priority_write=false) ~ram ~size ~spec ~wr ~rd = 
    let n_wr, n_rd = Array.length wr, Array.length rd in
    let banks = Array.map (fun wr -> ram_1wr ~ram ~size ~wr ~rd) wr in
    let lvt = lvt ~priority_write ~size ~spec ~wr ~rd in
    let lvt = Array.init n_rd (fun i -> Seq.reg spec rd.(i).re lvt.(i)) in

    Array.init n_rd (fun i -> mux lvt.(i) 
      (Array.to_list (Array.init n_wr (fun j -> banks.(j).(i))))) 

end

module Statemachine = struct

  open Comb

  (* new statemchine implementation *)
  let statemachine_binary rspec enable states = 
    let open Guarded in
    (* assign a value to each state *)
    let nstates = List.length states in
    let ls = Utils.clog2 nstates in
    let states = Utils.mapi (fun i s -> s, consti ls i) states in
    (* state variable *)
    let state_var = g_reg rspec enable ls in 
    (* update state *)
    let state_val s = 
      try List.assoc s states 
      with _ -> 
        (* report that we couldn't find the state.  We cant show which
          * one, as we don't know it's type (even if it will generally be 
          * a string *)
        failwith "couldn't find state"
    in
    let next_state s = state_var $== (state_val s) in
    let state_var = state_var#q (*-- "state_binary"*) in
    let switch cases = 
      g_switch (state_var) 
        (List.map (fun (s, c) -> state_val s, c) cases)
    in
    (fun s -> state_val s ==: state_var),
    switch, next_state

  let statemachine_onehot rspec enable states = 
    let open Guarded in
    let nstates = List.length states in
    let onehot i = 
      let module B = Bits.Comb.IntbitsList in
      let ls = Utils.clog2 nstates in
      constibl B.(select (binary_to_onehot (consti ls i)) (nstates-1) 0)
    in
    let states = Utils.mapi 
      (fun i s -> s, (i, onehot i)) states 
    in
    let state_var = 
      g_reg 
        Types.({ rspec with (* must be reset to get into state 0 *)
          reg_clear_value = one nstates;
          reg_reset_value = one nstates; })
      enable nstates in 
    (* update state *)
    let state_val s = 
      try List.assoc s states 
      with _ -> 
        (* report that we couldn't find the state.  We cant show which
          * one, as we don't know it's type (even if it will generally be 
          * a string *)
        failwith "couldn't find state"
    in
    let next_state s = state_var $== snd (state_val s) in
    let state_var = state_var#q (*-- "state_onehot"*) in
    let switch cases = 
      g_proc
        (List.map (fun (s, c) ->
          let i, _ = state_val s in
          g_when (bit state_var i) c) cases)
    in
    (fun s -> bit state_var (fst (state_val s))),
    switch, next_state

  let statemachine_gray rspec enable states = 
    let open Guarded in
    (* assign a value to each state *)
    let nstates = List.length states in
    let ls = Utils.clog2 nstates in
    let gray i = 
      let module B = Bits.Comb.IntbitsList in
      constibl (B.binary_to_gray (B.consti ls i))
    in
    let states = Utils.mapi (fun i s -> s, gray i) states in
    (* state variable *)
    let state_var = g_reg rspec enable ls in 
    (* update state *)
    let state_val s = 
      try List.assoc s states 
      with _ -> 
        (* report that we couldn't find the state.  We cant show which
          * one, as we don't know it's type (even if it will generally be 
          * a string *)
        failwith "couldn't find state"
    in
    let next_state s = state_var $== (state_val s) in
    let state_var = state_var#q (*-- "state_gray"*) in
    let switch cases = 
      g_switch state_var 
        (List.map (fun (s, c) -> state_val s, c) cases)
    in
    (fun s -> state_val s ==: state_var),
    switch, next_state

  let statemachine ?(encoding=`binary) = 
    match encoding with
    | `binary -> statemachine_binary
    | `onehot -> statemachine_onehot
    | `gray -> statemachine_gray 

end

module type Seq_spec = sig
    val reg_spec : Types.register
    val ram_spec : Types.register
end

module type Seq = sig

    open Types

    val reg : 
        ?clk:signal -> ?clkl:signal ->
        ?r:signal -> ?rl:signal -> ?rv:signal ->
        ?c:signal -> ?cl:signal -> ?cv:signal ->
        ?ge:signal ->
        e:signal -> signal -> signal

    val reg_fb : 
        ?clk:signal -> ?clkl:signal ->
        ?r:signal -> ?rl:signal -> ?rv:signal ->
        ?c:signal -> ?cl:signal -> ?cv:signal ->
        ?ge:signal ->
        e:signal -> w:int -> (signal -> signal) -> signal

    val pipeline : 
        ?clk:signal -> ?clkl:signal ->
        ?r:signal -> ?rl:signal -> ?rv:signal ->
        ?c:signal -> ?cl:signal -> ?cv:signal ->
        ?ge:signal ->
        n:int -> e:signal -> signal -> signal

    open Guarded

    val g_reg : 
        ?clk:signal -> ?clkl:signal ->
        ?r:signal -> ?rl:signal -> ?rv:signal ->
        ?c:signal -> ?cl:signal -> ?cv:signal ->
        ?ge:signal ->
        e:signal -> int -> variable

    val g_pipeline : 
        ?clk:signal -> ?clkl:signal ->
        ?r:signal -> ?rl:signal -> ?rv:signal ->
        ?c:signal -> ?cl:signal -> ?cv:signal ->
        ?ge:signal ->
        n:int -> e:signal -> int -> variable

    val statemachine : 
        ?clk:signal -> ?clkl:signal ->
        ?r:signal -> ?rl:signal -> ?rv:signal ->
        ?c:signal -> ?cl:signal -> ?cv:signal ->
        ?ge:signal ->
        e:signal -> 'a list -> 
        (('a -> signal) * ('a cases -> statement) * ('a -> statement))

    val memory : 
        ?clk:signal -> ?clkl:signal ->
        ?r:signal -> ?rl:signal -> ?rv:signal ->
        ?c:signal -> ?cl:signal -> ?cv:signal ->
        ?ge:signal ->
        int -> we:signal -> wa:signal -> d:signal -> ra:signal -> signal

    val ram_wbr : 
        ?clk:signal -> ?clkl:signal ->
        ?r:signal -> ?rl:signal -> ?rv:signal ->
        ?c:signal -> ?cl:signal -> ?cv:signal ->
        ?ge:signal ->
        int -> we:signal -> wa:signal -> d:signal -> re:signal -> ra:signal -> signal

    val ram_rbw : 
        ?clk:signal -> ?clkl:signal ->
        ?r:signal -> ?rl:signal -> ?rv:signal ->
        ?c:signal -> ?cl:signal -> ?cv:signal ->
        ?ge:signal ->
        int -> we:signal -> wa:signal -> d:signal -> re:signal -> ra:signal -> signal

    val multi_ram_wbr : ?priority_write:bool -> 
      rd:signal Multiram.read array ->
      wr:signal Multiram.write array ->
      int -> signal array

    val multi_ram_rbw : ?priority_write:bool -> 
      rd:signal Multiram.read array ->
      wr:signal Multiram.write array ->
      int -> signal array

end

module Make_seq(S : Seq_spec) = struct

    let make_spec 
        ?clk ?clkl 
        ?r ?rl ?rv 
        ?c ?cl ?cv 
        ?ge spec =
        let sel a b = 
            match a with
            | None -> b
            | Some(x) -> x
        in
        Types.({
            reg_clock       = sel clk  spec.reg_clock;
            reg_clock_level = sel clkl spec.reg_clock_level;
            reg_reset       = sel r    spec.reg_reset;
            reg_reset_level = sel rl   spec.reg_reset_level;
            reg_reset_value = sel rv   spec.reg_reset_value;
            reg_clear       = sel c    spec.reg_clear;
            reg_clear_level = sel cl   spec.reg_clear_level;
            reg_clear_value = sel cv   spec.reg_clear_value;
            reg_enable      = sel ge   spec.reg_enable;
        })

    let reg 
        ?clk ?clkl 
        ?r ?rl ?rv 
        ?c ?cl ?cv 
        ?ge ~e d =
        let spec = make_spec ?clk ?clkl ?r ?rl ?rv ?c ?cl ?cv ?ge S.reg_spec in
        Seq.reg spec e d

    let reg_fb 
        ?clk ?clkl 
        ?r ?rl ?rv 
        ?c ?cl ?cv 
        ?ge ~e ~w f =
        let spec = make_spec ?clk ?clkl ?r ?rl ?rv ?c ?cl ?cv ?ge S.reg_spec in
        Seq.reg_fb spec e w f

    let pipeline 
        ?clk ?clkl 
        ?r ?rl ?rv 
        ?c ?cl ?cv 
        ?ge ~n ~e d =
        let spec = make_spec ?clk ?clkl ?r ?rl ?rv ?c ?cl ?cv ?ge S.reg_spec in
        Seq.pipeline n spec e d

    let g_reg
        ?clk ?clkl 
        ?r ?rl ?rv 
        ?c ?cl ?cv 
        ?ge ~e w =
        let spec = make_spec ?clk ?clkl ?r ?rl ?rv ?c ?cl ?cv ?ge S.reg_spec in
        Guarded.g_reg spec e w

    let g_pipeline 
        ?clk ?clkl 
        ?r ?rl ?rv 
        ?c ?cl ?cv 
        ?ge ~n ~e w =
        let spec = make_spec ?clk ?clkl ?r ?rl ?rv ?c ?cl ?cv ?ge S.reg_spec in
        Guarded.g_pipeline n spec e w
 
    let statemachine
        ?clk ?clkl 
        ?r ?rl ?rv 
        ?c ?cl ?cv 
        ?ge ~e states =
        let spec = make_spec ?clk ?clkl ?r ?rl ?rv ?c ?cl ?cv ?ge S.reg_spec in
        Statemachine.statemachine spec e states
 
    let memory
        ?clk ?clkl 
        ?r ?rl ?rv 
        ?c ?cl ?cv 
        ?ge  
        size ~we ~wa ~d ~ra =
        let spec = make_spec ?clk ?clkl ?r ?rl ?rv ?c ?cl ?cv ?ge S.ram_spec in
        Seq.memory ~size ~spec ~we ~w:wa ~d ~r:ra

    let ram_wbr 
        ?clk ?clkl 
        ?r ?rl ?rv 
        ?c ?cl ?cv 
        ?ge  
        size ~we ~wa ~d ~re ~ra =
        let spec = make_spec ?clk ?clkl ?r ?rl ?rv ?c ?cl ?cv ?ge S.ram_spec in
        Seq.ram_wbr ~size ~spec ~we ~wa ~d ~re ~ra

    let ram_rbw 
        ?clk ?clkl 
        ?r ?rl ?rv 
        ?c ?cl ?cv 
        ?ge  
        size ~we ~wa ~d ~re ~ra =
        let spec = make_spec ?clk ?clkl ?r ?rl ?rv ?c ?cl ?cv ?ge S.ram_spec in
        Seq.ram_rbw ~size ~spec ~we ~wa ~d ~re ~ra

    let multi_ram_wbr ?priority_write ~rd ~wr size =
        Multiram.ram ?priority_write ~ram:(Seq.ram_wbr ~spec:S.ram_spec)
          ~size ~spec:S.reg_spec ~wr ~rd

    let multi_ram_rbw ?priority_write ~rd ~wr size =
        Multiram.ram ?priority_write ~ram:(Seq.ram_rbw ~spec:S.ram_spec)
          ~size ~spec:S.reg_spec ~wr ~rd

end

