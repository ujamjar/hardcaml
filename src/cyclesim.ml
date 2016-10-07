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
open Circuit
open Signal.Types

(* tail recursive list concatenation *)
let (@) a b = 
    let a = List.rev a in
    let rec f a b =
        match b with 
        | [] -> a
        | h::t -> f (h :: a) t
    in
    List.rev (f a b)

let map f l = 
    let rec fn l a = 
        match l with
        | [] -> a
        | h::t -> fn t (f h :: a)
    in
    List.rev (fn l [])

exception Failure of string
let failwith str = raise (Failure str)

let scheduler dependants remaining computed =
    let set_add_list set l = List.fold_left (fun set signal -> UidSet.add (uid signal) set) set l in

    let rec scheduler remaining computed computed_set = 
        let failed() = failwith "No cells can be scheduled" in

        if remaining = [] then
            List.rev computed
        else
            let is_computed signal = UidSet.mem (uid signal) computed_set in
            let is_ready signal = 
                List.for_all is_computed (List.filter ((<>) Signal.Comb.empty) (dependants signal)) in
            let ready, not_ready = List.partition is_ready remaining in
            if ready = [] then failed();
            scheduler not_ready (ready @ computed) (set_add_list computed_set ready)
    in
    scheduler remaining [] (set_add_list UidSet.empty computed)

let find_elements circuit =
    Circuit.search
        (fun (regs, mems, consts, inputs, remaining) signal ->
            if signal = Signal.Comb.empty then
                (regs, mems, consts, inputs ,remaining)
            else if is_reg signal then
                (signal::regs, mems, consts, inputs ,remaining)
            else if is_const signal then
                (regs, mems, signal::consts, inputs ,remaining)
            else if Circuit.is_input circuit signal then
                (regs, mems, consts, signal::inputs ,remaining)
            else if is_mem signal then
                (regs, signal::mems, consts, inputs, remaining)
            else
                (regs, mems, consts, inputs ,signal::remaining)
        ) id ([],[],[],[],[]) (Circuit.outputs circuit)

module Api = 
struct

    type task = unit -> unit

    type 'a cyclesim =
        {
            sim_in_ports : (string * 'a ref) list; 
            sim_out_ports : (string * 'a ref) list;
            sim_out_ports_next : (string * 'a ref) list;
            sim_internal_ports : (string * 'a ref) list;
            sim_reset : task;
            sim_cycle_check : task;
            sim_cycle_comb0 : task;
            sim_cycle_seq : task;
            sim_cycle_comb1 : task;
            sim_lookup_signal : uid -> 'a ref;
            sim_lookup_reg : uid -> 'a ref;
            sim_lookup_memory : uid -> 'a array;
        }

    let cycle_check sim = sim.sim_cycle_check ()
    let cycle_comb0 sim = sim.sim_cycle_comb0 ()
    let cycle_seq sim = sim.sim_cycle_seq ()
    let cycle_comb1 sim = sim.sim_cycle_comb1 ()
    let reset sim = sim.sim_reset ()
    let cycle sim = 
      cycle_check sim;
      cycle_comb0 sim;
      cycle_seq sim;
      cycle_comb1 sim

    let in_port sim name = try List.assoc name sim.sim_in_ports with _ ->
        failwith ("couldn't find input port " ^ name)
    let out_port sim name = try List.assoc name sim.sim_out_ports with _ ->
        failwith ("cound't find output port " ^ name)
    let out_port_next sim name = try List.assoc name sim.sim_out_ports_next with _ ->
        failwith ("cound't find output port " ^ name)
    let internal_port sim name = try List.assoc name sim.sim_internal_ports with _ ->
        failwith ("cound't find internal port " ^ name)

    let in_ports sim = sim.sim_in_ports
    let out_ports sim = sim.sim_out_ports
    let out_ports_next sim = sim.sim_out_ports_next
    let internal_ports sim = sim.sim_internal_ports

end

module Make = functor (Bits : Comb.S) ->
struct 

    open Api

    type t = Bits.t

    type cyclesim = t Api.cyclesim

    type get_internal = (signal -> bool) option
    type run_inst = Signal.Types.instantiation -> t list -> t list
    type get_inst = string -> run_inst option

    let make ?(log=(fun s -> ())) 
             ?(internal=None) 
             ?(inst=(fun s -> None)) circuit =  

        log "internal ports";

        (* add internally traced nodes *)
        let internal_ports = 
            (* create name mangler *)
            let name = Circuit.mangle_names [] "_" circuit in
            let i = match internal with
                | None -> []
                | Some(f) -> 
                    Circuit.filter (fun s ->
                        not (Circuit.is_input circuit s) &&
                        not (Circuit.is_output circuit s) &&
                        s <> Signal.Comb.empty && 
                        f s) (Circuit.outputs circuit)
            in
            (* create a wire for each node, and give it the mangled names.
             * NOTE: these wire are required to make registers 'look' like
             * they are updating correctly in simulation, however, they are
             * not specifically needed for combinatorial nodes.  It does make
             * the name mangling scheme a wee bit easier though. *)
            List.map (fun s ->
                let w = Signal.Comb.wire (Signal.Comb.width s) in
                Signal.Comb.(<==) w s;
                Utils.iteri (fun i _ -> 
                    ignore (Signal.Comb.(--) w (name (uid s) i))
                ) (names s);
                w) i
        in

        log "scheduler";

        (* schedule the simulation *)
        let regs, mems, consts, inputs, remaining = find_elements circuit in
        let ready = regs @ inputs @ consts in
        let deps' s = 
            match s with 
            | Signal_mem(_, _,  _, m) -> [m.mem_read_address]
            | _ -> deps s
        in
        let schedule = scheduler deps' (internal_ports @ mems @ remaining) ready in

        log "data map";

        (* create the data needed for simulation *)
        let data_map = 
            List.fold_left (fun map signal -> 
                let value = match signal with
                    | Signal_const(_,v) -> Bits.const v
                    | _ -> Bits.zero (Signal.Comb.width signal)
                in
                UidMap.add (uid signal) (ref value) map
            ) UidMap.empty (try (ready @ internal_ports @ mems @ remaining) 
                            with _ -> failwith "during concatenation")
        in

        log "reg map";
        
        let reg_map = 
            List.fold_left (fun map signal -> 
                UidMap.add (uid signal) (ref (Bits.zero (Signal.Comb.width signal))) map
            ) UidMap.empty regs 
        in
        log "mem map";
        let mem_map = 
            List.fold_left (fun map signal ->
                match signal with
                | Signal_mem(_, _, _, m) ->
                    let mem = Array.init m.mem_size 
                        (fun _ -> Bits.zero (Signal.Comb.width signal)) 
                    in
                    UidMap.add (uid signal) mem map
                | _ -> failwith "Expecting memory"
            ) UidMap.empty mems
        in
        
        (* compilation *)
        let compile signal = 
            let tgt = UidMap.find (uid signal) data_map in
            let deps = List.map (fun signal -> 
                    try UidMap.find (uid signal) data_map
                    with _ -> ref Bits.empty
                ) (deps signal) in
            match signal with
            | Signal_empty -> failwith "cant compile empty signal"
            | Signal_const(_) -> None 
            | Signal_op(_,op) ->
            begin
                let op2 op = 
                    let a = List.nth deps 0 in
                    let b = List.nth deps 1 in
                    Some(fun () -> tgt := op !a !b;)
                in 
                match op with
                | Signal_add -> op2 Bits.(+:) 
                | Signal_sub -> op2 Bits.(-:) 
                | Signal_mulu -> op2 Bits.( *: ) 
                | Signal_muls -> op2 Bits.( *+ )
                | Signal_and -> op2 Bits.(&:)
                | Signal_or -> op2 Bits.(|:)
                | Signal_xor -> op2 Bits.(^:)
                | Signal_eq -> op2 Bits.(==:)
                | Signal_not -> Some(fun () -> tgt := Bits.(~:) !(List.hd deps))
                | Signal_lt -> op2 Bits.(<:)
                | Signal_cat -> Some(fun () -> tgt := Bits.concat (List.map (!) deps))
                | Signal_mux -> 
                    (* tgt := Bits.mux !(List.hd deps) (List.map (!) (List.tl deps)) *)
                    (* this optimisation makes a large performance difference *)
                    let sel = List.hd deps in
                    let els = Array.of_list (List.tl deps) in
                    let max = Array.length els - 1 in
                    Some(fun () -> 
                        let sel = Bits.to_int (!sel) in
                        let sel = if sel > max then max else sel in
                        tgt := !(els.(sel))
                    )
            end
            | Signal_wire(_,d) -> 
                let src = List.hd deps in
                Some(fun () -> tgt := !src)
            | Signal_select(_,h,l) -> Some(fun () -> tgt := Bits.select !(List.hd deps) h l) 
            | Signal_reg(_,r) -> 
            begin
                let tgt = UidMap.find (uid signal) reg_map in
                let src = List.hd deps in
                let clr = 
                    if r.reg_clear <> Signal.Comb.empty then 
                        Some(UidMap.find (uid r.reg_clear) data_map,
                             UidMap.find (uid r.reg_clear_value) data_map,
                             Bits.to_int !(UidMap.find (uid r.reg_clear_level) data_map))
                    else None 
                in
                let ena = 
                    if r.reg_enable <> Signal.Comb.empty then
                        Some(UidMap.find (uid r.reg_enable) data_map)
                    else None 
                in
                match clr,ena with
                | None,None -> Some(fun () -> tgt := !src)
                | Some(c,v,l),None -> 
                    Some(fun () -> 
                        if Bits.to_int !c = l then tgt := !v
                        else tgt := !src)
                | None,Some(e) -> Some(fun () -> if Bits.to_int !e  = 1 then tgt := !src)
                | Some(c,v,l),Some(e) -> 
                    Some(fun () -> 
                        if Bits.to_int !c = l then tgt := !v
                        else if Bits.to_int !e = 1 then tgt := !src)
            end
            | Signal_mem(_,_,r,m) -> 
            begin
                let mem = UidMap.find (uid signal) mem_map in
                let addr = UidMap.find (uid m.mem_read_address) data_map in
                Some(fun () ->
                    try 
                        tgt := mem.(Bits.to_int !addr)
                    with _ ->
                        tgt := Bits.zero (Signal.Comb.width signal)
                )
            end
            | Signal_inst(_,_,i) -> 
            begin
                match inst i.inst_name with
                | None -> failwith ("Instantiation " ^ i.inst_name ^ 
                                    " not supported in simulation")
                | Some(f) -> begin
                    Some(fun () ->
                        tgt := Bits.concat (List.rev (f i (List.map (!) deps)))
                    ) 
                end 
            end
        in

        let compile_reg_update signal = 
            match signal with
            | Signal_reg(_,_) ->
                let tgt = UidMap.find (uid signal) data_map in
                let src = UidMap.find (uid signal) reg_map in
                (fun () -> tgt := !src)
            | _ -> failwith "error while compiling reg update"
        in

        let compile_mem_update signal = 
            match signal with
            | Signal_mem(_,_,r,m) ->
                let mem = UidMap.find (uid signal) mem_map in
                let we = UidMap.find (uid r.reg_enable) data_map in
                let w = UidMap.find (uid m.mem_write_address) data_map in
                let d = UidMap.find (uid (List.hd (deps signal))) data_map in
                (fun () ->
                    (* XXX memories can have resets/clear etc as well *)
                    if Bits.to_int !we = 1 then
                        mem.(Bits.to_int !w) <- !d
                )
            | _ -> failwith "error while compiling mem update"
        in

        let compile_reset signal = 
            match signal with
            | Signal_reg(_,r) ->
                if r.reg_reset <> Signal.Comb.empty then
                    let tgt0 = UidMap.find (uid signal) data_map in
                    let tgt1 = UidMap.find (uid signal) reg_map in
                    let value = UidMap.find (uid r.reg_reset_value) data_map in
                    Some(fun () -> 
                        tgt0 := !value;
                        tgt1 := !value)
                else 
                    None
            | _ -> failwith "Only registers should have a reset"
        in

        let check_input signal = 
            let signal_width = Signal.Comb.width signal in
            let name = List.hd (names signal) in
            let tgt = UidMap.find (uid signal) data_map in
            (fun () ->
                let data_width = Bits.width !tgt in
                if data_width != signal_width then
                    failwith 
                        (Printf.sprintf "'%s' has width %i but should be of width %i" 
                            name data_width signal_width))
        in

        (* compile the task list *)
        log "compile tasks";
        let filter_none l =
            let l = List.filter ((<>) None) l in
            map (function Some(x) -> x | _ -> failwith "error") l
        in
        let tasks_check = map check_input inputs in
        let tasks_comb = filter_none (map compile schedule) in
        let tasks_regs = filter_none (map compile regs) in
        let tasks_seq = (map compile_mem_update mems) @ (map compile_reg_update regs) in

        (* reset *)
        log "compile reset";
        let resets = filter_none (List.map compile_reset regs) in

        log "ports";
        (* list of input ports *)
        let in_ports = 
            List.map (fun signal ->
                (List.hd (names signal)), UidMap.find (uid signal) data_map) 
                (Circuit.inputs circuit)
        in

        (* list of output ports *)
        let out_ports = 
            List.map (fun signal ->
                (List.hd (names signal)), (UidMap.find (uid signal) data_map)) 
                (Circuit.outputs circuit)
        in

        let out_ports_cur = List.map (fun (n,p) -> (n,ref !p)) out_ports in
        let task_out_ports_cur = 
          (fun () -> List.iter2 (fun (_,pc) (_,pn) -> pc := !pn) out_ports_cur out_ports) 
        in

        (* List of internal ports *)
        let internal_ports = 
            List.concat (
                List.map (fun signal ->
                    (List.map (fun name -> name,UidMap.find (uid signal) data_map) (names signal)))
                    internal_ports)
        in 

        log "done";

        let sim_lookup_signal uid = UidMap.find uid data_map in
        let sim_lookup_reg uid = UidMap.find uid reg_map in
        let sim_lookup_memory uid = UidMap.find uid mem_map in

        (* simulator structure *)
        let task tasks = fun () -> (List.iter (fun f -> f()) tasks) in 
        {
            sim_in_ports = in_ports;
            sim_out_ports = out_ports_cur;
            sim_out_ports_next = out_ports;
            sim_internal_ports = internal_ports;
            sim_cycle_check = task tasks_check;
            sim_cycle_comb0 = task (tasks_comb @ tasks_regs @ [task_out_ports_cur]);
            sim_cycle_seq = task tasks_seq;
            sim_cycle_comb1 = task tasks_comb;
            sim_reset = task resets;
            sim_lookup_signal;
            sim_lookup_reg;
            sim_lookup_memory;
        }

    exception Sim_comparison_failure of int * string * string * string

    let combine_strict s0 s1 = 
        let ip0,ip1 = List.sort compare s0.sim_in_ports,
                      List.sort compare s1.sim_in_ports in
        let op0,op1 = List.sort compare s0.sim_out_ports,
                      List.sort compare s1.sim_out_ports in
        let cycle_no = ref 0 in

        (* copy input data *)
        let copy_inputs () = List.iter2 (fun (_,d0) (_,d1) -> d1 := !d0) ip0 ip1 in

        (* compare results *)
        let compare_results () = 
            List.iter2 (fun (n,d0) (_,d1) -> 
              if !d0 <> !d1 then
                raise (Sim_comparison_failure(!cycle_no, n, 
                                              Bits.to_string !d0, 
                                              Bits.to_string !d1))
            ) op0 op1 
        in
        let incr_cycle () = incr cycle_no in

        let cycle_check () = s0.sim_cycle_check(); copy_inputs(); s1.sim_cycle_check() in
        let cycle_comb0 () = s0.sim_cycle_comb0(); s1.sim_cycle_comb0() in
        let cycle_seq ()   = s0.sim_cycle_seq(); s1.sim_cycle_seq() in
        let cycle_comb1 () = s0.sim_cycle_comb1(); s1.sim_cycle_comb1(); 
                             compare_results (); incr_cycle() in
        let reset () = s0.sim_reset(); s1.sim_reset() in

        { s0 with
            sim_in_ports = ip0;
            sim_out_ports = op0;
            sim_out_ports_next = op0;
            sim_internal_ports = [];
            sim_reset = reset;
            sim_cycle_check = cycle_check;
            sim_cycle_comb0 = cycle_comb0;
            sim_cycle_seq = cycle_seq;
            sim_cycle_comb1 = cycle_comb1;
        }

    let combine_relaxed s0 s1 = 
      let ip0 = s0.sim_in_ports in
      let ip1 = s1.sim_in_ports in
      let op0 = s0.sim_out_ports in
      let op1 = s1.sim_out_ports in

      let module S = Set.Make(String) in
      let si = S.elements (List.fold_left (fun s (n,_) -> S.add n s) S.empty (ip0 @ ip1)) in
      let so = S.elements (List.fold_left (fun s (n,_) -> S.add n s) S.empty (op0 @ op1)) in
      
      let try_find n s = try Some(List.assoc n s) with _ -> None in

      let inputs, copy_inputs =
        let l = List.map 
          (fun n ->
            match try_find n ip0, try_find n ip1 with
            | Some(x), Some(y) -> (n,x), (fun () -> y := !x)
            | Some(x), None -> (n,x), (fun () -> ())
            | None, Some(y) -> (n,y), (fun () -> ())
            | _ -> failwith ("input port not found: " ^ n)
          ) si
        in
        List.map fst l, List.map snd l
      in

      let cycle_no = ref 0 in
      let check n x y = 
        if !x <> !y then
          raise (Sim_comparison_failure(!cycle_no, n, 
                        Bits.to_string !x, 
                        Bits.to_string !y))
      in
      let outputs, check_outputs = 
        let l = List.map 
          (fun n ->
            match try_find n op0, try_find n op1 with
            | Some(x), Some(y) -> (n,x), (fun () -> check n x y)
            | Some(x), None -> (n,x), (fun () -> ())
            | None, Some(y) -> (n,y), (fun () -> ())
            | _ -> failwith ("output port not found: " ^ n)
          ) so
        in
        List.map fst l, List.map snd l
      in

      let copy_inputs () = List.iter (fun f -> f()) copy_inputs in
      let check_outputs () = List.iter (fun f -> f()) check_outputs in
      let incr_cycle ()  = incr cycle_no in

      let cycle_check () = s0.sim_cycle_check(); copy_inputs (); s1.sim_cycle_check() in
      let cycle_comb0 () = s0.sim_cycle_comb0(); s1.sim_cycle_comb0() in
      let cycle_seq ()   = s0.sim_cycle_seq(); s1.sim_cycle_seq() in
      let cycle_comb1 () = s0.sim_cycle_comb1(); s1.sim_cycle_comb1();
                           check_outputs(); incr_cycle () in
      let reset () = s0.sim_reset(); s1.sim_reset() in
      { s0 with
        sim_in_ports = inputs;
        sim_out_ports = outputs;
        sim_out_ports_next = outputs;
        sim_internal_ports = [];
        sim_reset = reset;
        sim_cycle_check = cycle_check;
        sim_cycle_comb0 = cycle_comb0;
        sim_cycle_seq = cycle_seq;
        sim_cycle_comb1 = cycle_comb1;
      }

    module InstOps = struct

        module OpMap = Map.Make(String)

        type add_inst = string -> run_inst -> signal array -> int array -> 
            signal array

        let make () = 
            let op_map = ref OpMap.empty in

            let get_op name = 
                try Some(OpMap.find name !op_map)
                with _ -> None
            in

            let mk_op name f = 
                match get_op name with
                | Some(x) -> failwith ("sim op " ^ name ^ " already exists")
                | None -> begin
                    op_map := OpMap.add name f !op_map;
                    (fun inp outp ->
                        let a n f = Array.to_list (Array.init n f) in
                        let soi = string_of_int in
                        let n, m = Array.length inp, Array.length outp in
                        let x = Signal.Instantiation.inst name [] 
                            (a n (fun i -> "i" ^ soi i, inp.(i)))
                            (a m (fun i -> "o" ^ soi i, outp.(i)))
                        in
                        Array.init m (fun i -> x#o ("o" ^ soi i))
                    )
                end
            in
            get_op, mk_op

        let combine g0 g1 = 
            (fun s ->
                match g0 s with
                | None -> g1 s
                | _ as x -> x)

        (* simulation of floating point *)
        module Real(P : sig val mk : add_inst end) = struct

            let op2 op name =
                P.mk name (fun i a ->
                    match a with
                    | [x;y] when Bits.width x = 32 && Bits.width y = 32 -> 
                        (* float *)
                        let x,y = Bits.to_int32 x,Bits.to_int32 y in
                        let x,y = Int32.float_of_bits x,Int32.float_of_bits y in
                        [ Bits.consti32 32 (Int32.bits_of_float (op x y)) ]

                    | [x;y] when Bits.width x = 64 && Bits.width y = 64 -> 
                        (* double *)
                        let x,y = Bits.to_int64 x,Bits.to_int64 y in
                        let x,y = Int64.float_of_bits x,Int64.float_of_bits y in
                        [ Bits.consti64 64 (Int64.bits_of_float (op x y)) ]

                    | _ -> failwith "invalid float op")

            let op1 op name =
                P.mk name (fun i a ->
                    match a with
                    | [x] when Bits.width x = 32 -> 
                        (* float *)
                        let x = Bits.to_int32 x in
                        let x = Int32.float_of_bits x in
                        [ Bits.consti32 32 (Int32.bits_of_float (op x)) ]

                    | [x] when Bits.width x = 64 -> 
                        (* double *)
                        let x = Bits.to_int64 x in
                        let x = Int64.float_of_bits x in
                        [ Bits.consti64 64 (Int64.bits_of_float (op x)) ]

                    | _ -> failwith "invalid float op")
        
            module type Real = sig
                val (+:) : signal -> signal -> signal
                val (-:) : signal -> signal -> signal
                val ( *: ) : signal -> signal -> signal
                val (/:) : signal -> signal -> signal
                val (%:) : signal -> signal -> signal
                val ( **: ) : signal -> signal -> signal
                val exp : signal -> signal
                val log : signal -> signal
                val log10 : signal -> signal
                val cos : signal -> signal
                val sin : signal -> signal
                val tan : signal -> signal
                val acos : signal -> signal
                val asin : signal -> signal
                val atan : signal -> signal
                val atan2 : signal -> signal -> signal
                val cosh : signal -> signal
                val sinh : signal -> signal
                val tanh : signal -> signal
                val ceil : signal -> signal
                val floor : signal -> signal
                val abs : signal -> signal
            end

            module F(P : sig val bits : int end) = struct

                let n s = "float" ^ string_of_int P.bits ^ s

                let op2 op name = 
                    let op = op2 op (n name) in
                    (fun a b -> (op [|a;b|] [|P.bits|]).(0))

                let op1 op name = 
                    let op = op1 op (n name) in
                    (fun a -> (op [|a|] [|P.bits|]).(0))

                let (+:) = op2 (+.) "add"
                let (-:) = op2 (-.) "sub"
                let ( *: ) = op2 ( *. ) "mul"
                let (/:) = op2 ( /. ) "div"
                let (%:) = op2 mod_float "mod"
                let ( **: ) = op2 ( ** ) "pow"
                let exp = op1 exp "exp"
                let log = op1 log "log"
                let log10 = op1 log10 "log10"
                let cos = op1 cos "cos"
                let sin = op1 sin "sin"
                let tan = op1 tan "tan"
                let acos = op1 acos "acos"
                let asin = op1 asin "asin"
                let atan = op1 atan "atan"
                let atan2 = op2 atan2 "atan2"
                let cosh = op1 cosh "cosh"
                let sinh = op1 sinh "sinh"
                let tanh = op1 tanh "tanh"
                let ceil = op1 ceil "ceil"
                let floor = op1 floor "floor"
                let abs = op1 abs_float "abs"

            end

            module Float = F(struct let bits=32 end)
            module Double = F(struct let bits=64 end)

        end

    end


end

module Sim_obj_if = struct

  module type S = sig
    type t
    type i = <
      i : int -> unit;
      i32 : int32 -> unit;
      i64 : int64 -> unit;
      d : string -> unit;
      hu : string -> unit;
      hs : string -> unit;
      c : string -> unit;
      ibl : int list -> unit;
      bits : t ref;
    >
    val input : t ref -> i
    type o = <
      i : int;
      s : int;
      i32 : int32;
      s32 : int32;
      i64 : int64;
      s64 : int64;
      str : string;
      bits : t
    >
    val output : t ref -> o
  end

  module Make(B : Comb.S) = struct
    type t = B.t
    type i = < 
      i : int -> unit;
      i32 : int32 -> unit;
      i64 : int64 -> unit;
      d : string -> unit;
      hu : string -> unit;
      hs : string -> unit;
      c : string -> unit;
      ibl : int list -> unit;
      bits : t ref;
    >
    let input s = 
      let w = B.width !s in
      object
        method i v = s := B.consti w v
        method i32 v = s := B.consti32 w v
        method i64 v = s := B.consti64 w v
        method d v = s := B.constd w v
        method hu v = s := B.consthu w v
        method hs v = s := B.consths w v
        method c v = s := B.const v
        method ibl v = s := B.constibl v
        method bits = s
      end
    type o = <
      i : int;
      s : int;
      i32 : int32;
      s32 : int32;
      i64 : int64;
      s64 : int64;
      str : string;
      bits : t
    >
    let output s = 
      object
        method i = B.to_int !s
        method s = B.to_sint !s
        method i32 = B.to_int32 !s
        method s32 = B.to_sint32 !s
        method i64 = B.to_int64 !s
        method s64 = B.to_sint64 !s
        method str = B.to_bstr !s
        method bits = !s
      end
  end 

end

(** Wraps a simulator and allows it to be driven interactively from the command line *)
module Interactive(B : Comb.S) =
struct

    open Printf

    type command = 
        | Reset
        | Cycle of int 
        | Set of string * string
        | Print of string
        | Info
        | Quit

    let run chan sim = 
        let () = printf "? for options\n" in

        let rec get_command() = 
            let line = 
                if chan <> stdin then input_line chan
                else (printf "> %!"; read_line ())
            in
            let split str =
                let rec split l c chars = 
                    match chars with
                    | [] -> c :: l
                    | h :: t ->
                        if h = '#' then
                            split ("#" :: l) "" t
                        else if h = ' ' then
                            split (c :: l) "" t
                        else
                            split l (c ^ Char.Ascii.escape_char h) t
                in
                List.rev 
                    (List.filter ((<>) "") 
                                 (split [] "" 
                                        (Utils.list_of_string str)))
            in
            match split line with
            | [] -> 
                get_command ()
            | [ "!" ] -> Reset
            | [ "!quit" ] -> Quit
            | [ "#" ] -> Cycle 1
            | [ "?" ] -> Info
            | "#" :: n :: [] -> (try Cycle (int_of_string n) with _ -> get_command ())
            | var :: [] -> Print var
            | var :: value :: [] -> Set (var, value)
            | _ -> printf "Couldn't parse command"; get_command ()
        in

        let info () = 
          let h v = Utils.(hstr_of_bstr Unsigned B.(to_bstr v)) in
          printf "Commands:\n";
          printf "> ?         show state\n";
          printf "> !         reset\n";
          printf "> !quit     quit\n";
          printf "> #         cycle\n";
          printf "> #n        n cycles\n";
          printf "> data      display value of port 'data'\n";
          printf "> data d13  set value of port 'data' to decimal 13 (b1101 for binary, h3a for hex)\n\n";
          printf "Input ports:\n";
          List.iter (fun (n,v) -> printf " %s[%i] = h%s\n" n (B.width !v) (h !v)) (Api.in_ports sim);
          printf "Output ports:\n";
          List.iter (fun (n,v) -> printf " %s[%i] = h%s\n" n (B.width !v) (h !v)) (Api.out_ports sim)
        in

        let get_interactive_input ()=
            let quit = ref false in
            let get_var name = 
                try Some(Api.in_port sim name)
                with _ -> 
                    try Some(Api.out_port sim name)
                    with _ -> None
            in
            let rec get_interactive_input () = 
                let command = try get_command() with _ -> Quit in
                (match command with
                | Reset -> 
                    (printf "Resetting simulator\n";
                    Api.reset sim)
                | Cycle n -> 
                    (printf "Cycling simulator %d times\n" n;
                    for i=0 to n-1 do Api.cycle sim done)
                | Info -> info()
                | Set(var, value) -> 
                    (match get_var var with 
                     | Some(x) -> 
                        (try x := B.const ((string_of_int (B.width !x)) ^ "'" ^ value)
                        with _ -> printf "Couldnt set value %s\n" value)
                     | None -> printf "Couldn't find %s\n" var)
                | Print(var) -> 
                    (match get_var var with 
                     | Some(x) -> 
                        (let b = B.to_bstr !x in
                        printf "b%s / d%s / h%s\n" b
                            (Big_int.string_of_big_int
                                (Bits.Ext.Utils_ext.big_int_of_bstr b))
                            (Utils.hstr_of_bstr Utils.Unsigned b))
                     | None -> printf "Couldn't find %s\n" var)
                | Quit -> printf "Goodbye!\n"; quit := true);
                if not !quit then get_interactive_input ()
            in
            get_interactive_input ()
        in
        get_interactive_input ()

end


(* Binary IO dumping - Only supports the Nativeint Bigarray type *)
module BinaryIO =
struct

    type t = Bits.Ext.Comb.BigarraybitsNativeint.t

    type binary_io =
        {
            read_inputs : bool;
            read_inputs_chan : Pervasives.in_channel;
            write_inputs : bool;
            write_inputs_chan : Pervasives.out_channel;
            compare_outputs : bool;
            compare_outputs_chan : Pervasives.in_channel;
            compare_error_fn : string -> t -> t -> unit;
            write_outputs : bool;
            write_outputs_chan : Pervasives.out_channel;
        }

    type mapped_data = 
        {
            data_offset : int;
            field_size : int;
            data : (string * (int * int)) list;
            a : Bits.Ext.Utils_ext.bani;
        }

    let pbits = Utils.platform_bits 
    
    let setup_read chan = 
        let ti x = Nativeint.to_int x in
        let ba = Bigarray.Array1.map_file 
            (Unix.descr_of_in_channel chan) 
            Bigarray.nativeint Bigarray.c_layout false (-1)
        in
        (* read header *)
        let nelems = ti ba.{0} in
        let read_elem offset =
            let width = ti ba.{offset+0} in
            let name_len = ti ba.{offset+1} in
            let rec read_name str len off = 
                if len = 0 then str, off
                else
                    let c = Char.Ascii.escape_char (Char.of_byte (ti ba.{off})) in
                    read_name (str ^ c) (len-1) (off+1)
            in
            let name, off = read_name "" name_len (offset+2) in
            (name, width), off
        in
        let rec read_elements elems i offset = 
            if i = nelems then List.rev elems, offset
            else
                let elem, offset = read_elem offset in
                read_elements 
                    (elem :: elems)
                    (i+1) offset
        in
        let elements, offset = read_elements [] 0 2 in
        let elements, field_size = List.fold_left (fun (e,a) (n,w) ->
                let w = (w + pbits - 1) / pbits in
                (n, (w, a))::e, a + w
            ) ([],0) elements 
        in
        let elements = List.rev elements in
        List.iter (fun (n,(w,a)) -> Printf.printf "%s[%d] at %d\n" n w a) elements;
        {
            data_offset = offset;
            field_size = field_size;
            data = elements;
            a = ba;
        }

    let width = Bits.Ext.Comb.BigarraybitsNativeint.width

    let read chan ports = 
        let elems = setup_read chan in
        let data = List.map (fun (n,d) -> 
                let w,o = List.assoc n elems.data in 
                if width !d <> w then failwith "Width mismatch";
                n,(d,w,o)
            ) ports
        in
        let offset = ref elems.data_offset in
        (fun () -> 
            List.iter (fun (n,(d,w,o)) ->
                let o = !offset + o in
                let d = fst !d in
                for i=0 to w-1 do
                    d.{i} <- elems.a.{o+i}
                done
            ) data;
            offset := !offset + elems.field_size;
        )

    let compare chan ports err_fn = 
        let elems = setup_read chan in
        let data = List.map (fun (n,d) -> 
                let w,o = List.assoc n elems.data in 
                if width !d <> w then failwith "Width mismatch";
                n,(d,w,o)
            ) ports
        in
        let offset = ref elems.data_offset in
        (fun () -> 
            List.iter (fun (n,(d,w,o)) ->
                let o = !offset + o in
                let error = ref false in
                let d = fst !d in
                for i=0 to w-1 do
                    if d.{i} <> elems.a.{o+i} then error := true
                done;
                if !error then
                begin
                    (* if there was an error, then copy the signal and 
                       call the error function *)
                    let q = Bigarray.Array1.create 
                        Bigarray.nativeint Bigarray.c_layout w 
                    in
                    for i=0 to w-1 do
                        q.{i} <- elems.a.{o+i}
                    done;
                    err_fn n (d,w) (q,w) 
                end
            ) data;
            offset := !offset + elems.field_size;
        )

    let setup_write chan ports =
        (* this is probably bleedin' slow *)
        let wr x = Marshal.to_channel chan x [ Marshal.No_sharing ] in
        let wri x = wr (Nativeint.of_int x) in
        let wrc x = wri (Char.to_int x) in
        wri (List.length ports);
        List.map (fun (n,d) ->
            wri (width !d);
            wri (String.length n);
            String.iter wrc n;
            d, (width !d + pbits - 1) / pbits
        ) ports
    
    let write chan ports = 
        let ports = setup_write chan ports in
        (fun () ->
            List.iter (fun (d,o) ->
                let d = fst !d in
                for i=0 to o-1 do
                    Marshal.to_channel chan d.{i} [ Marshal.No_sharing ]
                done
            ) ports
        )

    open Api

    (* wrap a simulator and read/write binary dump files *)
    let wrap' cfg in_ports out_ports =
      let id () = () in
      let f0 = if cfg.read_inputs then read cfg.read_inputs_chan in_ports else id in
      let f1 = if cfg.write_inputs then write cfg.write_inputs_chan in_ports else id in
      let f2 = 
        if cfg.compare_outputs then 
          compare cfg.compare_outputs_chan out_ports cfg.compare_error_fn 
        else id
      in
      let f3 = 
        if cfg.write_outputs then write cfg.write_outputs_chan out_ports 
        else id
      in
      (fun () -> f0(); f1 ()),
      (fun () -> f2(); f3 ())

    let errfn name _ _ = failwith ("Mismatch: " ^ name)

    type cyclesim = t Api.cyclesim

    let wrap ?(rdin=None) ?(wrin=None) ?(cmpout=None) ?(cmpfn=errfn) ?(wrout=None) sim = 
        let ui i = match i with Some(x) -> x | _ -> stdin in
        let uo o = match o with Some(x) -> x | _ -> stdout in
        let pre, post =
          wrap' 
            {
              read_inputs = rdin <> None;
              read_inputs_chan = ui rdin;
              write_inputs = wrin <> None;
              write_inputs_chan = uo wrin;
              compare_outputs = cmpout <> None;
              compare_outputs_chan = ui cmpout;
              compare_error_fn = cmpfn;
              write_outputs = wrout <> None;
              write_outputs_chan = uo wrout;
            }
            sim.sim_in_ports sim.sim_out_ports
        in
        { sim with
          sim_reset = (fun () -> pre(); Api.reset sim; post());
          sim_cycle_check = (fun () -> pre(); Api.cycle_check sim);
          sim_cycle_comb1 = (fun () -> Api.cycle_comb1 sim; post());
        }

end



