(* 
 * hardcaml - hardware design in OCaml
 *
 *   (c) 2014 MicroJamJar Ltd
 *
 * Author(s): andy.ray@ujamjar.com
 * Description: 
 *
 *)

open HardCaml
open HardCamlX
open Llvm
open Signal.Types
open Cyclesim.Api
module Sc = Signal.Comb
module Cs = Cyclesim
module Ee = Llvm_executionengine.ExecutionEngine
module Gv = Llvm_executionengine.GenericValue

module type T = 
sig
    type cyclesim = Bits_ext.Comb.BigarraybitsNativeint.t Cyclesim.Api.cyclesim

    (** [make circuit] construct a simulator from [Circuit.t] *)
    val make : Circuit.t -> cyclesim

    (** write simulator to bitcode file *)
    val write : string -> Circuit.t -> unit

    (** [load name] loads the bit code file called [name] *)
    val load : string -> cyclesim
end

module type S = 
sig
    type t
    type cyclesim = t Cyclesim.Api.cyclesim

    (** construct a simulator from a circuit *)
    val make : Circuit.t -> cyclesim

    (** write simulator to bitcode file *)
    val write : string -> Circuit.t -> unit

    (** load simulator from bitcode file *)
    val load : string -> cyclesim
end

module V1 =
struct

    module UidSet = Signal.Types.UidSet
    module UidMap = Signal.Types.UidMap

    external llvm_get_ptr : nativeint -> int -> Bits_ext.Utils_ext.bani =
        "llvmsim_get_ptr"

    let platform_bits = Utils.platform_bits

    type llvmsim_setup = 
        {
            name : string;
            context : llcontext;
            modl : llmodule;
            builder : llbuilder;
            regs : signal list;
            consts : signal list;
            inputs : signal list;
            outputs : signal list;
            remaining : signal list;
            schedule : signal list;
            input_globals : (llvalue*int*int) UidMap.t;
            output_globals : (llvalue*int*int) UidMap.t;
            reg_globals : (llvalue*int*int) UidMap.t;
        }

    type llvmsim = 
        {
            llvm_in_ports : (string * Bits_ext.Comb.BigarraybitsNativeint.t) list;
            llvm_out_ports : (string * Bits_ext.Comb.BigarraybitsNativeint.t) list;
            llvm_reset : unit->unit;
            llvm_cycle : unit->unit;
        }

    type cyclesim = Bits_ext.Comb.BigarraybitsNativeint.t Cyclesim.Api.cyclesim

    let global_context =
        let gc = ref None in
        (fun () ->
            match !gc with
            | None ->
                let x = global_context() in
                gc := Some(x);
                x
            | Some(x) ->
                x
        )

    (* aligns vector of n bits to next largest multiple of platform_bits *)
    let pbits w = ((w + platform_bits) / platform_bits) * platform_bits 

    let int_type context w = integer_type context w 
    let const_int context w v r = const_int_of_string (int_type context w) v r 
    let create_const context s = const_int context (Sc.width s) (const_value s) 2  
    let create_consti context w i = const_int context w (string_of_int i) 10
    let rec ptr_type n t = 
        if n = 0 then t
        else ptr_type (n-1) (pointer_type t)
    let define_global a b c = 
        let g = define_global a b c in
        (* set_linkage Linkage.Internal g; *)
        g

    let init circuit =
        let name = Circuit.name circuit in
        let get_name s n = try List.nth (names s) n with _ -> ("hardcaml_" ^ Int64.to_string (uid s)) in
        let context = global_context () in
        let modl = create_module context name in
        let builder = builder context in

        (* run the scheduler *)
        let regs, mems, consts, inputs, remaining = Cs.find_elements circuit in
        let outputs = Circuit.outputs circuit in
        let ready = regs @ mems @ inputs @ consts in
        let schedule = Cs.scheduler deps remaining ready in

        (* create globals *)
        let create_globals tname round globals = 
            List.fold_left (fun map s ->
                let w = Sc.width s in
                let w' = if round then pbits w else w in
                let n = tname ^ (get_name s 0) in
                let g = define_global n (create_consti context w' 0) modl in
                UidMap.add (uid s) (g,w,w') map
            ) UidMap.empty globals
        in
        let input_globals = create_globals "input_" true inputs in
        let reg_globals = create_globals "reg_" false regs in
        let output_globals = create_globals "output_" true outputs in
        {
            name = name;
            context = context;
            modl = modl;
            builder = builder;
            regs = regs;
            consts = consts;
            inputs = inputs;
            outputs = outputs;
            remaining = remaining;
            schedule = schedule;
            input_globals = input_globals;
            output_globals = output_globals;
            reg_globals = reg_globals;
        }

    (* create a simple function prototype void fn(void) *)
    let make_void_function_header context modl builder name = 
        let func_type = function_type (void_type context) [||] in
        let func = declare_function name func_type modl in
        let bb = append_block context "entry" func in
        let _ = position_at_end bb builder in
        func 

    let compile_cycle l circuit = 
        let context = l.context in
        let modl = l.modl in
        let builder = l.builder in

        (* create constants *)
        let int_type = int_type context in
        let create_const = create_const context in
        let create_consti = create_consti context in

        (* create cycle function *)
        let cycle = make_void_function_header context modl builder ("cycle_" ^ l.name) in

        (* create a map of global declarations *)
        let load_map n m = 
            UidMap.map (fun (x,w,w') -> 
                let x = build_load x ("load_" ^ n) builder in
                if w <> w' then
                    build_trunc x (int_type w) "load_trunc" builder 
                else
                    x
            ) m in
        let input_globals_loaded = load_map "input" l.input_globals in
        let reg_globals_loaded = load_map "reg" l.reg_globals in
        let all_globals_loaded = UidMap.fold (fun k d m -> UidMap.add k d m)
            input_globals_loaded reg_globals_loaded 
        in

        (* create the internal logic *)
        let compile map signal = 
            let sdep n = List.nth (deps signal) n in
            let instr signal = UidMap.find (uid signal) map in
            let dep n = instr (sdep n) in
            match signal with
            | Signal_empty -> failwith "cant compile empty signal"
            | Signal_const(_,v) -> create_const signal
            | Signal_op(id,op) ->
            begin
                match op with
                | Signal_add -> build_add (dep 0) (dep 1) "add" builder  
                | Signal_sub -> build_sub (dep 0) (dep 1) "sub" builder
                | Signal_mulu -> 
                    let a = build_zext (dep 0) (int_type id.s_width) "zext_mula" builder in
                    let b = build_zext (dep 1) (int_type id.s_width) "zext_mulb" builder in
                    build_mul a b "mulu" builder (* XXX *)
                | Signal_muls -> 
                    let a = build_sext (dep 0) (int_type id.s_width) "sext_mula" builder in
                    let b = build_sext (dep 1) (int_type id.s_width) "sext_mulb" builder in
                    build_mul a b "muls" builder (* XXX *)
                | Signal_and -> build_and (dep 0) (dep 1) "and" builder 
                | Signal_or -> build_or (dep 0) (dep 1) "or" builder
                | Signal_xor -> build_xor (dep 0) (dep 1) "xor" builder
                | Signal_eq -> build_icmp Icmp.Eq (dep 0) (dep 1) "eq" builder
                | Signal_not -> build_not (dep 0) "not" builder
                | Signal_lt -> build_icmp Icmp.Ult (dep 0) (dep 1) "lt" builder
                | Signal_cat -> 
                begin
                    let width = Sc.width signal in
                    let ze s w = build_zext s (int_type w) "cat_ext" builder in
                    let or2 r s n =
                        if n = 0 then
                            ze s width
                        else
                            let f s n = build_shl (ze s width) (create_consti width n) "cat_shl" builder in
                            build_or r (f s n) "cat_or" builder
                    in
                    let sdeps = deps signal in
                    let deps = Utils.mapi (fun i _ -> dep i) sdeps in
                    let d = List.rev (Utils.map2 (fun s d -> Sc.width s, d) sdeps deps) in
                    fst(List.fold_left 
                        (fun (res,sft) (wid,arg) -> or2 res arg sft,sft+wid)
                        (create_consti width 0, 0) d)
                end
                | Signal_mux -> 
                begin
                    (* build the basic blocks *)
                    let sel = dep 0 in
                    let cur_bb = insertion_block builder in
                    (* NOTE: see 'create_entry_block_alloca' if we want to get
                     * these optimised to SSA registers *)
                    let r = build_alloca (int_type (Sc.width signal)) "mux_var" builder in
                    (* let r = build_alloca (int_type (Sc.width signal)) "mux_var"
                        (builder_at context (instr_begin (entry_block cycle)))
                    in *)
                    let bb = 
                        Utils.mapi (fun i s -> 
                            let bb = append_block context "mux_blk" cycle in
                            position_at_end bb builder;
                            ignore (build_store (dep (i+1)) r builder); 
                            bb
                        ) (List.tl (deps signal))
                    in
                    let end_bb = append_block context "mux_end_blk" cycle in
                    List.iter (fun bb ->
                        position_at_end bb builder;
                        ignore (build_br end_bb builder)
                    ) bb;
                    let def,cases = 
                        let l = List.rev bb in
                        List.hd l, List.rev (List.tl l)
                    in
                    position_at_end cur_bb builder;
                    let sw = build_switch sel def (List.length cases) builder in
                    Utils.iteri (fun i bb -> add_case sw (create_consti (Sc.width (sdep 0)) i) bb) cases;
                    position_at_end end_bb builder;
                    build_load r "mux_load" builder
                end
            end
            | Signal_wire(_) -> dep 0 (* wires are effectively removed *)
            | Signal_select(_,h,l) -> 
                let d = dep 0 in
                let w = Sc.width (sdep 0) in
                if w = (h-l+1) then d
                else
                    let sft = create_consti w l in
                    let s = build_lshr d sft "select_shr" builder in
                    build_trunc s (int_type (h-l+1)) "select_trunc" builder
            | Signal_reg(_,r) -> 
                let width = Sc.width signal in
                let src = dep 0 in (* input data *)
                let cur_q = UidMap.find (uid signal) reg_globals_loaded in
                let clr, clr_level, clr_value = 
                    if r.reg_clear <> Sc.empty then
                        instr r.reg_clear, instr r.reg_clear_level, instr r.reg_clear_value
                    else 
                        create_consti 1 0, create_consti 1 1, create_consti width 0
                in
                let clr = build_icmp Icmp.Eq clr clr_level "reg_clr" builder in
                let ena = 
                    if r.reg_enable <> Sc.empty then instr r.reg_enable
                    else create_consti 1 1
                in
                let ena = build_and ena (build_not clr "reg_clr_not" builder) "reg_ena" builder in
                let q = build_select clr clr_value cur_q "reg_clr_update" builder in
                let q = build_select ena src q "reg_ena_update" builder in
                let (d,w,w') = UidMap.find (uid signal) l.reg_globals in
                ignore (build_store q d builder);
                cur_q
            | Signal_mem(_,_,r,m) -> failwith "TODO: LLVM memories"
            | Signal_inst(_) -> failwith "Instantiations are not supported in simulation"
        in

        (* compile expressions *) 
        let expr_map = List.fold_left 
            (fun map s -> UidMap.add (uid s) (compile map s) map) 
            all_globals_loaded (l.consts @ l.schedule @ l.regs) 
        in

        (* copy outputs *)
        let store_globals signals = 
            List.iter (fun s ->
                match s with 
                | Signal_wire(_,d) ->
                    let d = UidMap.find (uid !d) expr_map in
                    let (q,w,w') = UidMap.find (uid s) l.output_globals in
                    let d = 
                        if w <> w' then 
                            build_zext d (int_type w') "store_zext" builder 
                        else 
                            d 
                    in
                    ignore (build_store d q builder)
                | _ -> failwith "Expecting assigned wire or register for store to output"
            ) signals
        in
        store_globals (Circuit.outputs circuit);
        
        (* return value *)
        let _ = build_ret_void builder in

        (* validate *)
        let _ = Llvm_analysis.assert_valid_function cycle in
        
        (* return function *)
        cycle

    let compile_reset l circuit = 
        (* create cycle function *)
        let reset = make_void_function_header l.context l.modl l.builder ("reset_" ^ l.name) in
        
        List.iter (fun s ->
            match s with
            | Signal_reg(_,r) ->
                let v = try create_const l.context r.reg_reset_value 
                        with _ -> failwith "register reset values must be constant in the llvm code generator"
                in
                let (g,w,w') = UidMap.find (uid s) l.reg_globals in
                ignore (build_store v g l.builder)
            | _ -> failwith "Expecting register"
        ) l.regs;

        (* return value *)
        let _ = build_ret_void l.builder in

        (* validate *)
        let _ = Llvm_analysis.assert_valid_function reset in

        (* return function *)
        reset

    let compile_circuit_info l circuit io signals data_globals = 
        let mod_name = Circuit.name circuit in
        let name signal = List.hd (names signal) in
        let find signal =
            (* return llvm global, width, rounded width + name *)
            let d,w,w' = UidMap.find (uid signal) data_globals in
            d,w,w',name signal
        in
        let make_function_header l rtype atype name = 
            let func_type = function_type rtype atype in
            let func = declare_function name func_type l.modl in
            let bb = append_block l.context "entry" func in
            let _ = position_at_end bb l.builder in
            func 
        in
        (* create globals for string names *)
        let name_globals = 
            List.map (fun s -> 
                let name = name s in
                define_global ("name_" ^ io ^ name) (const_stringz l.context name) l.modl 
            ) signals
        in

        let get fname op rtype atype values nil = 
            let get_int = make_function_header l 
                rtype atype (fname ^ mod_name)
            in
            let arg = (params get_int).(0) in
            let d,_ = 
                List.fold_left (fun (d,i) v ->
                    let cmp = build_icmp Icmp.Eq 
                        (create_consti l.context 32 i) 
                        arg "cmp" l.builder 
                    in 
                    let s = build_select cmp 
                        (op get_int v) d "sel" l.builder 
                    in
                    (s, i+1)
                ) (nil, 0) values
            in
            ignore (build_ret d l.builder)
        in
        (* get width *)
        let int32 = int_type l.context 32 in
        let int64 = int_type l.context 64 in
        let int8 = int_type l.context 8 in
        let zero32 = create_consti l.context 32 0 in
        let zero64 = create_consti l.context 64 0 in
        let zero8 = create_consti l.context 8 0 in
        get ("get_width_" ^ io) 
            (fun _ d -> d) int32 [|int32|]
            (List.map (fun s -> let d,w,w',n = find s in 
                create_consti l.context 32 w) signals)
            zero32;

        (* get name *)
        let name_gep g i = build_gep g [| zero32; i |] "gep" l.builder in
        get ("get_name_" ^ io)
            (fun f d -> build_load (name_gep d (params f).(1)) "" l.builder)
            int8 [|int32;int32|]
            name_globals
            zero8; 
        
        (* get pointer *)
        get ("get_ptr_" ^ io) 
            (fun _ d ->
                let bcast = build_bitcast d (ptr_type 1 (int64)) "bitcast" l.builder in
                let gep = build_gep bcast [| zero64 |] "gep" l.builder in
                let p2i = build_ptrtoint gep (int64) "get_p2i" l.builder in
                p2i)
            int64 [|int32|]
            (List.map (fun s -> let d,w,w',n = find s in d) signals)
            zero64;

        ()

    let compile_module circuit = 
        let llvm = init circuit in 
        let _ = compile_cycle llvm circuit in
        let _ = compile_reset llvm circuit in
        let _ = compile_circuit_info llvm circuit "input_" llvm.inputs llvm.input_globals in
        let _ = compile_circuit_info llvm circuit "output_" llvm.outputs llvm.output_globals in
        llvm

    let write path circuit = 
        let llvm = compile_module circuit in
        (* write bitcode *)
        let fname = Filename.concat path (llvm.name ^ ".bc") in
        if not (Llvm_bitwriter.write_bitcode_file llvm.modl fname) then
            failwith "Failed to write bitcode"

    module Ee = Llvm_executionengine.ExecutionEngine
    module Gv = Llvm_executionengine.GenericValue

    let _ = 
        ignore (Llvm_executionengine.initialize_native_target ())

    let lookup_function name jit = 
        match Ee.find_function name jit with
        | Some(x) -> x
        | None -> failwith ("Couldn't find function " ^ name ^ " in JIT")

    let query_ports context mod_name io jit = 
        let int32 = int_type context 32 in
        let width = lookup_function ("get_width_" ^ io ^ mod_name) jit in
        let name = lookup_function ("get_name_" ^ io ^ mod_name) jit in
        let ptr = lookup_function ("get_ptr_" ^ io ^ mod_name) jit in
        let rec get_widths i l = 
            let v = Gv.as_int (Ee.run_function width [| Gv.of_int int32 i |] jit) in
            if v <> 0 then get_widths (i+1) (v::l)
            else List.rev l
        in
        let widths = get_widths 0 [] in
        (*let count = List.length widths in
        Printf.printf "Total %s = %d\n" io count;*)
        let rec build_name i0 i1 str = 
            let v = Gv.as_int (Ee.run_function name [| Gv.of_int int32 i0; 
                                                       Gv.of_int int32 i1 |] jit)
            in
            if v <> 0 then build_name i0 (i1+1) (str ^ (Char.escaped (Char.chr v)))
            else str
        in
        let ports = Utils.mapi (fun i width ->
                let name = build_name i 0 "" in
                let ptr = Gv.as_nativeint (Ee.run_function ptr [| Gv.of_int int32 i |] jit) in
                name, (llvm_get_ptr ptr width, width)
            ) widths
        in
        (*List.iter (fun (n,(_,w)) -> Printf.printf "%s[%d]\n" n w) ports;*)
        ports

    open Cyclesim.Api

    let to_cyclesim sim = 
        let map s = List.map (fun (s,d) -> s, ref d) s in
        {
            sim_cycle = sim.llvm_cycle;
            sim_reset = sim.llvm_reset;
            sim_internal_ports = [];
            sim_in_ports = map sim.llvm_in_ports;
            sim_out_ports = map sim.llvm_out_ports;
        }

    let make circuit = 
        let llvm = compile_module circuit in 

        (*
        let mp = ModuleProvider.create llvm.modl in
        let jit = Ee.create_jit mp in
        *)
        let optlevel = 2 in
        let jit = Ee.create_jit llvm.modl optlevel in

        let cycle = lookup_function ("cycle_" ^ llvm.name) jit in
        let reset = lookup_function ("reset_" ^ llvm.name) jit in
        
        let in_ports = query_ports llvm.context llvm.name "input_" jit in
        let out_ports = query_ports llvm.context llvm.name "output_" jit in

        let cycle = (fun () -> ignore (Ee.run_function cycle [||] jit)) in
        let reset = (fun () -> ignore (Ee.run_function reset [||] jit)) in

        to_cyclesim
            {
                llvm_cycle = cycle;
                llvm_reset = reset;
                llvm_in_ports = in_ports;
                llvm_out_ports = out_ports;
            }

    (* load bitcode simulation *)
    let load path = 
        let name = Bits_ext.Utils_ext.filebase path in
        let context = global_context () in
        let membuf = MemoryBuffer.of_file path in
        let modl = Llvm_bitreader.parse_bitcode context membuf in
        (*let mp = ModuleProvider.create modl in
        let jit = Ee.create_jit mp in *)
        let optlevel = 2 in
        let jit= Ee.create_jit modl optlevel in
        (* need to get the circuit information from the bit code *)
        let cycle = lookup_function ("cycle_" ^ name) jit in
        let reset = lookup_function ("reset_" ^ name) jit in
        
        let in_ports = query_ports context name "input_" jit in
        let out_ports = query_ports context name "output_" jit in

        let cycle = (fun () -> ignore (Ee.run_function cycle [||] jit)) in
        let reset = (fun () -> ignore (Ee.run_function reset [||] jit)) in

        to_cyclesim
            {
                llvm_cycle = cycle;
                llvm_reset = reset;
                llvm_in_ports = in_ports;
                llvm_out_ports = out_ports;
            }

end

(* V2 *)

module LlvmUtils = 
struct

    let (>>) f g x = g (f x)
    let (<<) f g x = f (g x)
    let (|>) x f = f x

    let global_context =
        let gc = ref None in
        (fun () ->
            match !gc with
            | None ->
                let x = global_context() in
                gc := Some(x);
                x
            | Some(x) ->
                x)

    let platform_bits = Utils.platform_bits
    let pbits w = ((w + platform_bits) / platform_bits) * platform_bits 

    (* types and constants *)
    let int_type width = integer_type (global_context()) width
    let const_int width value radix = 
        const_int_of_string (int_type width) value radix
    let const_of_signal s = const_int (Sc.width s) (const_value s) 2 
    let const_int width value = const_int width (string_of_int value) 10 
    let const_string str = const_stringz (global_context()) str
    let int64, int32, int8 = int_type 64, int_type 32, int_type 8
    let zero64, zero32, zero8 = const_int 64 0, const_int 32 0, const_int 8 0
    let rec ptr_type n t = 
        if n = 0 then t
        else ptr_type (n-1) (pointer_type t)
    let void = void_type (global_context())

    let build_uresize d width_from width_to name builder = 
        if width_from = width_to then d
        else if width_from < width_to then
            build_zext d (int_type width_to) name builder
        else
            build_trunc d (int_type width_to) name builder

    (* switch *)
    let make_switch width_sel' sel cases builder = 
        let def, cases = 
            let c = List.rev cases in
            List.hd c, List.rev (List.tl c)
        in
        let width_sel = max 32 (width_sel' + 1) in
        let sel = build_uresize sel width_sel' width_sel "switch" builder in 
        let switch = build_switch sel def (List.length cases) builder in
        Utils.iteri (fun i bb ->
            add_case switch (const_int width_sel i) bb)
            cases

    (* builders *)
    let builder() = builder (global_context())
    let append_block name f = append_block (global_context()) name f

    (* build a function with the given argument and return types *)
    let make_function modl name returns args f = 
        let builder = builder () in
        let fn_type = function_type returns args in
        let fn = declare_function name fn_type modl in
        let bb = append_block "entry" fn in
        position_at_end bb builder;
        f fn builder

    let name n s = n ^ "_" ^ Int64.to_string (uid s) ^ "_s"
    let memsize = function Signal_mem(_,_,_,m) -> m.mem_size | _ -> 0 

    (* select 1st 'n' elements from list *)
    let split n l = 
        let rec split n a l =
            if n=0 then a, l
            else
                match l with
                | [] -> a, l
                | b::c -> split (n-1) (b::a) c
        in
        let a,b = split n [] l in
        List.rev a, b

end

module type LlvmGlobals_S = 
sig

    type global_type = G_Port | G_Internal | G_Reg | G_Mem
    type global = 
        {
            width : int;
            rnd_width : int;
            cur : llvalue;
            next : llvalue;
            typ : global_type;
        }

    type global_simple = bool -> int -> uid -> global
    type global_reg = int -> uid -> global
    type global_mem = int -> int -> uid -> global
    type global_fns = global_simple * global_reg * global_mem

    val globals : llmodule -> global UidMap.t ref * global_fns

    type load_simple = bool -> signal -> llvalue
    type load_reg = signal -> llvalue
    type load_mem = llvalue -> signal -> llvalue
    type load_fns = load_simple * load_reg * load_mem

    val load : llvalue -> global_fns -> load_fns

    type store_simple = llvalue -> bool -> signal -> unit
    type store_reg = llvalue -> signal -> unit
    type store_mem = llvalue -> signal -> unit
    type store_fns = store_simple * store_reg * store_mem

    val store : llbuilder -> global_fns -> store_fns

    type update_reg = signal -> unit
    type update_mem = llvalue -> signal -> unit
    type update_fns = update_reg * update_mem

    val update : llbuilder -> global_fns -> update_fns

    val load_signal : load_fns -> llvalue UidMap.t -> signal -> llvalue

end

module LlvmGlobals : LlvmGlobals_S = 
struct

    open LlvmUtils

    (* Dynamic generation of globals 
     *
     * (1) Input/Output Signals (IOs)
     * (2) Registers (REGs)
     * (3) Memories  (MEMs)
     * (4) Temporaries between sub-cycles (TEMPs)
     * (5) Internally monitored signals
     *
     * Globals have different types.  
     *
     * IOs - statically created 'n' bit signals
     * REGs - 2 element arrays (cur and next values)
     * MEMs - 'n' element arrays + next value
     * TEMPs - dynamically stored 'n' bit signals
     *
     * NOTE: With output signals we must write them out.  We can do so as we
     * calculate them (in particular because they are always wires).  To 
     * support wires we could do much the same thing - effectively create 
     * output wires for them.  Hmm interesting.
     *
     *)

    (* one of the global value types *)

    type global_type = G_Port | G_Internal | G_Reg | G_Mem
    type global = 
        {
            width : int;
            rnd_width : int;
            cur : llvalue;
            next : llvalue;
            typ : global_type;
        }

    (* some type abbreviations *)
    type global_simple = bool -> int -> uid -> global
    type global_reg = int -> uid -> global
    type global_mem = int -> int -> uid -> global
    type global_fns = global_simple * global_reg * global_mem

    type load_simple = bool -> signal -> llvalue
    type load_reg = signal -> llvalue
    type load_mem = llvalue -> signal -> llvalue
    type load_fns = load_simple * load_reg * load_mem

    type store_simple = llvalue -> bool -> signal -> unit
    type store_reg = llvalue -> signal -> unit
    type store_mem = llvalue -> signal -> unit
    type store_fns = store_simple * store_reg * store_mem

    let globals modl = 
        let simple port width uid = 
            let name = (if port then "port_" else "internal_") ^ 
                       string_of_int width ^ "_" ^ Int64.to_string uid in
            let width' = if port then pbits width else width in
            let cur = define_global name (const_int width' 0) modl in
            set_linkage Linkage.Internal cur;
            let g = {
                width = width;
                rnd_width = width';
                cur = cur;
                next = cur;
                typ = if port then G_Port else G_Internal;
            } in
            g
        in
        let reg width uid = 
            let name = "reg_" ^ string_of_int width ^ "_" ^ Int64.to_string uid in
            let cur = define_global name (const_int width 0) modl in
            let next = define_global (name^"_next") (const_int width 0) modl in
            set_linkage Linkage.Internal cur;
            set_linkage Linkage.Internal next;
            let g = {
                width = width;
                rnd_width = width;
                cur = cur;
                next = next;
                typ = G_Reg;
            } in
            g
        in
        let mem width count uid = 
            let name = "mem_" ^ string_of_int width ^ "_" ^ 
                                string_of_int count ^ "_" ^ Int64.to_string uid in
            let cur = declare_global (array_type (int_type width) count) name modl in
            let next = define_global (name^"_next") (const_int width 0) modl in
            set_linkage Linkage.Internal cur;
            set_linkage Linkage.Internal next;
            let g = {
                width = width;
                rnd_width = width;
                cur = cur;
                next = next;
                typ = G_Mem;
            } in
            g
        in
        let globals = ref UidMap.empty in
        let memoize f uid = 
            try UidMap.find uid !globals
            with _ -> 
                let g = f uid in
                globals := UidMap.add uid g !globals;
                g
        in
        let simple r w = memoize (simple r w) in
        let reg w = memoize (reg w) in
        let mem w c = memoize (mem w c) in
        globals, (simple, reg, mem)


    let load fn (simple,reg,mem) = 
        let entry_bb () = builder_at (global_context()) 
            (instr_begin (entry_block fn)) in
        let load_simple port s = 
            let builder = entry_bb () in
            let name = name "sload" s in
            let g = simple port (Sc.width s) (uid s) in 
            let x = build_load g.cur name builder in
            if g.width = g.rnd_width then x
            else build_trunc x (int_type g.width) name builder
        in
        let load_reg s = 
            let builder = entry_bb () in
            let name = name "rload" s in
            let g = reg (Sc.width s) (uid s) in 
            let x = build_load g.cur name builder in
            build_uresize x g.rnd_width g.width name builder
        in
        let load_mem addr s = 
            let builder = entry_bb () in
            let name = name "mload" s in
            let g = mem (Sc.width s) (memsize s) (uid s) in
            let addr = build_gep g.cur [| zero32; addr |] "" builder in
            build_load addr name builder
        in
        let globals = ref UidMap.empty in
        let memoize f s = 
            try UidMap.find (uid s) !globals
            with _ ->
                let instr = f s in 
                globals := UidMap.add (uid s) instr !globals;
                instr
        in
        let load_simple port = memoize (load_simple port) in
        let load_reg = memoize load_reg in
        let load_mem addr = memoize (load_mem addr) in
        load_simple, load_reg, load_mem

    let store builder (simple,reg,mem) = 
        let store_simple instr port s = 
            let name = name "sstore" s in
            let g = simple port (Sc.width s) (uid s) in
            let x = build_uresize instr g.width g.rnd_width name builder in
            build_store x g.next builder |> ignore
        in
        let store_reg instr s = 
            let g = reg (Sc.width s) (uid s) in
            build_store instr g.next builder |> ignore
        in
        let store_mem instr s = 
            let g = mem (Sc.width s) (memsize s) (uid s) in
            build_store instr g.next builder |> ignore
        in
        store_simple, store_reg, store_mem

    type update_reg = signal -> unit
    type update_mem = llvalue -> signal -> unit
    type update_fns = update_reg * update_mem

    let update builder (simple,reg,mem) = 
        let update_reg s =
            let g = reg (Sc.width s) (uid s) in
            let x = build_load g.next "" builder in
            build_store x g.cur builder |> ignore
        in 
        let update_mem addr s = 
            let g = mem (Sc.width s) (memsize s) (uid s) in
            let addr = build_gep g.cur [| zero32; addr |] "" builder in
            let x = build_load g.next "" builder in
            build_store x addr builder |> ignore
        in
        update_reg, update_mem

    let rec load_signal (s,r,m) map signal = 
        match signal with
        | Signal_const(_) -> const_of_signal signal
        | _ ->
        begin
            try UidMap.find (uid signal) map
            with _ ->
            begin
                match signal with
                | Signal_reg(_) -> r signal
                | Signal_mem(_,_,_,x) -> 
                    m (load_signal (s,r,m) map x.mem_read_address) signal
                | _ -> s false signal
            end
        end

end

module LlvmInfo = 
struct

    open LlvmUtils
    open LlvmGlobals

    (* functions for getting information about the circuit *)
    let compile_info modl name (ret,retz) args f values = 
        make_function modl name ret args
            (fun fn builder ->
                let arg = (params fn).(0) in
                let comparer (d,i) v = 
                    let cmp = build_icmp Icmp.Eq (const_int 32 i) arg "cmp" builder in 
                    let s = build_select cmp (f fn builder v) d "sel" builder in
                    (s, i+1)
                in
                let d,_ = List.fold_left comparer (retz, 0) values in
                ignore (build_ret d builder)
            )


    (* get width of an IO signal *)
    let compile_width modl io signals = 
        compile_info modl ("width_" ^ io) (int32,zero32) [|int32|]
            (fun fn builder signal -> const_int 32 (Sc.width signal))
            signals

    (* get name of an IO signal.  Note, we generate a function which returns
     * each character in turn, rather than a pointer to the string *)
    let compile_name modl io signals = 
        (* generate globals for the names *)
        let mk_global_name signal  = 
            let name = List.hd (names signal) in
            define_global ("name" ^ "_" ^ io ^ "_" ^ name) (const_string name) modl
        in
        let names = List.map mk_global_name signals in
        (* create look up function *)
        compile_info modl ("name_" ^ io) (int8,zero8) [|int32;int32|]
            (fun fn builder name -> 
                let name_gep builder g i = build_gep g [| zero32; i |] "gep" builder in
                build_load (name_gep builder name (params fn).(1)) "" builder
            ) names

    (* returns a pointer to the data as a 'nativeint' which can then be wrapped
     * in a big array *)
    let compile_ptr get_global modl io signals =
        let globals = List.map (fun s -> (get_global (Sc.width s) (uid s)).cur) signals in
        compile_info modl ("ptr_" ^ io) (int64,zero64) [|int32|]
            (fun fn builder d ->
                let bcast = build_bitcast d (ptr_type 1 (int64)) "bitcast" builder in
                let gep = build_gep bcast [| zero64 |] "gep" builder in
                let p2i = build_ptrtoint gep (int64) "get_p2i" builder in
                p2i
            ) globals

    (* converts a nativeint (as queried from the simulation module) to a
     * bigarray *)
    external llvm_get_ptr : nativeint -> int -> Bits_ext.Utils_ext.bani =
        "llvmsim_get_ptr"

    (* find a function in the JIT, or raise an exception *)
    let lookup_function name jit = 
        match Ee.find_function name jit with
        | Some(x) -> x
        | None -> failwith ("Couldn't find function " ^ name ^ " in JIT")

    (* look up the circuit ports *)
    let query_ports io jit =
        (* find the functions in the module *)
        let width = lookup_function ("width_" ^ io) jit in
        let name = lookup_function ("name_" ^ io) jit in
        let ptr = lookup_function ("ptr_" ^ io) jit in
        (* wrap so they can be called *)
        let width n = Gv.as_int (Ee.run_function width [| Gv.of_int int32 n |] jit) in
        let name n m = Gv.as_int (Ee.run_function name 
                                    [| Gv.of_int int32 n; Gv.of_int int32 m |] jit) in
        let name n = 
            let rec b n m str =
                let x = name n m in
                if x <> 0 then b n (m+1) (str ^ Char.escaped (Char.chr x))
                else str
            in
            b n 0 ""
        in
        let ptr n = Gv.as_nativeint (Ee.run_function ptr [| Gv.of_int int32 n |] jit) in
        (* look up each port *)
        let rec lookup n = 
            let width,name,ptr = width n, name n, ptr n in
            if name = "" then []
            else (name, ref (llvm_get_ptr ptr width, width)) :: lookup (n+1)
        in
        List.rev (lookup 0)

end

module LlvmCompile = 
struct

    open LlvmUtils
    open LlvmGlobals

    let compile_comb modl fn builder load map signal =
        let sdep n = List.nth (deps signal) n in
        let instr = load map in
        let dep = instr << sdep in
        let name n = name n signal in

        let compile_cat () =
            let name = name "cat" in
            let width = Sc.width signal in
            let ze s w = build_zext s (int_type w) name builder in
            let or2 r s n =
                if n = 0 then
                    ze s width
                else
                    let f s n = build_shl (ze s width) 
                        (const_int width n) 
                        "cat_shl" builder 
                    in
                    build_or r (f s n) name builder
            in
            let sdeps = deps signal in
            let deps = Utils.mapi (fun i _ -> dep i) sdeps in
            let d = List.rev (Utils.map2 (fun s d -> Sc.width s, d) sdeps deps) in
            fst(List.fold_left 
                (fun (res,sft) (wid,arg) -> or2 res arg sft,sft+wid)
                (const_int width 0, 0) d)
        in

        let compile_select h l = 
            let name = name "select" in
            let d = dep 0 in
            let w = Sc.width (sdep 0) in
            if w = (h-l+1) then d
            else
                let sft = const_int w l in
                let s = build_lshr d sft name builder in
                build_trunc s (int_type (h-l+1)) name builder
        in
        
        let compile_mul signed id = 
            let name = name "mul" in
            let ext = if signed then build_sext else build_zext in
            let a = ext (dep 0) (int_type id.s_width) name builder in
            let b = ext (dep 1) (int_type id.s_width) name builder in
            build_mul a b name builder (* XXX this may only work upto a max no of bits *)
        in

        (* build a select table and jump targets for the mux *)
        let compile_generic_mux () = 
            let name = name "gmux" in
            let entry_bb () = builder_at 
                (global_context()) (instr_begin (entry_block fn)) in
            let alloca w = build_alloca (int_type w) name (entry_bb ()) in 
            let r = alloca (Sc.width signal) in
            let sel_bb = insertion_block builder in 
            let case_bbs = 
                (* for some reason I dont get, the cases come out
                 * backwards in the llvm code *)
                let append_blk _ = 
                    let bb = append_block (name^"_bb") fn in
                    position_at_end bb builder;
                    bb
                in
                List.map append_blk (List.tl (deps signal))
            in
            let end_bb = append_block  (name^"_bb_end") fn in
            Utils.iteri (fun i bb ->
                    position_at_end bb builder;
                    build_store (dep (i+1)) r builder |> ignore;
                    build_br end_bb builder |> ignore
            ) case_bbs;
            position_at_end sel_bb builder;
            make_switch (Sc.width signal) (dep 0) case_bbs builder;
            position_at_end end_bb builder;
            build_load r name builder
        in

        (* a simple mux is one with only a few cases, and is implemented as
         * a chain of 'select' statements.  Not actually sure this is
         * worthwhile. *)
        let compile_simple_mux () = 
            let name = name "smux" in
            let wsel = Sc.width (List.hd (deps signal)) in
            let deps = List.map instr (deps signal) in
            let sel,cases = List.hd deps, List.tl deps in
            let def,cases =
                let c = List.rev cases in
                List.hd c, List.rev (List.tl c)
            in
            let r, i = List.fold_left (fun (r,i) v -> 
                build_select 
                    (build_icmp Icmp.Eq sel (const_int wsel i) name builder) 
                    v r name builder, 
                    (i+1)
                ) (def,0) cases in
            r
        in
        
        (* a constant mux has only constants as it's cases and is built
         * using a global array *)
        let is_constant_mux () = 
            List.fold_left (fun b s -> is_const s && b)
                true (List.tl (deps signal))
        in
        let compile_constant_mux () = 
            let name = name "cmux" in
            let size = List.length (List.tl (deps signal)) in
            let global = lookup_global name modl in
            let global = 
                match global with
                | Some (x) -> x
                | None ->
                    (* create initialized array *)
                    let values = 
                        List.map (fun s -> const_of_signal s) (List.tl (deps signal))
                        |> Array.of_list
                    in
                    let values = const_array (int_type (Sc.width signal)) values in
                    let g = define_global name values modl in
                    set_linkage Linkage.Internal g;
                    g
            in
            let sel = dep 0 in
            let w = Sc.width (sdep 0) in
            let max = const_int (w+1) (size-1) in
            let sel = build_uresize sel w (w+1) name builder in
            let sel = 
                build_select 
                    (build_icmp Icmp.Ule sel max name builder)
                    sel max name builder
            in
            let addr = build_gep global [| zero32; sel |] name builder in
            build_load addr name builder
        in

        (* select the mux implementation *)
        let compile_mux () = 
            if List.length (deps signal) <= (4+1) then
                compile_simple_mux ()
            else if is_constant_mux () then
                compile_constant_mux ()
            else 
                compile_generic_mux ()
        in

        (* compile each type of signal *)
        match signal with
        | Signal_empty -> failwith "cant compile empty signal"
        | Signal_const(_,v) -> failwith "cant compile constants"
        | Signal_op(id,op) ->
        begin
            match op with
            | Signal_add -> build_add (dep 0) (dep 1) (name "add") builder  
            | Signal_sub -> build_sub (dep 0) (dep 1) (name "sub") builder
            | Signal_mulu -> compile_mul false id
            | Signal_muls -> compile_mul true id
            | Signal_and -> build_and (dep 0) (dep 1) (name "and") builder 
            | Signal_or -> build_or (dep 0) (dep 1) (name "or") builder
            | Signal_xor -> build_xor (dep 0) (dep 1) (name "xor") builder
            | Signal_eq -> build_icmp Icmp.Eq (dep 0) (dep 1) (name "eq") builder
            | Signal_not -> build_not (dep 0) (name "not") builder
            | Signal_lt -> build_icmp Icmp.Ult (dep 0) (dep 1) (name "lt") builder
            | Signal_cat -> compile_cat ()
            | Signal_mux -> compile_mux ()
        end
        | Signal_wire(_) -> dep 0
        | Signal_select(_,h,l) -> compile_select h l
        | Signal_reg(_,r) -> failwith "Registers not expected here"
        | Signal_mem(_,_,r,m) -> failwith "Memories not expected here"
        | Signal_inst(_) -> failwith "Instantiations are not supported in simulation"

    let compile_comb_list modl fn builder load map signals = 
        let compile_comb = compile_comb modl fn builder load in
        List.fold_left (fun map s ->
            UidMap.add (uid s) (compile_comb map s) map)
            map signals

    (* let compile_reg *)
    let compile_reg builder load store signal = 
        let r = match signal with Signal_reg(_,r) -> r | _ -> failwith "" in
        let sdep n = List.nth (deps signal) n in
        let instr = load in
        let dep = instr << sdep in
        let name n = name n signal in
        let width = Sc.width signal in
        let src = dep 0 in (* input data *)
        (*let cur_q = UidMap.find (uid signal) reg_globals_loaded in*)
        let cur_q = instr signal in
        let clr, clr_level, clr_value = 
            if r.reg_clear <> Sc.empty then
                instr r.reg_clear, instr r.reg_clear_level, instr r.reg_clear_value
            else 
                const_int 1 0, const_int 1 1, const_int width 0
        in
        let clr = build_icmp Icmp.Eq clr clr_level (name "reg_clr") builder in
        let ena = 
            if r.reg_enable <> Sc.empty then instr r.reg_enable
            else const_int 1 1
        in
        let ena = build_and ena 
            (build_not clr (name "reg_clr_not") builder) 
            (name "reg_ena") builder in
        let q = build_select clr clr_value cur_q 
            (name "reg_clr_update") builder in
        let q = build_select ena src q 
            (name "reg_ena_update") builder in
        (*let (d,w,w') = UidMap.find (uid signal) l.reg_globals in*)
        store q signal
    
    (* let compile_mem *)
            
end


module V2 =
struct

    open LlvmUtils
    open LlvmGlobals
    open LlvmInfo

    type cyclesim = Bits_ext.Comb.BigarraybitsNativeint.t Cyclesim.Api.cyclesim

    let reset_value = function
        | Signal_reg(_,r) ->
            let v = try const_of_signal r.reg_reset_value 
                    with _ -> failwith "register reset values must be constant in the llvm code generator"
            in
            v
        | _ -> failwith "Expecting register"

    let compile_simple circuit = 
        let context = global_context () in
        let modl = create_module context (Circuit.name circuit) in

        let g_map,g_ops = globals modl in
        let g_simple,g_reg,g_mem = g_ops in

        (* information gathering *)
        compile_width modl "i" (Circuit.inputs circuit);
        compile_width modl "o" (Circuit.outputs circuit);
        compile_name modl "i" (Circuit.inputs circuit);
        compile_name modl "o" (Circuit.outputs circuit);
        compile_ptr (g_simple true) modl "i" (Circuit.inputs circuit);
        compile_ptr (g_simple true) modl "o" (Circuit.outputs circuit);
        
        (* scheduler *)
        let regs, mems, consts, inputs, remaining = Cs.find_elements circuit in
        let ready = regs @ mems @ inputs @ consts in
        let schedule = Cs.scheduler deps remaining ready in

        (* initial creation of regs and mems (inputs and outputs already made) *)
        List.iter (fun s -> g_reg (Sc.width s) (uid s) |> ignore) regs;
        List.iter (fun s -> g_mem (Sc.width s) (memsize s) (uid s) |> ignore) mems;

        let compile_cycle cycle builder = 
            let l_ops = LlvmGlobals.load cycle g_ops in
            let load = LlvmGlobals.load_signal l_ops in
            let s_simple,s_reg,_ = LlvmGlobals.store builder g_ops in
            let u_reg,_ = LlvmGlobals.update builder g_ops in
            let map = LlvmCompile.compile_comb_list 
                modl cycle builder load
                UidMap.empty schedule 
            in
            let store rnd s = 
                try s_simple (load map s) rnd s 
                with _ -> ()
            in
            let store_reg instr s = s_reg instr s in
            let update_reg s = u_reg s in
            let return () = build_ret_void builder |> ignore in
            let compile_reg = LlvmCompile.compile_reg builder (load map) store_reg in
            (* store outputs *)
            List.iter (store true) (Circuit.outputs circuit);
            (* sequential logic *)
            List.iter compile_reg regs;
            List.iter update_reg regs;
            (* return value *)
            return();
            Llvm_analysis.assert_valid_function cycle |> ignore
        in

        let compile_reset reset builder = 
            List.iter (fun s ->
                let v = reset_value s in
                let g = g_reg (Sc.width s) (uid s) in
                ignore (build_store v g.cur builder) 
            ) regs;
            build_ret_void builder |> ignore;
            Llvm_analysis.assert_valid_function reset |> ignore
        in

        make_function modl "reset" void [||] compile_reset;
        make_function modl "cycle" void [||] compile_cycle;

        (* dump_module modl;*)
        modl

    let compile max circuit = 

        let context = global_context () in
        let modl = create_module context (Circuit.name circuit) in

        let g_map,g_ops = globals modl in
        let g_simple,g_reg,g_mem = g_ops in

        (* information gathering *)
        compile_width modl "i" (Circuit.inputs circuit);
        compile_width modl "o" (Circuit.outputs circuit);
        compile_name modl "i" (Circuit.inputs circuit);
        compile_name modl "o" (Circuit.outputs circuit);
        compile_ptr (g_simple true) modl "i" (Circuit.inputs circuit);
        compile_ptr (g_simple true) modl "o" (Circuit.outputs circuit);
        
        (* scheduler *)
        let regs, mems, consts, inputs, remaining = Cs.find_elements circuit in
        let ready = regs @ mems @ inputs @ consts in
        let schedule = Cs.scheduler deps remaining ready in

        (* initial creation of regs and mems (inputs and outputs already made) *)
        List.iter (fun s -> g_reg (Sc.width s) (uid s) |> ignore) regs;
        List.iter (fun s -> g_mem (Sc.width s) (memsize s) (uid s) |> ignore) mems;

        let compile_cycle cycle builder = 

            let compile_cycle signals cycle builder = 
                set_linkage Linkage.Internal cycle;
                let l_ops = LlvmGlobals.load cycle g_ops in
                let load = LlvmGlobals.load_signal l_ops in
                let s_simple,s_reg,_ = LlvmGlobals.store builder g_ops in
                let map = LlvmCompile.compile_comb_list 
                    modl cycle builder load
                    UidMap.empty signals
                in
                let store rnd s = 
                    try s_simple (load map s) rnd s 
                    with _ -> ()
                in
                let return () = build_ret_void builder |> ignore in
                cycle, map, store, return 
            in

            let compile_reg_store signals cycle builder = 
                set_linkage Linkage.Internal cycle;
                let l_ops = LlvmGlobals.load cycle g_ops in
                let load = LlvmGlobals.load_signal l_ops in
                let _,s_reg,_ = LlvmGlobals.store builder g_ops in
                let compile_reg = LlvmCompile.compile_reg builder 
                    (load UidMap.empty) s_reg in
                List.iter compile_reg signals;
                build_ret_void builder |> ignore;
                cycle
            in

            let compile_reg_update signals cycle builder = 
                set_linkage Linkage.Internal cycle;
                let u_reg,_ = LlvmGlobals.update builder g_ops in
                List.iter u_reg signals;
                build_ret_void builder |> ignore;
                cycle
            in

            let rec build name n f signals = 
                let rec split n a b = 
                    if n=max then List.rev a, b
                    else
                        match b with
                        | [] -> List.rev a,[]
                        | h::t -> split (n+1) (h::a) t
                in
                let split = split 0 [] in
                let h,t = split signals in
                let r = 
                    make_function modl 
                        (name ^ "_" ^ string_of_int n) void [||]
                        (f h)
                in
                if t=[] then [r]
                else r :: build name (n+1) f t
            in

            (* build the sub-cycle functions *)
            let r = build "cycle" 0 compile_cycle schedule in
            
            (* build register updates *)
            let reg_store = build "reg_store" 0 compile_reg_store regs in
            let reg_update = build "reg_update" 0 compile_reg_update regs in

            (* sort out inter-sub-cycle dependancies *)
            let is_input = 
                let set = List.fold_left (fun m s -> UidSet.add (uid s) m)
                    UidSet.empty (Circuit.inputs circuit) in
                (fun u -> UidSet.mem u set)
            in
            List.iter (fun (_,m,store,_) ->
                UidMap.iter (fun u _ ->
                    let s = Circuit.signal_of_uid circuit u in
                    let find u = 
                        try UidMap.find u !g_map |> ignore; true
                        with _ -> false
                    in
                    if (find u) &&
                       (not (is_input u)) && 
                       (not (is_reg s)) && 
                       (not (is_mem s)) then
                            store false s
                ) m
            ) r;
            List.iter (fun (_,_,_,r) -> r()) r;

            (* call sub-cycle functions *)
            List.iter (fun (f,_,_,_) -> build_call f [||] "" builder |> ignore) r;
            (* call reg update function *)
            List.iter (fun f -> build_call f [||] "" builder |> ignore) reg_store; 
            List.iter (fun f -> build_call f [||] "" builder |> ignore) reg_update;
            build_ret_void builder |> ignore 
        in

        let compile_reset reset builder = 
            List.iter (fun s ->
                let v = reset_value s in
                let g = g_reg (Sc.width s) (uid s) in
                ignore (build_store v g.cur builder) 
            ) regs;
            build_ret_void builder |> ignore;
            Llvm_analysis.assert_valid_function reset |> ignore
        in

        make_function modl "reset" void [||] compile_reset;
        make_function modl "cycle" void [||] compile_cycle;

        (* dump_module modl; *)
        modl


    let make circuit = 
        let modl = compile 100 circuit in
        (*let mp = ModuleProvider.create modl in
        let jit = Ee.create_jit mp in*)
        let optlevel = 2 in
        let jit = Ee.create_jit modl optlevel in
        let mk name = 
            let f = lookup_function name jit in
            (fun () -> Ee.run_function f [||] jit |> ignore) 
        in
        let cycle = mk "cycle" in 
        let reset = mk "reset" in
        let in_ports = query_ports "i" jit in
        let out_ports = query_ports "o" jit in
        {
            sim_cycle = cycle;
            sim_reset = reset;
            sim_internal_ports = [];
            sim_in_ports = in_ports;
            sim_out_ports = out_ports;
        }

    let write path circuit = 
        let modl = compile 100 circuit in
        (* write bitcode *)
        let fname = Filename.concat path (Circuit.name circuit ^ ".bc") in
        if not (Llvm_bitwriter.write_bitcode_file modl fname) then
            failwith "Failed to write bitcode"

    let load path = 
        let name = Bits_ext.Utils_ext.filebase path in
        let context = global_context () in
        let membuf = MemoryBuffer.of_file path in
        let modl = Llvm_bitreader.parse_bitcode context membuf in
        (* let mp = ModuleProvider.create modl in
        let jit = Ee.create_jit mp in *)
        let optlevel = 2 in
        let jit = Ee.create_jit modl optlevel in
        (* need to get the circuit information from the bit code *)
        let cycle = lookup_function ("cycle_" ^ name) jit in
        let reset = lookup_function ("reset_" ^ name) jit in
        
        let in_ports = query_ports "i" jit in
        let out_ports = query_ports "o" jit in

        let cycle = (fun () -> ignore (Ee.run_function cycle [||] jit)) in
        let reset = (fun () -> ignore (Ee.run_function reset [||] jit)) in

        {
            sim_cycle = cycle;
            sim_reset = reset;
            sim_internal_ports = [];
            sim_in_ports = in_ports;
            sim_out_ports = out_ports;
        }

end


module Make(Base : T)(B : Bits_ext.S) =
struct

    open HardCaml

    type t = B.t
    type base_cyclesim = Base.cyclesim
    type cyclesim = t Cyclesim.Api.cyclesim

    open Cyclesim.Api
    open Base

    let wrap sim = 
        let in_ports = List.map (fun (s,d) -> 
            let d,w = fst !d, snd !d in s, (ref (B.zero w), d, w)) sim.sim_in_ports in
        let out_ports = List.map (fun (s,d) -> 
            let d,w = fst !d, snd !d in s, (ref (B.zero w), d, w)) sim.sim_out_ports in
        let cycle () = 
            List.iter (fun (s,(t,b,w)) -> B.to_bani_ptr !t b) in_ports; 
            sim.sim_cycle();
            List.iter (fun (s,(t,b,w)) -> t := B.of_bani_ptr w b !t) out_ports
        in
        let reset () = 
            List.iter (fun (s,(t,b,w)) -> B.to_bani_ptr !t b) in_ports;
            sim.sim_reset();
            List.iter (fun (s,(t,b,w)) -> t := B.of_bani_ptr w b !t) out_ports
        in
        {
            sim_cycle = cycle;
            sim_reset = reset;
            sim_internal_ports = [];
            sim_in_ports = List.map (fun (s,(t,b,w)) -> s,t) in_ports;
            sim_out_ports = List.map (fun (s,(t,b,w)) -> s,t) out_ports;
        }
    
    let make circuit = wrap (Base.make circuit)

    let write path circuit = Base.write path circuit

    let load path = wrap (Base.load path)

end

