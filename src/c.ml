(* 
 * hardcaml - hardware design in OCaml
 *
 *   (c) 2014 MicroJamJar Ltd
 *
 * Author(s): andy.ray@ujamjar.com
 * Description: 
 *
 *)

open Circuit
open Signal.Types
open Signal.Comb
open Utils
open Printf

let array_mul
    os
    signed                  (* signed/unsigned multiplication *)
    words_r words_a words_b (* words in args *)
    bits_r bits_a bits_b    (* bits in args *)
    r a b                   (* offset of result and args in memory array *)
    =
    let soi = string_of_int in

    let mem_at a = "sim->data[" ^ soi a ^ "]" in
    let int_to_hex a = sprintf "%lx" a in
    
    let (<<.) = Int32.shift_left in

    (* return a string giving the value of the look up on the arg *)
    let access a words bits i =
        if i = words-1 then
            (* last word, sign extend if necessary *)
            if signed && (bits mod 32 <> 0) then (
                ("((" ^ mem_at (a + i) ^ " & 0x" ^ 
                    int_to_hex (1l <<. ((bits-1) mod 32)) ^ 
                    ") != 0 ? " ^ mem_at (a + i) ^ " | 0x" ^ 
                    int_to_hex (0xffffffffl <<. (bits mod 32)) ^ 
                    " : " ^ mem_at (a + i) ^ ")")
            ) else
                (mem_at (a + i))
        else if i >= words then
            if signed then 
                ("((" ^ mem_at (a + words - 1) ^ 
                    " & 0x" ^ int_to_hex (1l <<. ((bits-1) mod 32)) ^ 
                    ") != 0 ? 0xffffffff : 0)") 
            else "0"
        else
        (mem_at (a + i))
    in
    let access_a = access a words_a bits_a in
    let access_b = access b words_b bits_b in
      
    if words_r = 1 then begin
        os ("  " ^ mem_at r ^ " = " ^ 
                   access_a 0 ^ " * " ^ 
                   access_b 0 ^ ";")  
    end else begin
        for i = 0 to words_r - 1 do 
            os ("    " ^ mem_at (r + i) ^ " = 0;\n")
        done;
        os ("    {\n    uint64_t tmp = 0;\n");
        for i = 0 to words_r - 1 do 
            let ib = ref 0 in
            for ia = i downto 0 do 
                os ("    tmp = ((uint64_t) " ^ access_a ia ^ 
                        ") * ((uint64_t) " ^ access_b !ib ^ ");\n");
                for ic = i to words_r - 1 do
                    os ("    tmp += (uint64_t) " ^ mem_at (r+ic) ^ ";\n");
                    os ("  " ^ mem_at (r + ic) ^ 
                        " = (uint32_t) (tmp & 0xffffffff);\n");
                    os ("  tmp = tmp >> 32;\n");
                done;
                ib := !ib + 1;
            done;
        done;
        os ("    }\n")
    end

let write ?(name="") os circuit = 
    let name = if name = "" then Circuit.name circuit else name in
    let name_ = "" in
    let soi = string_of_int in
    
    (* schedule the simulation *)
    let regs, mems, consts, inputs, remaining = Cyclesim.find_elements circuit in
    let ready = regs @ inputs @ consts in
    let deps' s = 
        match s with 
        | Signal_mem(_, _,  _, m) -> [m.mem_read_address]
        | _ -> deps s
    in
    let schedule = Cyclesim.scheduler deps' (mems @ remaining) ready in
    
    (* the state of the simulator is represented by an array of uint32_t in c.
     * Each signal is at a certain offset within that array.  Create a map
     * of signals to the offset index *)

    let words n = (n + 31) / 32 in

    let mk_map f = 
        List.fold_left (fun (map,ofs) signal ->
            UidMap.add (uid signal) ofs map, ofs + (f signal)
        ) 
    in
    let m0 = UidMap.empty, 0 in
    (* data map *)
    let data_map, data_length = 
        (* XXX do we need mems?  what about internal_ports *)
        List.fold_left (mk_map (words << width)) m0 [ ready; mems; remaining ]
    in
    (* shadow maps for state elements *)
    let reg_map, reg_length = mk_map (words << width) m0 regs in
    let mem_size s = 
        match s with 
        | Signal_mem(_,_,_,m) -> words (m.mem_size * width s)
        | _ -> failwith "expecting memory"
    in
    let mem_map, mem_length = mk_map mem_size m0 mems in
    (* XXX muxes map? *)
    let mux_size s = List.length (deps s) - 1 in
    let muxes = List.filter 
        (function Signal_op(_, Signal_mux) -> true | _ -> false) remaining
    in
    let mux_map, mux_length = mk_map mux_size m0 muxes in

    (* write key *)
    os "/* key:\n";
    let key l = List.iter (fun s ->
            os (sprintf "[%i] %s\n"
                (UidMap.find (uid s) data_map) (to_string s))
        ) l
    in
    List.iter key [ inputs; schedule; regs; mems ];
    os "*/\n";

    (* write init function *)
    let num_in_ports = List.length (Circuit.inputs circuit) in
    let num_out_ports = List.length (Circuit.outputs circuit) in
    os ("#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>

typedef struct _port {
    char *name;
    int width;
    uint32_t *bits;
} port;

typedef struct _simulator {
    uint32_t *data, *regs, *mems, *muxs;
    port *in_ports;
    int num_in_ports;
    port *out_ports;
    int num_out_ports;
} simulator;

simulator *"^name_^"init() {
    simulator *sim = malloc(sizeof(simulator));
    sim->data = calloc(sizeof(uint32_t), " ^ soi data_length ^ ");
    sim->regs = calloc(sizeof(uint32_t), " ^ soi reg_length ^ ");
    sim->mems = calloc(sizeof(uint32_t), " ^ soi mem_length ^ ");
    sim->muxs = calloc(sizeof(uint32_t), " ^ soi mux_length ^ ");
    sim->in_ports = calloc(sizeof(port), " ^ soi num_in_ports ^ ");
    sim->num_in_ports = " ^ soi num_in_ports ^ ";
    sim->out_ports = calloc(sizeof(port), " ^ soi num_out_ports ^ ");
    sim->num_out_ports = " ^ soi num_out_ports ^ ";
");

    (* set inputs and outputs *)
    let io_port io i s = 
        let prt = if io then "in_ports" else "out_ports" in
        let offset s = UidMap.find (uid s) data_map in
        os ("    sim->" ^ prt ^ "[" ^ soi i ^ "].name = \"" ^ 
            List.hd (names s) ^ "\";\n");
        os ("    sim->" ^ prt ^ "[" ^ soi i ^ "].width = " ^ 
            soi (width s) ^ ";\n");
        os ("    sim->" ^ prt ^ "[" ^ soi i ^ "].bits = sim->data + " ^ 
            soi (offset s) ^ ";\n");
    in
    os ("    // inputs\n");
    iteri (io_port true) (Circuit.inputs circuit);
    os ("    // outputs\n");
    iteri (io_port false) (Circuit.outputs circuit);

    (* initialize muxes *)
    os ("    // mux tables\n");
    let _ = List.fold_left (fun off s ->
        let deps = List.tl (deps s) in
        iteri (fun i s ->
            let d = UidMap.find (uid s) data_map in
            os (sprintf "    sim->muxs[%i] = %i;\n" (off+i) d)
        ) deps;
        off + List.length deps) 0 muxes
    in

    (* set constants *)
    os ("    // constants\n");
    let write_const s = 
        let ofs = UidMap.find (uid s) data_map in
        let x,w = Bits.Comb.ArraybitsInt32.const (const_value s) in
        Array.iteri (fun i x ->
            os ("    sim->data[" ^ soi (ofs+i) ^ "] = (uint32_t) (" ^
                Int32.to_string x ^ ");\n")
        ) x
    in
    List.iter write_const consts;
    os "    return sim;\n}\n\n";

    (* reset function *)
    os ("void "^name_^"reset(simulator *sim) {\n");
    let compile_reset signal =
        match signal with
        | Signal_reg(_,r) ->
            if r.reg_reset <> Signal.Comb.empty then begin
                let tgt0 = UidMap.find (uid signal) data_map in
                let tgt1 = UidMap.find (uid signal) reg_map in
                let value = UidMap.find (uid r.reg_reset_value) data_map in
                os (sprintf "    copy(sim->data+%i, sim->data+%i, %i);\n"
                    value tgt0 (width signal));
                os (sprintf "    copy(sim->data+%i, sim->regs+%i, %i);\n"
                    value tgt1 (width signal));
            end
        | _ -> failwith "expecting reg"
    in
    List.iter compile_reset regs;
    os "}\n\n";

    (* cycle function *)
    os ("void "^name_^"cycle(simulator *sim) {\n");

    let mask w tgt=  
        let q = w / 32 in
        if w mod 32 <> 0 then
            os (sprintf "    sim->data[%i] &= 0x%lx;\n"
                (tgt+q)
                (Int32.shift_right_logical (-1l) (32 - (w mod 32))))
    in

    (* combinatorial logic *)
    let compile signal = 
        os (sprintf "// %s\n" (to_string signal));
        let tgt = UidMap.find (uid signal) data_map in
        let doff n = UidMap.find (uid (List.nth (deps signal) n)) data_map in
    
        let data i = "sim->data[" ^ soi i ^ "]" in

        let mask() = mask (width signal) tgt in

        let addop op = 
            let w = words (width signal) in
            let d0,d1 = doff 0, doff 1 in
            os "    {\n    uint64_t carry=0;\n    uint64_t sum=0;\n";
            for i=0 to w-1 do
                os (sprintf "    sum = (uint64_t)%s %s (uint64_t)%s %s carry;\n" 
                    (data (d0+i)) op (data (d1+i)) op);
                os ("    carry = (sum >> 32) & 1;\n");
                os (sprintf "    %s = (uint32_t) sum;\n" (data (tgt+i)));
            done;
            mask();
            os "    }\n"
        in

        let eqop () = 
            let w = words (width (List.hd (deps signal))) in
            let d0,d1 = doff 0, doff 1 in
            os (sprintf "    %s = 1;\n" (data tgt));
            for i=0 to w-1 do
                os (sprintf "    %s &= %s == %s;\n" (data tgt)
                    (data (d0+i)) (data (d1+i)));
            done
        in

        let ltop () = 
            let w = words (width (List.hd (deps signal))) in
            let d0,d1 = doff 0, doff 1 in
            os (sprintf "    %s = \n" (data tgt));
            for i=w-1 downto 0 do
                os (sprintf "        %s < %s ? 1 :\n" 
                    (data (d0+i)) (data (d1+i)));
                os (sprintf "        %s > %s ? 0 :\n" 
                    (data (d0+i)) (data (d1+i)));
            done;
            os (sprintf "        0;\n")
        in

        let binop op =
            let d0,d1 = doff 0, doff 1 in
            let n = words (width signal) in
            for i=0 to n-1 do
                let s = sprintf 
                    "    %s = %s %s %s;\n"
                    (data (tgt+i)) (data (d0+i)) op (data (d1+i))
                in 
                os s
            done
        in
        let notop () = 
            let d = doff 0 in
            let n = words (width signal) in
            for i=0 to n-1 do
                let s = sprintf 
                    "    %s = ~ %s;\n"
                    (data (tgt+i)) (data (d+i))
                in
                os s
            done;
            mask ()
        in

        let copy f t n = 
            let w = words n in
            for i=0 to w-1 do
                os (sprintf "    %s = %s;\n"
                    (data (t+i)) (data (f+i)))
            done
        in

        let cpy () = copy (doff 0) tgt (width signal) in

        let rec ins s m tbit sbit = 
            let bits_left = m - tbit in
            if bits_left > 0 then
                let bits_left_t = 32 - (tbit mod 32) in
                let bits_left_s = 32 - (sbit mod 32) in
                let bits = min bits_left_t bits_left_s in
                let bits = min bits_left bits in
                let mask = 
                    sprintf "0x%lx" 
                        (Int32.shift_right_logical (-1l) (32-bits))
                in
                os (sprintf "    %s %s= (((%s >> %i) & %s) << %i);\n"
                    (data (tgt+(tbit/32)))
                    (if tbit mod 32 = 0 then "" else "|")
                    (data (s + (sbit/32)))
                    (sbit mod 32)
                    mask
                    (tbit mod 32));
                ins s m (tbit+bits) (sbit+bits)
            else
                ()
        in
        
        let sel hi lo =
            let w = width signal in
            let s = doff 0 in
            if lo mod 32 = 0 then
                let loword = lo / 32 in
                copy (s + loword) tgt w;
                mask ()
            else
                ins s w 0 lo
        in

        let cat() = 
            let d = List.rev (deps signal) in
            assert (List.length d > 1);
            let insert o s = 
                let w = width s in
                let s = UidMap.find (uid s) data_map in
                ins s (o+w) o 0;
                o+w
            in
            let _ = List.fold_left insert 0 d in
            ()
        in

        let reg r = 
            let f s = 
                if s = empty then None
                else Some(UidMap.find (uid s) data_map)
            in
            let data'' s i = data (i+(match s with Some(x) -> x | None -> 0)) in
            let data' s = data'' s 0 in
            let reg i = "sim->regs[" ^ soi i ^ "]" in
            let words = words (width signal) in
            let clr = f r.reg_clear in
            let clr_val = f r.reg_clear_value in
            let clr_lev = f r.reg_clear_level in
            let ena = f r.reg_enable in
            let d = UidMap.find (uid (List.hd (deps signal))) data_map in
            let q = UidMap.find (uid signal) reg_map in
            let do_clear() = 
                for i=0 to words-1 do
                    os (sprintf "        %s = %s;\n" 
                        (reg (q+i)) (data'' clr_val i))
                done
            in
            let do_update() = 
                for i=0 to words-1 do
                    os (sprintf "        %s = %s;\n" (reg (q+i)) (data (d+i)))
                done
            in
            match clr,ena with
            | None,None -> 
                 do_update()
            | None,Some(_) ->
                os (sprintf "    if (%s) {\n" (data' ena));
                do_update();
                os (sprintf "    }\n")
            | Some(_),None ->
                os (sprintf "    if (%s == %s) {\n" (data' clr) (data' clr_lev));
                do_clear();
                os (sprintf "    } else {\n");
                do_update();
                os (sprintf "    }\n")
            | Some(_),Some(_) ->
                os (sprintf "    if (%s == %s) {\n" (data' clr) (data' clr_lev));
                do_clear();
                os (sprintf "    } else if (%s) {\n" (data' ena));
                do_update();
                os (sprintf "    }\n")
        in

        let mem m =
            let words = words (width signal) in
            let mem = UidMap.find (uid signal) mem_map in
            let addr = UidMap.find (uid m.mem_read_address) data_map in
            for i=0 to words-1 do
                os (sprintf 
                    "    sim->mems[%i+(sim->data[%i]*%i)+%i] = sim->data+%i);\n"
                    mem addr words i (tgt+i))
            done
        in

        let mux () = 
            let d = deps signal in
            let sel,d = List.hd d, List.tl d in
            let len = List.length d in
            let sel = UidMap.find (uid sel) data_map in
            let mux = UidMap.find (uid signal) mux_map in
            os ("    {\n");
            os (sprintf "    uint32_t *base = sim->muxs+%i;\n" mux);
            os (sprintf "    uint32_t addr = sim->data[%i];\n" sel);
            os (sprintf "    addr = addr > %i ? %i : addr;\n" 
                (len-1) (len-1));
                os (sprintf "    addr = base[addr];\n");
            for i=0 to words (width signal) - 1 do
                os (sprintf "    sim->data[%i] = sim->data[addr+%i];\n"
                    (tgt+i) i)
            done;
            os "    }\n"
        in

        let mul signed = 
            let d0 = List.nth (deps signal) 0 in
            let d1 = List.nth (deps signal) 1 in
            let o0,o1 = doff 0, doff 1 in
            array_mul os signed 
                (words (width signal)) (words (width d0)) (words (width d1))
                (width signal) (width d0) (width d1)
                tgt o0 o1;
            mask();
        in

        match signal with
        | Signal_empty -> failwith "cant compile empty signal"
        | Signal_const(_) -> failwith "cant compile const - already done!"
        | Signal_op(_,op) ->
        begin
            match op with
            | Signal_add -> addop "+"
            | Signal_sub -> addop "-"
            | Signal_mulu -> mul false
            | Signal_muls -> mul true
            | Signal_and -> binop "&"
            | Signal_or -> binop "|"
            | Signal_xor -> binop "^"
            | Signal_eq -> eqop ()
            | Signal_not -> notop()
            | Signal_lt -> ltop()
            | Signal_cat -> cat()
            | Signal_mux -> mux()

        end
        | Signal_wire(_,d) -> cpy ()
        | Signal_select(_,h,l) -> sel h l
        | Signal_reg(_,r) -> reg r
        | Signal_mem(_,_,_,m) -> mem m
        | Signal_inst(_) -> failwith 
            "Instantiations are not supported in simulation"
    in

    let compile_reg_update signal = 
        match signal with
        | Signal_reg(_,_) ->
            let tgt = UidMap.find (uid signal) data_map in
            let src = UidMap.find (uid signal) reg_map in
            for i=0 to words(width signal)-1 do
                os (sprintf "    sim->data[%i] = sim->regs[%i];\n" 
                    (tgt+i) (src+i))
            done
        | _ -> failwith "expecting register"
    in

    let compile_mem_update signal = 
        match signal with
        | Signal_mem(_,_,r,m) ->
            let words = words (width signal) in
            let mem = UidMap.find (uid signal) mem_map in
            let we = UidMap.find (uid r.reg_enable) data_map in
            let w = UidMap.find (uid m.mem_write_address) data_map in
            let d = UidMap.find (uid (List.hd (deps signal))) data_map in
            os (sprintf "    if (sim->data[%i]) {\n" we);
            for i=0 to words-1 do
                os (sprintf "        sim->data[%i] = sim->mems[%i+(%i*%i)+%i];\n"
                    (d+i) mem w words i)
            done;
            os (sprintf "    }\n")
        | _ -> failwith "error while compiling mem update"
    in

    let mask_inputs s = mask (width s) (UidMap.find (uid s) data_map) in

    List.iter mask_inputs (Circuit.inputs circuit);
    List.iter compile schedule;
    List.iter compile regs;
    List.iter compile_mem_update mems;
    List.iter compile_reg_update regs;
    os "}\n\n"
 
(*****************************************************************)

