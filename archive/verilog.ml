(* 
 * hardcaml - hardware design in OCaml
 *
 *   (c) 2014 MicroJamJar Ltd
 *
 * Author(s): andy.ray@ujamjar.com
 * Description: 
 *
 *)

open Printf
open Signal.Types
open Signal.Comb

exception Failure of string
let failwith str = raise (Failure str)

(* tail recursive list concatenation *)
let (@) a b = 
    let a = List.rev a in
    let rec f a b =
        match b with 
        | [] -> a
        | h::t -> f (h :: a) t
    in
    List.rev (f a b)

let reserved = [
    "and"; "always"; "assign"; "attribute"; "begin"; "buf"; "bufif0"; "bufif1";
    "case"; "cmos"; "deassign"; "default"; "defparam"; "disable"; "else"; "endattribute";
    "end"; "endcase"; "endfunction"; "endprimitive"; "endmodule"; "endtable"; "endtask"; "event";
    "for"; "force"; "forever"; "fork"; "function"; "highhz0"; "highhz1"; "if";
    "initial"; "inout"; "input"; "integer"; "join"; "large"; "medium"; "module";
    "nand"; "negedge"; "nor"; "not"; "notif0"; "notif1"; "nmos"; "or"; "output";
    "parameter"; "pmos"; "posedge"; "primitive"; "pulldown"; "pullup"; "pull0"; "pull1";
    "rcmos"; "reg"; "release"; "repeat"; "rnmos"; "rpmos"; "rtran"; "rtranif0";
    "rtranif1"; "scalared"; "small"; "specify"; "specparam"; "strong0"; "strong1"; "supply0";
    "supply1"; "table"; "task"; "tran"; "tranif0"; "tranif1"; "time"; "tri";
    "triand"; "trior"; "trireg"; "tri0"; "tri1"; "vectored"; "wait"; "wand";
    "weak0"; "weak1"; "while"; "wire"; "wor"
]

let prefix = "_"
let iteri f l = 
    let rec iteri n l =
        match l with
        | [] -> ()
        | h::t -> f n h; iteri (n+1) t
    in
    iteri 0 l

let folds c s = List.fold_left (fun s n -> if s = "" then n else n ^ c ^  s) "" s 

let write circuit = 
    let inputs = Circuit.inputs circuit in
    let outputs = Circuit.outputs circuit in

    let buf = Buffer.create (10*1024) in

    (* create mangled names *)
    let name = Circuit.mangle_names reserved prefix circuit in 

    (* writing signal declarations *)
    let write_signal_decl typ signal =
        let write typ name ass = 
            if width signal = 1 then bprintf buf "%s %s%s;\n" typ name ass
            else bprintf buf "%s [%d:0] %s%s;\n" typ (width signal - 1) name ass in
        let names = names signal in
        let name = name (uid signal) in
        let name0 = name 0 in
        write typ name0 "";
        if List.length names > 1 then
            iteri (fun i _ -> write "  wire" (name (i+1)) (" = " ^ name0)) (List.tl names)
    in
    let write_mem_decl s = 
        match s with
        | Signal_mem(id,mid,r,sp) ->
            bprintf buf "  reg [%d:0] %s[0:%d];\n" 
                (width s - 1) (name mid 0) (sp.mem_size - 1)
        | _ -> failwith "can only write memories here"
    in

    (* write header *)
    bprintf buf "module %s (" (Circuit.name circuit);
    bprintf buf "%s" (folds ", " (List.map (fun s -> List.hd (names s)) (inputs@outputs)));
    bprintf buf ");\n";
    List.iter (write_signal_decl "  input") inputs;
    List.iter (write_signal_decl "  output") outputs;

    let is_internal_signal s = not (Circuit.is_input circuit s) && 
                               not (Circuit.is_output circuit s) && 
                               not (s = empty) in 
    let internal_signals = Circuit.filter is_internal_signal outputs in

    (* internal declarations *)
    List.iter 
        (fun s -> 
            if is_mem s then write_mem_decl s;
            write_signal_decl (if is_op Signal_mux s || is_reg s then "  reg" else "  wire") s)  
        internal_signals;
            
    (* write logic *)
    let write_logic s = 
        let dep = List.nth (deps s) in
        let nameuid u = name u 0 in
        let name s = name (uid s) 0 in
        let edge s = if s = vdd then "posedge" else "negedge" in
        let cat s = fst (List.fold_left (fun (s,i) d -> 
            if i=0 then name d,i+1 else s ^ ", " ^ name d,i+1) ("",0) (deps s)) in
        let mux s =
            let deps = deps s in
            let sel = List.hd deps in
            let vals = List.tl deps in
            let nvals = List.length vals in
            let name_s = name s in
            bprintf buf "  always @*\n    case (%s)\n" (name sel);
            iteri (fun i d ->
                if (i+1) <> nvals then
                    (bprintf buf "    %i'd%i: %s <= %s;\n" (width sel) i name_s (name d)) 
                else
                    (bprintf buf "    default: %s <= %s;\n" name_s (name d)) 
            ) vals;
            bprintf buf "    endcase\n"
        in
        let reg s = 
            let d = List.hd (deps s) in
            match s with 
            | Signal_reg(_,r) ->
            begin
                if r.reg_reset = empty then
                    bprintf buf "  always @(%s %s)\n" (edge r.reg_clock_level) (name r.reg_clock)
                else
                begin
                    bprintf buf "  always @(%s %s, %s %s)\n" 
                        (edge r.reg_clock_level) (name r.reg_clock) (edge r.reg_reset_level) (name r.reg_reset);
                    bprintf buf "    if (%s == %i)" (name r.reg_reset) (if r.reg_reset_level = vdd then 1 else 0);
                    bprintf buf " %s <= %s; else\n" (name s) (name r.reg_reset_value)
                end;
                if r.reg_clear <> empty then
                begin
                    bprintf buf "    if (%s == %i)" (name r.reg_clear) (if r.reg_clear_level = vdd then 1 else 0);
                    bprintf buf " %s <= %s; else\n" (name s) (name r.reg_clear_value)
                end;
                if r.reg_enable = vdd then
                    bprintf buf "      %s <= %s;\n" (name s) (name d)
                else
                    bprintf buf "    if (%s) %s <= %s;\n" (name r.reg_enable) (name s) (name d)
            end
            | _ -> failwith "Cannot write non-register value here"
        in
        let mem s = 
            match s with 
            | Signal_mem(id,muid,r,m) ->
            begin
                let d = dep 0 in
                if r.reg_reset = empty then
                    bprintf buf "  always @(%s %s)\n" (edge r.reg_clock_level) (name r.reg_clock)
                else
                begin
                    bprintf buf "  always @(%s %s, %s %s)\n" 
                        (edge r.reg_clock_level) (name r.reg_clock) (edge r.reg_reset_level) (name r.reg_reset);
                    bprintf buf "    if (%s == %i)\n" (name r.reg_reset) (if r.reg_reset_level = vdd then 1 else 0);
                    bprintf buf "    begin\n";
                    for i=0 to m.mem_size-1 do
                        bprintf buf "      %s[%i] <= %s;\n" (nameuid muid) i
                            (name r.reg_reset_value)
                    done;
                    bprintf buf "    end\n";
                    bprintf buf "    else\n";
                end;
                if r.reg_clear <> empty then
                begin
                    bprintf buf "    if (%s == %i)\n" (name r.reg_clear) (if r.reg_clear_level = vdd then 1 else 0);
                    bprintf buf "    begin\n";
                    for i=0 to m.mem_size-1 do
                        bprintf buf "      %s[%i] <= %s;\n" (nameuid muid) i
                            (name r.reg_clear_value)
                    done;
                    bprintf buf "    end\n";
                    bprintf buf "    else\n";
                end;
                if r.reg_enable = vdd then
                    bprintf buf "      %s[%s] <= %s;\n" 
                        (nameuid muid) (name m.mem_write_address) (name d)
                else
                    bprintf buf "    if (%s) %s[%s] <= %s;\n" 
                        (name r.reg_enable) (nameuid muid) (name m.mem_write_address) (name d);
                bprintf buf "  assign %s = %s[%s];\n" 
                    (name s) (nameuid muid) (name m.mem_read_address)
            end
            | _ -> failwith "Cannot write non-memory value here"
        in 
        let inst s = 
            match s with
            | Signal_inst(id,iuid,i) ->
            begin
                let rec folds = function
                    | [] -> ""
                    | s :: [] -> s
                    | h :: t -> h ^ ", " ^ (folds t)
                in
                let generic (n,g) = 
                    "." ^ n ^ "(" ^ 
                        (match g with
                        | ParamString(s) -> "\"" ^ s ^ "\""
                        | ParamInt(i) -> string_of_int i
                        | ParamBool(b) -> if b then "1" else "0"
                        | ParamFloat(f) -> string_of_float f) ^ ")"
                in
                let inport (n,s) = "." ^ n ^ "(" ^ (name s) ^ ")" in
                let outport (n,(w,l)) = 
                    if width s = 1 then
                    "." ^ n ^ "(" ^ (name s) ^ ")"
                    else
                    "." ^ n ^ "(" ^ (name s) ^ 
                    "[" ^ string_of_int (w+l-1) ^ ":" ^ string_of_int l ^ "])" 
                in
                let generics = List.map generic i.inst_generics in
                let inports = List.map inport i.inst_inputs in
                let outports = List.map outport i.inst_outputs in
                bprintf buf "  %s\n" i.inst_name;
                if i.inst_generics <> [] then
                    bprintf buf "    #( %s )\n" (folds generics);
                bprintf buf "    %s\n" (nameuid iuid);
                bprintf buf "    ( %s );\n" (folds (inports @ outports));
            end
            | _ -> failwith "expecting an instantiation"
        in
        match s with
        | Signal_empty -> failwith "Can't write empty signal"
        | Signal_const(_,v) ->
            bprintf buf "  assign %s = %i'b%s;\n" (name s) (width s) v
        | Signal_wire(_,d) ->
            bprintf buf "  assign %s = %s;\n" (name s) (name !d)
        | Signal_select(_,h,l) ->
            bprintf buf "  assign %s = %s[%d:%d];\n" (name s) (name (dep 0)) h l
        | Signal_op(_,op) ->
        begin
            let op2 op = bprintf buf "  assign %s = %s %s %s;\n" (name s) (name (dep 0)) op (name (dep 1)) in
            match op with
            | Signal_add -> op2 "+"
            | Signal_sub -> op2 "-"
            | Signal_mulu -> op2 "*"
            | Signal_muls -> bprintf buf "  assign %s = $signed(%s) * $signed(%s);\n" 
                                        (name s) (name (dep 0)) (name (dep 1))  
            | Signal_and -> op2 "&"
            | Signal_or -> op2 "|"
            | Signal_xor -> op2 "^"
            | Signal_eq -> op2 "=="
            | Signal_not -> bprintf buf "  assign %s = ~ %s;\n" (name s) (name (dep 0)) 
            | Signal_lt -> op2 "<"
            | Signal_cat ->  bprintf buf "  assign %s = { %s };\n" (name s) (cat s)
            | Signal_mux -> mux s 
        end
        | Signal_reg(_,_) -> reg s
        | Signal_mem(_,_,_,_) -> mem s
        | Signal_inst(_) -> inst s
    in
    List.iter write_logic (internal_signals @ outputs);

    bprintf buf "endmodule\n";

    buf

module Testbench = 
struct

    let write clocks resets circuit = 
        let buf = Buffer.create (10*1024) in
        let inputs = Circuit.inputs circuit in
        let outputs = Circuit.outputs circuit in
        let name s = List.hd (names s) in
        let write_signal_decl t s = 
            if width s = 1 then
                bprintf buf "  %s %s;\n" t (name s)
            else
                bprintf buf "  %s [%i:0] %s;\n" t (width s) (name s)
        in
        let write_reg = write_signal_decl "reg" in
        let write_wire = write_signal_decl "wire" in
        bprintf buf "module %s_tb;\n" (Circuit.name circuit);
        (* write the net declarations *)
        List.iter write_reg inputs;
        List.iter write_wire outputs;
        (* DUT *)
        let write_assoc s =
            folds ", " (List.map (fun s -> 
                "." ^ (name s) ^ "(" ^ (name s) ^ ")") s)
        in
        bprintf buf "  /* design under test */\n";
        bprintf buf "  %s the_dut (\n" (Circuit.name circuit);
        bprintf buf "    %s\n" 
            (write_assoc (inputs @ outputs));
        bprintf buf "  );\n";

        let clocks, inputs = 
            List.partition (fun s -> List.mem (name s) clocks) inputs
        in
        let resets, inputs = 
            List.partition (fun s -> List.mem (name s) resets) inputs
        in
    
        (* drive stimulus *)
        bprintf buf "  /* stimulus */\n";
        bprintf buf "  initial begin\n";
        bprintf buf "    /* async reset */\n";
        List.iter (fun s ->
        bprintf buf "    %s <= 0;\n" (name s);
        ) clocks;
        List.iter (fun s ->
        bprintf buf "    %s <= 1;\n" (name s);
        ) resets;
        bprintf buf "    #10;\n";
        List.iter (fun s ->
        bprintf buf "    %s <= 0;\n" (name s);
        ) resets;
        bprintf buf "    #10;\n";
        bprintf buf "    /* clocking */\n";
        bprintf buf "    while (1) begin\n";
        List.iter (fun s ->
        bprintf buf "      %s <= 1;\n" (name s);
        ) clocks;
        List.iter (fun s ->
        bprintf buf "      $scanf(\"%%b\", %s);\n" (name s);
        ) inputs;
        bprintf buf "      #10;\n";
        List.iter (fun s ->
        bprintf buf "      %s <= 0;\n" (name s);
        ) clocks;
        bprintf buf "      #10;\n";
        bprintf buf "    end\n";
        bprintf buf "  end\n";

        bprintf buf "endmodule\n";

        buf

end

