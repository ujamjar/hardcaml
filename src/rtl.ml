(* 
 * hardcaml - hardware design in OCaml
 *
 *   (c) 2014 MicroJamJar Ltd
 *
 * Author(s): andy.ray@ujamjar.com
 * Description: 
 *
 *)

(* generic rtl writing for vhdl/verilog *)

open Astring
open Signal.Types
open Signal.Comb 
open Signal.Seq 

(* utils *)

type io = string -> unit
type name = signal -> string

type type_decl = 
    | Input 
    | Output 
    | Wire 
    | Reg 
    | Mem of int
    | Constant of string

let tab n = String.v ~len:n (fun _ -> ' ')
let t4 = tab 4 
let t8 = tab 8 

let write_strings io f s = 
    let s = Array.of_list s in
    let len = Array.length s in
    for i=0 to len - 1 do
        io (f (i=0) (i=len-1) s.(i))
    done

let rec sep s l = 
    match l with
    | [] -> ""
    | [a] -> a
    | a::b -> a ^ s ^ sep s b

let str_map f s = 
    let l = String.length s in
    let x = Bytes.create l in
    for i=0 to l-1 do
        Bytes.set x i (f s.[i])
    done;
    Bytes.to_string x

let is_alpha c = (c >= 'a' && c <= 'z') ||
                 (c >= 'A' && c <= 'Z')
let is_num c = c >= '0' && c <= '9'
let is_alpha_num c = is_alpha c || is_num c

(* name manager *)

module type OrderedString = (Map.OrderedType with type t = string)

module CaseSensitive = struct
    type t = string
    let compare a b = compare a b
end
module CaseInsensitive = struct
    type t = string
    let compare a b = 
        compare (String.Ascii.uppercase a) (String.Ascii.uppercase b)
end

module type Mangler = sig
    module SM : Map.S 
    val empty : string SM.t
    val lookup : string -> int SM.t -> int option
    val mangle : string -> int SM.t  -> int SM.t * string
end

module Mangler(Ord : OrderedString) = struct

    module SM = Map.Make(Ord)

    let empty  = SM.empty

    let lookup name map = 
        try Some(SM.find name map) with Not_found -> None

    let rec mangle name map = 
        match lookup name map with
        | Some(x) ->
            mangle (name ^ "_" ^ string_of_int x) (SM.add name (x+1) map) 
        | None ->
            SM.add name 0 map, name

end

(* control mapping of signals to their (various) names *)
module type SignalNaming = sig
    (* identifier case sensitivity *)
    module Case : OrderedString
    (* given a name, turn it into a legal identifier *)
    val prefix : string
    val reserved : string list
    val legalize : string -> string
end

module VerilogNames = struct
    module Case = CaseSensitive
    let prefix = "_"
    let reserved = [
        "and"; "always"; "assign"; "attribute"; "begin"; "buf"; "bufif0"; 
        "bufif1"; "case"; "cmos"; "deassign"; "default"; "defparam"; "disable"; 
        "else"; "endattribute"; "end"; "endcase"; "endfunction"; "endprimitive";
        "endmodule"; "endtable"; "endtask"; "event"; "for"; "force"; "forever"; 
        "fork"; "function"; "highhz0"; "highhz1"; "if"; "initial"; "inout"; 
        "input"; "integer"; "join"; "large"; "medium"; "module"; "nand"; 
        "negedge"; "nor"; "not"; "notif0"; "notif1"; "nmos"; "or"; "output";
        "parameter"; "pmos"; "posedge"; "primitive"; "pulldown"; "pullup"; 
        "pull0"; "pull1"; "rcmos"; "reg"; "release"; "repeat"; "rnmos"; 
        "rpmos"; "rtran"; "rtranif0"; "rtranif1"; "scalared"; "small"; 
        "specify"; "specparam"; "strong0"; "strong1"; "supply0";
        "supply1"; "table"; "task"; "tran"; "tranif0"; "tranif1"; "time"; "tri";
        "triand"; "trior"; "trireg"; "tri0"; "tri1"; "vectored"; "wait"; "wand";
        "weak0"; "weak1"; "while"; "wire"; "wor";
    ]
    let rec legalize name = 
        (* alpha or '_' are legal staring identifiers.  if this is not
         * the case add the prefix and try again *)
        if is_alpha name.[0] || name.[0] == '_' then
            (* alpha,num,_,$ are ok, replace invalid chars with '_' *)
            str_map (fun c ->
                if is_alpha c || is_num c || c = '_' || c = '$' then c
                else '_') name
        else
            legalize (prefix ^ name)
end

module VhdlNames = struct
    module Case = CaseInsensitive
    let prefix = "hc_"
    let reserved = [
        "abs"; "access"; "after"; "alias"; "all"; "and"; "architecture";
        "array"; "assert"; "attribute"; "begin"; "block"; "body"; "buffer";
        "bus"; "case"; "component"; "configuration"; "constant"; "disconnect";
        "downto"; "else"; "elsif"; "end"; "entity"; "exit"; "file"; "for";
        "function"; "generate"; "generic"; "group"; "guarded"; "if"; "impure";
        "in"; "inertial"; "inout"; "is"; "label"; "library"; "linkage";
        "literal"; "loop"; "map"; "mod"; "nand"; "new"; "next"; "nor"; "not";
        "null"; "of"; "on"; "open"; "or"; "others"; "out"; "package"; "port";
        "postponed"; "procedure"; "process"; "pure"; "range"; "record";
        "register"; "reject"; "return"; "rol"; "ror"; "select"; "severity";
        "signal"; "shared"; "sla"; "sli"; "sra"; "srl"; "subtype"; "then";
        "to"; "transport"; "type"; "unaffected"; "units"; "until"; "use";
        "variable"; "wait"; "when"; "while"; "with"; "xnor"; "xor";
        (* conversion functions used within code. *)
        prefix ^ "uns"; prefix ^ "sgn"; prefix ^ "sl"; prefix ^ "slv";
    ]
    let rec legalize name = 
        (* alpha are legal staring identifiers.  if this is not
         * the case add the prefix and try again *)
        if is_alpha name.[0] then
            (* alpha,num,_ are ok, replace invalid chars with '_' *)
            str_map (fun c ->
                if is_alpha c || is_num c || c = '_' then c
                else '_') name
        else
            legalize (prefix ^ name)
end

module type SignalNameManager = sig
    module M : Mangler 
    module UI : sig
        type t = uid * int
        val compare : t -> t -> int
    end
    module SMap : Map.S 

    type mem_names = 
        {
            arr : string;
            typ : string;
            t1 : string;
            t2 : string;
        }

    type name_map = 
        {
            mangler : int M.SM.t;
            signals : string SMap.t;
            mem : mem_names UidMap.t;
            inst_labels : string UidMap.t;
        }

    val prefix : string
    val reserved : string list

    val init : string list -> name_map
    val add_port : signal -> name_map -> name_map
    val add_signal : signal -> name_map -> name_map

    val signal_name : name_map -> signal -> int -> string
    val mem_names : name_map -> signal -> mem_names
    val inst_label : name_map -> signal -> string

end

module SignalNameManager(S : SignalNaming) = struct
    module M = Mangler(S.Case)
    module UI = struct
        type t = uid * int
        let compare = compare
    end
    module SMap = Map.Make(UI)

    let prefix = S.prefix
    let reserved = S.reserved

    type mem_names = 
        {
            arr : string;
            typ : string;
            t1 : string;
            t2 : string;
        }

    type name_map = 
        {
            mangler : int M.SM.t;
            signals : string SMap.t;
            mem : mem_names UidMap.t;
            inst_labels : string UidMap.t;
        }

    (* mangle a name *)
    let mangle name nm = 
        let map, name = M.mangle name nm.mangler in
        { nm with mangler = map }, name

    let generate_name signal = 
        S.prefix ^ Int64.to_string (uid signal)

    (* get list of names of signal, or auto generate one *)
    let names_of_signal signal = 
        let n = names signal in
        if n=[] then [generate_name signal]
        else n

    let add_signal_name uid idx name nm = 
        (* mangle *)
        let nm, name = mangle name nm in
        (* add to signal map *)
        { nm with signals = SMap.add (uid,idx) name nm.signals }

    let add_signal_names signal nm = 
        (* get names and legalize *)
        let names = names_of_signal signal in
        let names = List.map S.legalize names in
        (* add names to map *)
        fst (List.fold_left 
            (fun (nm,idx) name -> 
                add_signal_name (uid signal) idx name nm, (idx+1))
            (nm,0) names)

    (* add reserved names to the mangler *)
    let init resv = 
        {
            mangler = 
                List.fold_left (fun m r -> fst (M.mangle r m)) M.empty resv;
            signals = SMap.empty;
            mem = UidMap.empty;
            inst_labels = UidMap.empty;
        }

    (* add port names, but ensure they are unique and legal 
     * and throw and error if not *)
    let add_port signal nm = 
        match names signal with
        | [] -> failwith "port has no name"
        | name::[] -> begin
            if S.legalize name <> name then 
                failwith ("port name " ^ name ^ " is not legal")
            else
                match M.lookup name nm.mangler with
                | Some(_) -> failwith ("port name '" ^ name ^ 
                                       "' has already been defined" ^ 
                                       " or is reserved") 
                | None -> add_signal_name (uid signal) 0 name nm
        end
        | a::b -> failwith ("port has multiple names " ^ sep ", " (a::b))

    let add_mem signal nm = 
        let name = List.hd (names_of_signal signal) in
        let nm, name_arr = mangle (name ^ "_mem") nm in
        let nm, name_typ = mangle (name ^ "_type") nm in
        let nm, name_t1 = mangle (name ^ "_blk") nm in
        let nm, name_t2 = mangle (name ^ "_idx") nm in
        { nm with
            mem = UidMap.add (uid signal) 
                {
                    arr = name_arr;
                    typ = name_typ;
                    t1 = name_t1;
                    t2 = name_t2;
                } nm.mem;
        }

    let add_inst iname signal nm = 
        let nm, name = mangle ("the_" ^ iname) nm in
        { nm with inst_labels = UidMap.add (uid signal) name nm.inst_labels }

    (* add signals names to map; deal with special cases *)
    let add_signal signal nm =
        (* signal names *)
        let nm = add_signal_names signal nm in
        (* special cases *)
        let nm = 
            match signal with
            | Signal_mem(_) -> add_mem signal nm
            | Signal_inst(_,_,i) -> add_inst i.inst_name signal nm
            | _ -> nm
        in
        nm

    let signal_name nm signal idx = 
        let uid = uid signal in
        try
            SMap.find (uid, idx) nm.signals
        with _ -> failwith ("cant find name[" ^ string_of_int idx ^ "] of " ^ 
                             to_string signal)

    let mem_names nm signal = 
        let uid = uid signal in
        try UidMap.find uid nm.mem
        with _ -> failwith ("cant find mem_type of " ^ to_string signal)

    let inst_label nm signal = 
        let uid = uid signal in
        try UidMap.find uid nm.inst_labels
        with _ -> failwith ("cant find inst_label of " ^ to_string signal)

end

(* regiser/memory process/always helper *)

module Process = struct

    (* helper for writing registers neatly *)
    type level = Rising | Falling | High | Low
    type stat = 
        | If of Signal.Types.signal * level * stat * stat
        | Assign of Signal.Types.signal * Signal.Types.signal
        | Empty

    let make_reg ?(clock=true) r s = 
        let q = s in
        let d = List.hd (deps s) in
        let lev s = 
            if s = empty || s = vdd then High
            else Low
        in
        let edge s = 
            if s = empty || s = vdd then Rising
            else Falling
        in
        (* main assignment *)
        let e = Assign(q, d) in
        (* enable *)
        let e = 
            if r.reg_enable = empty || r.reg_enable = vdd then e
            else If(r.reg_enable, High, e, Empty)
        in
        (* clear *)
        let e = 
            if r.reg_clear = empty then e
            else If(r.reg_clear, lev r.reg_clear_level, 
                        Assign(q, r.reg_clear_value), e)
        in
        (* reset and clock *)
        let clock e = 
            if clock then If(r.reg_clock, edge r.reg_clock_level, e, Empty)
            else e
        in
        let e = 
            if r.reg_reset = empty then clock e
            else 
                If(r.reg_reset, lev r.reg_reset_level,
                        Assign(q, r.reg_reset_value), 
                        clock e)
        in
        e

end

(* vhdl/verilog config *)

module type Rtl = sig

    open Signal.Types

    module Names : SignalNameManager

    val comment : string -> string

    val header_and_ports : io -> string -> 
        (string*int) list -> (string*int) list -> unit

    val signal_decl : io -> string -> signal -> unit

    val alias_decl : io -> string -> signal -> unit

    val mem_decl : io -> Names.mem_names -> signal -> unit

    val start_logic : io -> unit

    val logic : 
        io -> name -> signal -> unit

    val logic_mem : io -> name -> Names.mem_names -> signal ->
            register -> memory -> unit

    val logic_inst : io -> name -> string -> signal -> instantiation -> unit
    
    val assign : io -> string -> string -> unit
            
    val end_logic : io -> unit

end


module VerilogCore : Rtl = struct

    module Names = SignalNameManager(VerilogNames)

    let string_of_type = function
        | Input -> "input"
        | Output -> "output"
        | Wire -> "wire"
        | Reg -> "reg"
        | Mem(_) -> "mem"
        | Constant(_) -> "wire"

    let comment s = "/* " ^ s ^ " */"

    let decl t n b = 
        let decl s n b = 
            if b=1 then
                s ^ " " ^ n
            else
                s ^ " [" ^ string_of_int (b - 1) ^ ":0] " ^ n
        in
        match t with
        | Constant(v) ->
            decl (string_of_type t) n b ^ " = " ^ string_of_int b ^ "'b" ^ v
        | _ ->
            decl (string_of_type t) n b

    let put_decl io t n b = io (decl t n b ^ ";\n")

    let header_and_ports io name i o = 
        let module_port _ last (s,_) =
            if last then t4 ^ s ^ "\n"
            else t4 ^ s ^ "," ^ "\n"
        in
        let decl_port t _ _ (s,b) = t4 ^ decl t s b ^ ";\n" in
        io ("module " ^ name ^ " (\n");
        write_strings io module_port (i @ o);
        io (");\n\n");
        write_strings io (decl_port Input) i;
        write_strings io (decl_port Output) o;
        io ("\n")

    let signal_decl io name s = 
        match s with
        | Signal_empty -> failwith "unexpected empty signal"
        | Signal_op(_,op) when op = Signal_mux -> begin
            if List.length (deps s) = 3 then
                io (t4 ^ decl Wire name (width s) ^ ";\n")
            else
                io (t4 ^ decl Reg name (width s) ^ ";\n")
        end
        | Signal_op(_) | Signal_wire(_) 
        | Signal_select(_) | Signal_inst(_) ->
            io (t4 ^ decl Wire name (width s) ^ ";\n")
        | Signal_reg(_) ->
            io (t4 ^ decl Reg name (width s) ^ ";\n")
        | Signal_const(_,v) ->
            io (t4 ^ decl (Constant(v)) name (width s) ^ ";\n")
        | Signal_mem(_) -> 
            io (t4 ^ decl Wire name (width s) ^ ";\n")

    let alias_decl io name s = 
        io (t4 ^ decl Wire name (width s) ^ ";\n")

    let mem_decl io mem s = 
        let open Names in
        match s with
        | Signal_mem(_,_,_,sp) -> 
            let b = string_of_int (width s - 1) in
            let s = string_of_int (sp.mem_size - 1) in
            io (t4 ^ "reg [" ^ b ^ ":0] " ^ mem.arr ^ "[0:" ^ s ^ "];\n")
        | _ -> failwith "expecting memory"

    let start_logic io = ()

    let clocked io s r name assign = 
        let open Process in
        let level n = function
            | Rising | High -> "(" ^ n ^ ")"
            | Falling | Low -> "(" ^ n ^ " == 0" ^ ")"
        in
        let rec write_reg tab r = 
            match r with
            | Empty -> ()
            | If(c,v,t,f) ->
                io (tab ^ "if " ^ level (name c) v ^ "\n");
                write_reg (tab ^ t4) t;
                if f<>Empty then begin
                    io (tab ^ "else\n");
                    write_reg (tab ^ t4) f;
                end
            | Assign(q,d) ->
                assign tab q d
        in
        let edge s l = 
            (if l = vdd then "posedge " else "negedge ") ^ name s 
        in
        let edges = 
            if r.reg_reset = empty then 
                [ edge r.reg_clock r.reg_clock_level ] 
            else
                [ edge r.reg_clock r.reg_clock_level;
                  edge r.reg_reset r.reg_reset_level ]
        in
        let edges = sep " or " edges in
        io (t4 ^ "always @(" ^ edges ^ ") begin\n");
        write_reg t8 (make_reg ~clock:false r s);
        io (t4 ^ "end\n")
        
    let logic io name s = 
        let dep n = List.nth (deps s) n in
        let sn = name s in
        let binop op = 
            let a = name (dep 0) in
            let b = name (dep 1) in
            io (t4 ^ "assign " ^ sn ^ " = " ^ 
                    a ^ " " ^ op ^ " " ^ b ^ ";\n") 
        in
        let sbinop op = 
            let a = name (dep 0) in
            let b = name (dep 1) in
            let a,b = "$signed("^a^")", "$signed("^b^")" in
            io (t4 ^ "assign " ^ sn ^ " = " ^ 
                    a ^ " " ^ op ^ " " ^ b ^ ";\n") 
        in

        match s with
        | Signal_empty -> failwith "unexpected empty signal"
        | Signal_op(id,op) -> begin
            match op with
            | Signal_add -> binop "+"
            | Signal_sub -> binop "-"
            | Signal_mulu -> binop "*"
            | Signal_muls -> sbinop "*"
            | Signal_and -> binop "&"
            | Signal_or -> binop "|"
            | Signal_xor -> binop "^"
            | Signal_eq -> binop "=="
            | Signal_not -> 
                io (t4 ^ "assign " ^ sn ^ " = ~ " ^ 
                                (name (dep 0)) ^ ";\n")
            | Signal_lt -> binop "<"
            | Signal_cat -> 
                let cat = sep ", " 
                    (List.map (fun s -> name s) (deps s)) 
                in
                io (t4 ^ "assign " ^ sn ^ " = { " ^ cat ^ " };\n")
            | Signal_mux -> 
                let switch,cases = List.hd (deps s), List.tl (deps s) in
                let n = List.length cases in
                if n=2 then
                    io (t4 ^ "assign " ^ sn ^ " = " ^
                        name switch ^ " ? " ^
                        name (dep 2) ^ " : " ^
                        name (dep 1) ^ ";\n")

                else begin
                    io (t4 ^ "always @* begin\n");
                    io (t8 ^ "case (" ^ name switch ^ ")\n");
                    Utils.iteri (fun i s ->
                        (if i <> n-1 then
                            io (t8 ^ string_of_int i ^ ": ")
                        else
                            io (t8 ^ "default" ^ ": "));
                        io (sn ^ " <= " ^ name s ^ ";\n")
                    ) cases;
                    io (t8 ^ "endcase\n");
                    io (t4 ^ "end\n")
                end
        end
        | Signal_wire(id,d) -> begin
            io (t4 ^ "assign " ^ sn ^ " = " ^ name (!d) ^ ";\n")
        end
        | Signal_reg(id,r) -> begin
            clocked io s r name
                (fun tab q d -> 
                    io (tab ^ name q ^ " <= " ^ name d ^ ";\n"))
        end
        | Signal_select(id,h,l) -> begin
            io (t4 ^ "assign " ^ sn ^ " = " ^ 
                (name (dep 0)) ^ "[" ^ 
                (string_of_int h) ^ ":" ^ 
                (string_of_int l) ^ "];\n")
        end
        | Signal_const(id,v) -> () (* already done *)
        (* these are done seperately *)
        | Signal_mem(_,muid,r,sp) -> failwith "logic mem"
        | Signal_inst(_) -> failwith "logic inst" 

    let logic_mem io name mem s r sp = 
        let open Names in
        let sn = name s in
        clocked io s r name
            (fun tab q d -> 
                let d' = uid (List.hd (deps s)) in
                if d' = uid d then begin
                    let wa = name sp.mem_write_address in
                    io (tab ^ mem.arr ^ "[" ^ wa ^ "] <= " ^ 
                        name d ^ ";\n")
                end else begin (* some reset/clear clause *)
                    let i = mem.t2 in
                    io (tab ^ "begin: " ^ mem.t1 ^ "\n");
                    io (tab ^ t4 ^ "integer " ^ i ^ ";\n");
                    io (tab ^ t4 ^ "for (" ^ i ^ "=0; " ^ i ^ "<" ^ 
                        string_of_int sp.mem_size ^ "; " ^ 
                        i ^ "=" ^ i ^ "+1)\n");
                    io (tab ^ t8 ^ mem.arr ^ "[" ^ i ^ "] <= " ^ 
                            name d ^ ";\n");
                    io (tab ^ "end\n")
                end);
        (* read *)
        let a = name sp.mem_read_address in
        io (t4 ^ "assign " ^ sn ^ " = " ^ mem.arr ^ "[" ^ a ^ "];\n")

    let logic_inst io name inst_name s i = 
        let open Signal.Instantiation in
        io (t4 ^ i.inst_name ^ "\n");
        let assoc n v = "." ^ n ^ "(" ^ v ^ ")" in
        (* parameters *)
        let param_string = function
            | ParamString(v) -> "\"" ^ v ^ "\""
            | ParamInt(v) -> string_of_int v
            | ParamFloat(v) -> string_of_float v
            | ParamBool(v) -> if v then "1" else "0" 
        in
        if i.inst_generics <> [] then begin
            let generics = 
                let generic (s,p) = assoc s (param_string p) in
                sep ", " (List.map generic i.inst_generics) 
            in
            io (t8 ^ "#( " ^ generics ^ " )\n")
        end;
        io (t8 ^ inst_name ^ "\n");
        (* ports *)
        let in_ports = 
            List.map (fun (n,s) -> assoc n (name s)) i.inst_inputs
        in
        let out_ports = 
            List.map (fun (n,(w,l)) ->
                assoc n ((name s) ^ "[" ^ string_of_int (w+l-1) ^ ":" ^
                                          string_of_int l ^ "]"))
                i.inst_outputs
        in
        io (t8 ^ "( " ^ sep ", " (in_ports @ out_ports) ^ " );\n")
            
    let assign io t f = 
        io (t4 ^ "assign " ^ t ^ " = " ^ f ^ ";\n")

    let end_logic io = io ("endmodule\n")

end

module VhdlCore : Rtl = struct

    module Names = SignalNameManager(VhdlNames)

    let conversions = 
        let p = Names.prefix in
    [
        "function " ^ p ^ "uns(a : std_logic)        return unsigned         is variable b : unsigned(0 downto 0); begin b(0) := a; return b; end;";
        "function " ^ p ^ "uns(a : std_logic_vector) return unsigned         is begin return unsigned(a); end;";
        "function " ^ p ^ "sgn(a : std_logic)        return signed           is variable b : signed(0 downto 0); begin b(0) := a; return b; end;";
        "function " ^ p ^ "sgn(a : std_logic_vector) return signed           is begin return signed(a); end;";
        "function " ^ p ^ "sl (a : std_logic_vector) return std_logic        is begin return a(a'right); end;";
        "function " ^ p ^ "sl (a : unsigned)         return std_logic        is begin return a(a'right); end;";
        "function " ^ p ^ "sl (a : signed)           return std_logic        is begin return a(a'right); end;";
        "function " ^ p ^ "sl (a : boolean)          return std_logic        is begin if a then return '1'; else return '0'; end if; end;";
        "function " ^ p ^ "slv(a : std_logic_vector) return std_logic_vector is begin return a; end;";
        "function " ^ p ^ "slv(a : unsigned)         return std_logic_vector is begin return std_logic_vector(a); end;";
        "function " ^ p ^ "slv(a : signed)           return std_logic_vector is begin return std_logic_vector(a); end;";
    ]

    let comment s = "-- " ^ s

    let string_of_type = function
        | Input -> "input"
        | Output -> "output"
        | Wire -> "signal"
        | Reg -> "signal"
        | Mem(_) -> "signal"
        | Constant(_) -> "constant"

    let const_str b v = 
        let q = if b=1 then "'" else "\"" in
        q ^ v ^ q

    let decl t n b = 
        let decl_slv b = 
            if b=1 then "std_logic"
            else "std_logic_vector(" ^ string_of_int (b-1) ^ " downto 0)"
        in
        match t with
        | Constant(v) ->
               "constant " ^ n ^ " : " ^ decl_slv b ^ " := " ^ const_str b v
        | Input -> n ^ " : in " ^ decl_slv b
        | Output -> n ^ " : out " ^ decl_slv b
        | Wire -> "signal " ^ n ^ " : " ^ decl_slv b 
        | Reg -> "signal " ^ n ^ " : " ^ decl_slv b 
        | Mem(_) -> failwith "decl memory"

    let put_decl io t n b = io (decl t n b ^ ";\n")

    let header_and_ports io name i o = 
        let entity_in_port _ _ (s,n) = t8 ^ decl Input s n ^ ";\n"  in
        let entity_out_port _ last (s,n) = 
            t8 ^ decl Output s n ^ (if last then "\n" else ";\n")
        in
        io ("library ieee;\n");
        io ("use ieee.std_logic_1164.all;\n");
        io ("use ieee.numeric_std.all;\n\n");
        io ("entity " ^ name ^ " is\n");
        io ("    port (\n");
        write_strings io entity_in_port i;
        write_strings io entity_out_port o;
        io ("    );\n");
        io ("end entity;\n\n");
        io ("architecture rtl of " ^ name ^ " is\n\n");
        io (t4 ^ comment "conversion functions" ^ "\n");
        List.iter (fun s -> io (t4 ^ s ^ "\n")) conversions;
        io ("\n")

    let signal_decl io name s = 
        match s with
        | Signal_empty -> failwith "unexpected empty signal"
        | Signal_op(_) | Signal_wire(_) 
        | Signal_select(_) | Signal_inst(_) ->
            io (t4 ^ decl Wire name (width s) ^ ";\n")
        | Signal_reg(_) ->
            io (t4 ^ decl Reg name (width s) ^ ";\n")
        | Signal_const(_,v) ->
            io (t4 ^ decl (Constant(v)) name (width s) ^ ";\n")
        | Signal_mem(_) -> 
            io (t4 ^ decl Reg name (width s) ^ ";\n")

    let alias_decl io name s = 
        io (t4 ^ decl Wire name (width s) ^ ";\n")

    let mem_decl io mem s = 
        let open Names in
        match s with
        | Signal_mem(_,_,_,sp) -> 
            let b = string_of_int (width s - 1) in
            let sz = string_of_int (sp.mem_size - 1) in
            (* need a sepatate ID for the array type *)
            io (t4 ^ "type " ^ mem.typ ^ 
                " is array (0 to " ^ sz ^ ") of ");
            (if width s = 1 then io ("std_logic;\n")
            else io ("std_logic_vector(" ^ b ^ " downto 0);\n"));
            io (t4 ^ "signal " ^ mem.arr ^ " : " ^ mem.typ ^ ";\n")
        | _ -> failwith "expecting memory"

    let start_logic io = io ("begin\n\n")

    let clocked io s r name assign = 
        let open Process in
        let level n = function
            | High -> n ^ " = '1'"
            | Low -> n ^ " = '0'"
            | Rising -> "rising_edge(" ^ n ^ ")"
            | Falling -> "falling_edge(" ^ n ^ ")"
        in
        let rec write_reg tab r = 
            match r with
            | Empty -> ()
            | If(c,v,t,f) ->
                io (tab ^ "if " ^ level (name c) v ^ " then\n");
                write_reg (tab ^ t4) t;
                if f<>Empty then begin
                    io (tab ^ "else\n");
                    write_reg (tab ^ t4) f;
                end;
                io (tab ^ "end if;\n")
            | Assign(q,d) ->
                assign tab q d
        in
        let edges = 
            if r.reg_reset = empty then [ r.reg_clock ]
            else [ r.reg_clock; r.reg_reset ]
        in
        let edges = sep ", " (List.map (fun s -> name s) edges) in
        io (t4 ^ "process (" ^ edges ^ ") begin\n");
        write_reg t8 (make_reg r s);
        io (t4 ^ "end process;\n")


    let logic io name s = 
        let dep n = List.nth (deps s) n in
        let sn = name s in
        
        let dname n = name (dep n) in
        let udname n = Names.prefix ^ "uns(" ^ dname n ^ ")" in
        let sdname n = Names.prefix ^ "sgn(" ^ dname n ^ ")" in
        let slv str = 
            Names.prefix ^ (if width s=1 then "sl" else "slv") ^ "(" ^ str ^ ")"
        in

        let binop name op = 
            io (t4 ^ sn ^ " <= " ^ 
                    slv (name 0 ^ " " ^ op ^ " " ^ name 1) ^ ";\n") 
        in
        let sbinop = binop sdname in
        let binop = binop udname in

        match s with
        | Signal_empty -> failwith "unexpected empty signal"
        | Signal_op(id,op) -> begin
            match op with
            | Signal_add -> binop "+"
            | Signal_sub -> binop "-"
            | Signal_mulu -> binop "*"
            | Signal_muls -> sbinop "*"
            | Signal_and -> binop "and"
            | Signal_or -> binop "or"
            | Signal_xor -> binop "xor"
            | Signal_eq -> binop "="
            | Signal_not -> begin
                io (t4 ^ sn ^ " <= " ^ 
                        slv ("not " ^ udname 0) ^ ";\n") 
            end
            | Signal_lt -> binop "<"
            | Signal_cat -> begin
                let cat = sep " & " 
                    (List.map (fun s -> name s) (deps s)) 
                in
                io (t4 ^ sn ^ " <= " ^ cat ^ ";\n")
            end
            | Signal_mux -> begin
                let cases = List.tl (deps s) in
                let n = List.length cases in
                io (t4 ^ "with to_integer(" ^ udname 0 ^ ") select ");
                io (sn ^ " <= \n");
                Utils.iteri (fun i s ->
                    io (t8 ^ name s ^ " when ");
                    if i <> n-1 then io (string_of_int i ^ ",\n")
                    else io ("others;\n")
                ) cases
            end
        end
        | Signal_wire(id,d) -> 
            io (t4 ^ sn ^ " <= " ^ name (!d) ^ ";\n")
        | Signal_reg(id,r) -> begin
            clocked io s r name 
                (fun tab q d -> 
                    io (tab ^ name q ^ " <= " ^ name d ^ ";\n"))
        end
        | Signal_select(id,h,l) -> begin
            let sel = dname 0 ^ "(" ^ 
                string_of_int h ^ " downto " ^ 
                string_of_int l ^ ")"
            in
            let sel = if width s = 1 then slv sel else sel in
            io (t4 ^ sn ^ " <= " ^ sel ^ ";\n")
        end
        | Signal_const(id,v) -> () (* already done *)
        (* these are done seperately *)
        | Signal_mem(_) -> failwith "logic mem"
        | Signal_inst(_) -> failwith "logic inst"

    let logic_mem io name mem s r sp = 
        let open Names in
        let sn = name s in
        let to_integer s = 
            "to_integer(" ^ Names.prefix ^ "uns(" ^ s ^ "))"
        in
        clocked io s r name
            (fun tab q d -> 
                let d' = uid (List.hd (deps s)) in
                if d' = uid d then begin
                    let wa = name sp.mem_write_address in
                    io (tab ^ mem.arr ^ "(" ^ to_integer wa ^ ") <= " ^ 
                        name d ^ ";\n")
                end else begin (* some reset/clear clause *)
                    let i = mem.t2 in
                    io (tab ^ "for " ^ i ^ " in 0 to " ^ 
                        string_of_int (sp.mem_size - 1) ^ " loop\n");
                    io (tab ^ t4 ^ mem.arr ^ "(" ^ i ^ ") <= " ^ 
                            name d ^ ";\n");
                    io (tab ^ "end loop;\n")
                end);
        (* read *)
        let a = name sp.mem_read_address in
        io (t4 ^ sn ^ " <= " ^ mem.arr ^ "(" ^ to_integer a ^ ");\n")

    let logic_inst io name inst_name s i = 
        let open Signal.Instantiation in
        io (t4 ^ inst_name ^ ": entity " ^ 
            i.inst_lib ^ "." ^ i.inst_name ^ " (" ^ i.inst_arch ^ ")" ^ "\n");
        let assoc n v = n ^ " => " ^ v in
        (* parameters *)
        let param_string = function
            | ParamString(v) -> "\"" ^ v ^ "\""
            | ParamInt(v) -> string_of_int v
            | ParamFloat(v) -> string_of_float v
            | ParamBool(v) -> if v then "true" else "false" 
        in
        if i.inst_generics <> [] then begin
            let generics = 
                let generic (s,p) = assoc s (param_string p) in
                sep ", " (List.map generic i.inst_generics)
            in
            io (t8 ^ "generic map ( " ^ generics ^ ")\n")
        end;
        let in_ports = 
            List.map (fun (n,s) -> assoc n (name s)) i.inst_inputs
        in
        let out_ports = 
            if width s = 1 then (* special case - 1 output of 1 bit *)
                List.map (fun (n,(w,l)) -> assoc n (name s)) i.inst_outputs
            else
                List.map (fun (n,(w,l)) ->
                    let n = if w=1 then "std_logic_vector(" ^ n ^ ")" else n in
                    assoc n ((name s) ^ "(" ^ string_of_int (w+l-1) ^ 
                                        " downto " ^ string_of_int l ^ ")"))
                    i.inst_outputs
        in
        io (t8 ^ "port map ( " ^ sep ", " (in_ports @ out_ports) ^ " );\n")


    let assign io t f = 
        io (t4 ^ t ^ " <= " ^ f ^ ";\n")

    let end_logic io = 
        io ("end architecture;\n")

end

(* RTL writer *)

module type Rtl_S = sig
    val write : (string -> unit) -> Circuit.t -> unit
end

module Make(R : Rtl) = struct

    let write io circ = 
        let inputs, outputs = Circuit.inputs circ, Circuit.outputs circ in
        (* write signal declarations *)
        let is_internal s = not (Circuit.is_input circ s) && 
                            not (Circuit.is_output circ s) && 
                            not (s = empty) in 
        let internal_signals = Circuit.filter is_internal outputs in

        (* initialize the mangler *)
        let nm = R.Names.init R.Names.reserved in
        let add f nm x = List.fold_left (fun nm x -> f x nm) nm x in
        let nm = add R.Names.add_port nm inputs in
        let nm = add R.Names.add_port nm outputs in
        let nm = add R.Names.add_signal nm internal_signals in

        let primary_name s = R.Names.signal_name nm s 0 in
        let secondary_names s = 
            let l = List.length (names s) in
            if l < 2 then []
            else
                let n = Array.init (l-1) 
                        (fun i -> R.Names.signal_name nm s (i+1)) 
                in
                Array.to_list n
        in
        let primary s = primary_name s, width s in

        R.header_and_ports io 
            (Circuit.name circ)
            (List.map primary inputs)
            (List.map primary outputs);
        
        (* write internal declarations *)
        io (t4 ^ R.comment "signal declarations" ^ "\n");
        List.iter (fun s -> 
            (* primary signals *)
            R.signal_decl io (primary_name s) s;
            (* aliases *)
            List.iter (fun name ->
                R.alias_decl io name s) (secondary_names s);
            (* special declarations *)
            begin
                match s with
                | Signal_mem(_,u,_,_) -> begin
                    let mem_names = R.Names.mem_names nm s in
                    R.mem_decl io mem_names s
                end
                | _ -> ()
            end
        ) internal_signals;
        io ("\n");

        R.start_logic io;

        (* logic *)
        io (t4 ^ R.comment "logic" ^ "\n");
        List.iter (fun signal ->
            match signal with 
            | Signal_mem(_,_,r,m) ->
                let mem = R.Names.mem_names nm signal in
                R.logic_mem io primary_name mem signal r m
            | Signal_inst(_,_,i) ->
                let inst_name = R.Names.inst_label nm signal in
                R.logic_inst io primary_name inst_name signal i
            | _ ->
                R.logic io primary_name signal;
        ) internal_signals;
        io ("\n");

        (* connect aliases *)
        io (t4 ^ R.comment "aliases" ^ "\n");
        List.iter 
            (fun s -> 
                List.iter 
                    (fun t -> R.assign io t (primary_name s)) 
                    (secondary_names s))
            internal_signals;
        io ("\n");

        (* connect outputs *)
        io (t4 ^ R.comment "output assignments" ^ "\n");
        List.iter (R.logic io primary_name) outputs;
        io ("\n");

        R.end_logic io

end

module Vhdl = Make(VhdlCore)
module Verilog = Make(VerilogCore)

module C = struct

  open Circuit
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

  let write os circuit = 
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

  void "^name_^"destroy(simulator *sim) {
      if (sim) {
        if (sim->data) free(sim->data);
        if (sim->regs) free(sim->regs);
        if (sim->mems) free(sim->mems);
        if (sim->muxs) free(sim->muxs);
        if (sim->in_ports) free(sim->in_ports);
        if (sim->out_ports) free(sim->out_ports);
        free(sim);
      }
  }

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
   

end

module Hierarchy = 
struct

    open Signal.Types

    let inst_name = function
        | Signal_inst(_,_,i) -> i.inst_name
        | _ -> failwith "expecting instantiation"

    let rec write' ~transforms (seen : string list ref) database path write_hdl circ = 

      let name = Circuit.name circ in
      let fname = path ^ name ^ ".v" in
      (* write this module *)
      let circ = 
        List.fold_left (fun circ fn ->
            Transform.rewrite_circuit fn circ) circ transforms
      in
      write_hdl fname circ;
      (* find instantiations *)
      let insts = Circuit.search Circuit.id 
          (fun l s -> if is_inst s then s::l else l)
          [] (Circuit.outputs circ)
      in
      List.iter (fun inst -> (* XXX only write instantiations once for each unique name *)
          let name = inst_name inst in
          try
            match Circuit.Hierarchy.get database name with
            | Some(c) when not (List.mem name !seen) -> begin
                seen := name :: !seen;
              write' ~transforms:transforms seen database path write_hdl c
            end
            | _ -> ()
          with 
          | Circuit.Failure(e) -> failwith ("error generating " ^ name ^ ": " ^ e)
        ) insts

    let write ?(transforms=[]) database path write_hdl circ = 
      let seen = ref [] in
      write' ~transforms seen database path write_hdl circ

end

