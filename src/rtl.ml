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

let tab n = String.make n ' '
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
    let x = String.create l in
    for i=0 to l-1 do
        x.[i] <- f s.[i]
    done;
    x

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
        compare (String.uppercase a) (String.uppercase b)
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

module Hierarchy = 
struct

    open Signal.Types

    let inst_name = function
        | Signal_inst(_,_,i) -> i.inst_name
        | _ -> failwith "expecting instantiation"

    let rec write ?(transforms=[]) database path write_hdl circ = 
        
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
        List.iter (fun inst ->
            let name = inst_name inst in
            try
                match Circuit.Hierarchy.get database name with
                    | Some(c) -> write ~transforms:transforms database path write_hdl c
                    | None -> ()
            with 
            | Circuit.Failure(e) -> failwith ("error generating " ^ name ^ ": " ^ e)
        ) insts

end

(*
(* TEST *)

let circ = 
    let a = input "a" 8 in
    let b = input "b" 8 in
    let c = a +: b -- "hc_uns" -- "cc" in
    let c = reg r_full enable c -- "creg" in
    let c = reg r_none enable c -- "cregn" in
    let d = reduce (|:) (bits c) in
    let d = (d ^: enable &: clear) ==: gnd in
    let d = (mux (bit enable 0 @: ~: (clear +: enable)) [c; a; b]) @: d in
    let d = d *: enable in
    let d = d *+ enable in
    let open Signal.Instantiation in
    let x = inst "testmod"
        [ "blah" ==> ParamInt(1) ] 
        [ "x" ==> d; "y" ==> c ] 
        [ "z" ==> 19; "w" ==> 21; "o" ==> 1 ]
    in
    let d = x#o "o" in
    
    let we = input "we" 1 in
    let e = memory ~size:256 ~spec:r_sync ~we:we ~w:a ~d:b ~r:(a+:b) in
    let e = e -- "memarr" in
    let e = output "temp" e in

    let c = output "c" c in
    let d = output "d" d in
    let e = output "e" e in
    Circuit.make "temp" [c;d;e] 

let test write = 
    write (output_string stdout) circ

let _ = test Vhdl.write

*)
