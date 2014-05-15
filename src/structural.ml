(* 
 * hardcaml - hardware design in OCaml
 *
 *   (c) 2014 MicroJamJar Ltd
 *
 * Author(s): andy.ray@ujamjar.com
 * Description: 
 *
 *)

(*
 
we have the following object types;

- MI module input {R}
- MO module output {W}
- MT module tristate {RW}

- SI submodule input [<-MI,MT,W,C] 
- SO submodule output [->MO,MT,W]
- ST submodule tristate [<-MI,MT,W,C ->MO,MT,W]

- W wires {RW} [<-MI,MT,W,C -> MO,MT,W]
- C constants {R} [->C,MO,MT]

Basic rules;

- module IO's may connect to submodules
- submodulues connect via wires or module IOs
- constants are currently included but are not needed - might be better to
  detect them at generation time instead - similar for other simple RTL
  operators.

Multiple {RW}

- many things may read an object
- in general only 1 thing may drive (write) and object
- special case - multiple tristates may drive an object

NOTE: It would be nice if many of the rules below could be encoded into the
      type system, but I dont know how or if it's possible.

*)

module StringMap = Map.Make(String)
module StringSet = Set.Make(String)

type name = string
type id = int
type width = int

type signal = 
    | Empty
    
    (* module interface *)
    | Module_input of id * name * width
    | Module_output of id * name * width * signal ref
    | Module_tristate of id * name * width * signal list ref  
    
    (* internal wires *)
    | Internal_wire of id * width * signal ref 
    | Internal_triwire of id * width * signal list ref
    
    (* instnatiations *)
    | Instantiation_output of id * name (* reference to instantiation *)
    | Instantiation_tristate of id * name 
    | Instantiation of id * name * 
        (string * generic) list *
        (string * signal) list * (* inputs (read) *)
        (string * signal) list * (* outputs (write; drive wires/module outputs *)
        (string * signal) list   (* tristate (write; drive triwires/module tristates *)

    (* basic RTL operators *)
    | Rtl_op of id * width * rtl_op

and rtl_op = 
    | Constant of string 
    | Select of int * int * signal
    | Concat of signal list
    | Mux of signal * signal list

and generic = 
    | GInt of int
    | GFloat of float
    | GString of string
    | GUnquoted of string

type circuit = 
    {
        name : string;
        id : id;
        mutable signals : signal list;
    }

exception Invalid_submodule_input_connection of string * string * signal
exception Invalid_submodule_output_connection of string * string * signal
exception Invalid_submodule_tristate_connection of string * string * signal
exception Wire_already_assigned of signal
exception Invalid_assignment_target of signal
exception Cant_assign_wire_with of signal
exception Cant_assign_triwire_with of signal

exception Invalid_name of signal
exception Invalid_width of signal
exception Invalid_id of signal

exception Invalid_constant of string
exception Rtl_op_arg_not_readable of signal
exception Too_few_mux_data_elements
exception Too_many_mux_data_elements of int
exception All_mux_data_elements_must_be_same_width of int list
exception No_elements_to_concat
exception Select_index_error of int * int
exception Binop_arg_widths_different of string

exception No_circuit
exception Circuit_already_started
exception Circuit_already_exists of string
exception IO_name_already_exists of string

(* state *)
let id' = ref 0 
let const_map = ref StringMap.empty 
let circuit' = ref None
let circuits' = ref []

let id () = 
    let id = !id' in 
    incr id'; 
    id

let name t = 
    match t with
    | Module_input(_,name,_) 
    | Module_output(_,name,_,_) 
    | Module_tristate(_,name,_,_) 
    | Instantiation(_,name,_,_,_,_) -> name
    | Internal_triwire(id,_,_) 
    | Internal_wire(id,_,_) 
    | Rtl_op(id,_,_) -> "_" ^ string_of_int id
    | Empty 
    | Instantiation_output _ 
    | Instantiation_tristate _ -> raise (Invalid_name t)

let width t = 
    match t with
    | Module_input(_,_,width) 
    | Module_output(_,_,width,_) 
    | Module_tristate(_,_,width,_) 
    | Internal_triwire(_,width,_) 
    | Internal_wire(_,width,_) 
    | Rtl_op(_,width,_) -> width
    | Empty 
    | Instantiation _
    | Instantiation_output _ 
    | Instantiation_tristate _ -> raise (Invalid_width t)

let get_id t = 
    match t with
    | Empty 
    | Instantiation_output _
    | Instantiation_tristate _ -> raise (Invalid_id t)
    | Module_input(i,_,_) 
    | Module_output(i,_,_,_) 
    | Module_tristate(i,_,_,_) 
    | Internal_triwire(i,_,_) 
    | Internal_wire(i,_,_) 
    | Rtl_op(i,_,_) 
    | Instantiation(i,_,_,_,_,_) -> i

let empty name = 
    {
        name = name;
        id = id();
        signals = [];
    }

let circuit_exists name = 
    try ignore (List.assoc name !circuits'); true
    with _ -> false

let circuit name = 
    match !circuit' with
    | Some(x) -> raise Circuit_already_started
    | None -> 
        if circuit_exists name then raise (Circuit_already_exists name)
        else begin
            id' := 0;
            const_map := StringMap.empty;
            circuit' := Some (empty name)
        end

let check_unique_io_names c = 
    let ios,_ = List.partition 
        (function Module_input _  
                | Module_output _ 
                | Module_tristate _ -> true 
                | _ -> false) c.signals
    in
    let _ = List.fold_left (fun set io ->
        let name = name io in
        if StringSet.mem name set then raise (IO_name_already_exists name)
        else StringSet.add name set) StringSet.empty ios
    in
    ()

let end_circuit () = 
    match !circuit' with
    | None -> raise No_circuit
    | Some(x) -> begin
        check_unique_io_names x;
        circuits' := (x.name,x) :: !circuits';
        circuit' := None
    end

let get_circuit () = 
    match !circuit' with
    | None -> raise No_circuit
    | Some(x) -> x

let find_circuit name = List.assoc name !circuits'

let add_sig s = 
    let c = get_circuit() in
    c.signals <- s :: c.signals;
    s

let (>>) a b = b a

let mk_input name width = 
    Module_input(id(), name, width) >> add_sig
let mk_output name width = 
    Module_output(id(), name, width, ref Empty) >> add_sig
let mk_tristate name width = 
    Module_tristate(id(), name, width, ref []) >> add_sig
let mk_wire width = 
    Internal_wire(id(), width, ref Empty) >> add_sig
let mk_triwire width = 
    Internal_triwire(id(), width, ref []) >> add_sig

let is_z s = 
    try (* check constant is all 'z' and this assignable to a tristate *)
        let is_z = function 'z' -> () | _ -> raise Not_found in
        let () = String.iter is_z s in
        true
    with Not_found ->
        false

let (<==) a b = 
    match a with
    | Module_output(_,_,_,con) 
    | Internal_wire(_,_,con) ->
        (match !con with
        | Empty -> 
            (match b with
            | Internal_wire _ | Module_input _ 
            | Instantiation_output _ | Rtl_op _ ->
                con := b
            | _ -> raise (Cant_assign_wire_with(b)))
        | _ -> raise (Wire_already_assigned(a)))
    | Module_tristate(_,_,_,con) 
    | Internal_triwire(_,_,con) ->
        (match b with
        | Module_tristate _ | Internal_triwire _ 
        | Instantiation_tristate _ -> 
            con := b :: !con
        | Rtl_op(_,_,Constant _) -> (* need to be able to assign any type of constant *)
            con := b :: !con
        | _ -> raise (Cant_assign_triwire_with(b)))
    | _ -> raise (Invalid_assignment_target(a))

let is_readable = 
    function Module_input _ | Internal_wire _ | Rtl_op _ -> true | _ -> false
let is_writeable = 
    function Module_output _ | Internal_wire _ -> true | _ -> false
let is_readwrite = 
    function Module_tristate _ | Internal_triwire _ -> true | _ -> false

let is_connected = 
    function Module_output(_,_,_,con)
           | Internal_wire(_,_,con) -> !con <> Empty
           | Module_tristate(_,_,_,cons)
           | Internal_triwire(_,_,cons) -> !cons <> []
           | _ -> true

let inst ?(g=[]) ?(i=[]) ?(o=[]) ?(t=[]) name =
    let mod_id = id () in
    (* inputs: module inputs, wires *)
    List.iter (fun (n,s) -> if not (is_readable s) then 
        raise (Invalid_submodule_input_connection(name,n,s))) i;
    List.iter 
        (fun (n,s) -> 
            if not (is_writeable s) then 
                raise (Invalid_submodule_output_connection(name,n,s))
            else
                s <== Instantiation_output(mod_id, n)
        ) o;
    List.iter 
        (fun (n,s) -> 
            if not (is_readwrite s) then 
                raise (Invalid_submodule_tristate_connection(name,n,s))
            else
                s <== Instantiation_tristate(mod_id,n)
        ) t;
    ignore (Instantiation(mod_id, name, g, i, o, t) >> add_sig)

let (==>) a b = (a,b)

let const' b = 
    let _ = String.iter 
        (function '0' | '1' | 'z' -> () | _ -> raise (Invalid_constant b)) b
    in
    let _ = if b = "" then raise (Invalid_constant b) in
    Rtl_op(id(), String.length b, Constant b) >> add_sig

let const b = 
    try StringMap.find b !const_map 
    with Not_found -> begin
        let c = const' b in
        const_map := StringMap.add b c !const_map;
        c
    end

let constz w = const (String.make w 'z')

let check_readable l = 
    let ok s = if not (is_readable s) then raise (Rtl_op_arg_not_readable s) in
    List.iter ok l

let mux sel d = 
    let _ = check_readable (sel::d) in
    let len = List.length d in
    let _ = if len < 1 then raise Too_few_mux_data_elements in
    let _ = if len > (1 lsl (width sel)) then 
        raise (Too_many_mux_data_elements len) in
    let w = width (List.hd d) in 
    let _ = 
        if not (List.fold_left (fun a s -> a && width s = w) true d) then
            raise (All_mux_data_elements_must_be_same_width (List.map width d))
    in
    Rtl_op(id(), w, Mux(sel, d)) >> add_sig

let concat d = 
    let _ = check_readable d in
    let _ = if List.length d = 0 then raise No_elements_to_concat in
    Rtl_op(id(), List.fold_left (fun a s -> a + width s) 0 d, Concat d) >> add_sig

let select d hi lo = 
    let _ = check_readable [d] in
    let _ = if hi < lo then raise (Select_index_error(hi,lo)) in
    let _ = if lo < 0 then raise (Select_index_error(hi,lo)) in
    let _ = if hi >= width d then raise (Select_index_error(hi,lo)) in
    Rtl_op(id(), hi-lo+1, Select(hi,lo,d)) >> add_sig

module type Config = sig
    val structural_const : bool
    val structural_mux : bool
    val structural_concat : bool
    val structural_select : bool
end

let prefix = "hardcaml_lib_"

(* Comb API *)
module Base(C : Config) = struct
    open C

    type t = signal

    let empty = Empty
    let width = width
    let wire = mk_wire
    let (<==) = (<==)
    let (--) a _ = a
    
    let lazy_const name = 
        lazy (
            let x = wire 1 in
            let _ = inst (prefix^name) ~o:[ "o" ==> x ] in
            x
        )

    let vdd = lazy_const "vdd"
    let gnd = lazy_const "gnd"
    let z = lazy_const "z"

    let list_of_string s = 
        let a = Array.init (String.length s) (String.get s) in
        Array.to_list a

    let binop0 name a b = 
        let _ = if width a <> width b then raise (Binop_arg_widths_different name) in
        let o = wire 1 in
        let _ = inst (prefix^name)
            ~g:[ "b" ==> GInt(width a) ]
            ~i:[ "i0" ==> a; "i1" ==> b ]
            ~o:[ "o" ==> o ]
        in
        o

    let binop1 name a b = 
        let _ = if width a <> width b then raise (Binop_arg_widths_different name) in
        let o = wire (width a) in
        let _ = inst (prefix^name)
            ~g:[ "b" ==> GInt(width a) ]
            ~i:[ "i0" ==> a; "i1" ==> b ]
            ~o:[ "o" ==> o ]
        in
        o
    
    let binop2 name a b = 
        let o = wire (width a + width b) in
        let _ = inst (prefix^name)
            ~g:[ "w0" ==> GInt(width a); "w1" ==> GInt(width b) ]
            ~i:[ "i0" ==> a; "i1" ==> b ]
            ~o:[ "o" ==> o ]
        in
        o
    
    let concat2 = binop2 "concat2"

    let s_concat d = 
        let rec cat2 = function
            | [] -> raise No_elements_to_concat
            | [a] -> a
            | h::t -> concat2 h (cat2 t)
        in 
        cat2 d

    let concat = 
        if structural_concat then s_concat
        else concat 

    let s_select d h l = 
        let o = wire (h-l+1) in
        let _ = inst (prefix^"select") 
            ~g:[ "b" ==> GInt(width d); "h" ==> GInt(h); "l" ==> GInt(l) ]
            ~i:[ "i" ==> d ]
            ~o:[ "o" ==> o ]
        in
        o

    let select = 
        if structural_select then s_select
        else select 

    let mux2 sel a b = 
        let o = wire (width a) in
        let _ = inst (prefix^"mux2")
            ~g:[ "b" ==> GInt(width a) ]
            ~i:[ "d0" ==> b; "d1" ==> a ]
            ~o:[ "o" ==> o ]
        in
        o

    let s_mux sel d = 
        let w = width sel in
        let rec f n l def = 
            if n=w then 
                match l with
                | [a] -> a
                | _ -> raise (Too_many_mux_data_elements (List.length d))
            else
                let s = select sel n n in
                let rec pairs = function 
                    | [] -> []
                    | [a] -> [a]
                    | a::b::t -> mux2 s b a :: pairs t
                in
                match l with
                | [a] -> mux2 s def a
                | _ -> f (n+1) (pairs l) def
        in
        let def = try List.hd (List.rev d) 
                  with _ -> raise Too_few_mux_data_elements in
        f 0 d def

    let mux = 
        if structural_mux then s_mux
        else mux

    let s_const b = 
        concat (List.map 
            (function '0' -> Lazy.force gnd
                    | '1' -> Lazy.force vdd 
                    | 'z' -> Lazy.force z 
                    | _ -> raise (Invalid_constant b)) 
            (list_of_string b))

    let const = 
        if structural_const then s_const
        else const 

    let (+:) = binop1 "add"
    let (-:) = binop1 "sub"

    let ( *: ) = binop2 "mulu"
    let ( *+ ) = binop2 "muls"

    let (&:) = binop1 "and"
    let (|:) = binop1 "or"
    let (^:) = binop1 "xor"

    let (~:) i = 
        let o = wire (width i) in
        let _ = inst (prefix^"not")
            ~g:[ "b" ==> GInt(width i) ]
            ~i:[ "i" ==> i ]
            ~o:[ "o" ==> o ]
        in
        o

    let (==:) = binop0 "eq"
    let (<:) = binop0 "lt"

    let to_string s = name s
    let to_int _ = failwith "Structural.Base.to_int"
    let to_bstr _ = failwith "Structural.Base.to_bstr"
end

module Base0 = Base(struct
    let structural_const = false
    let structural_mux = false
    let structural_concat = false
    let structural_select = false
end)

module Base1 = Base(struct
    let structural_const = false
    let structural_mux = true
    let structural_concat = true
    let structural_select = true
end)

module Base2 = Base(struct
    let structural_const = true
    let structural_mux = true
    let structural_concat = true
    let structural_select = true
end)
(*
let remove_unconnected circuit = 
    let module IdSet = Set.Make
        (struct 
            type t = int 
            let compare = compare
        end) 
    in

    let add set signal = IdSet.add (get_id signal) set in
    let add_list set signals = List.fold_left add set signals in

    let rec find (i,o,io,m_o,m_io,inst,rest) = function
        | [] -> (i,o,io,m_o,m_io,inst,rest)
        | s :: t -> begin
            match s with
            | Module_input _ -> add i s
            | Module_output _ -> add o s
            | Module_tristate _ -> add io s

            | Instantiation_output _ -> add m_o s
            | Instantiation_tristate _ -> add m_io s

            | Instantiation _ -> add inst s
            
            | _ -> s :: rest
        end
    in
    ()
*)


let write_verilog os circuit = 
    let open Printf in

    let declare typ signal = 
        "  " ^ typ ^ " " ^ 
        (if width signal = 1 then "" else sprintf "[%i:0] " (width signal-1)) ^
        (name signal)
    in
    let seperator sep l = List.fold_left (fun a s -> if a="" then s else a ^ sep ^ s) "" l in
    let assign s0 s1 = os ("  assign " ^ (name s0) ^ " = " ^ (name s1) ^ ";\n") in
    let part = List.partition in

    let is_input = function Module_input _ -> true | _ -> false in
    let is_output = function Module_output _ -> true | _ -> false in
    let is_inout = function Module_tristate _ -> true | _ -> false in
    let is_inst = function Instantiation _ -> true | _ -> false in

    let signals = List.rev circuit.signals in
    let inputs, signals = part is_input signals in
    let outputs, signals = part is_output signals in
    let inouts, signals = part is_inout signals in
    let inst, signals = part is_inst signals in

    (* module interface *)
    os ("module " ^ circuit.name ^ "\n");
    os ("(\n");
    os (seperator ",\n" 
        (List.concat [
            List.map (declare "input") inputs;
            List.map (declare "output") outputs;
            List.map (declare "inout") inouts;
        ]));
    os ("\n);\n\n");

    (* write wire declarations *)
    let declare_wire s = os (declare "wire" s ^ ";\n") in
    List.iter declare_wire signals;

    (* write assignments *)
    let connects = 
        function Empty | Instantiation_output _ | Instantiation_tristate _ -> false
        | _ -> true
    in
    let write_assignment s = 
        match s with
        | Internal_wire(id,width,con) -> 
            (if connects !con then assign s !con)
        | Internal_triwire(id,width,cons) ->
            List.iter (fun con -> if connects con then assign s con) !cons
        | Rtl_op(id,width,op) -> begin
            os ("  assign " ^ name s ^ " = ");
            (match op with
            | Constant(b) -> os (string_of_int width ^ "'b" ^ b)
            | Select(hi,lo,s) ->
                os (name s ^ "[" ^ string_of_int hi ^ ":" ^
                                   string_of_int lo ^ "]")
            | Concat(d) ->
                (os "{ "; os (seperator ", " (List.map name d)); os " }")
            | Mux(sel,d) ->
                let rec write n l = 
                    match l with
                    | [] -> ()
                    | [x] -> os ("    " ^ name x)
                    | x::t -> begin
                        os ("    " ^ name sel ^ " == " ^ string_of_int n ^ 
                            " ? " ^ name x ^ " :\n");
                        write (n+1) t
                    end
                in
                os "\n";
                write 0 d);
                os (";\n")
        end
        | _ -> failwith "write_assignment"
    in
    List.iter write_assignment signals;

    (* write module outputs and inouts *)
    let assign_output s = 
        match s with
        | Module_output(_,_,_,con) ->
            (if connects !con then assign s !con)
        | Module_tristate(_,_,_,cons) ->
            List.iter (fun con -> if connects con then assign s con) !cons 
        | _ -> failwith "assign_output"
    in
    List.iter assign_output outputs;
    List.iter assign_output inouts;

    (* write instantiations *)
    let write_inst = function
        | Instantiation(id,iname,g,i,o,t) -> begin
            os ("  " ^ iname ^ "");
            if g <> [] then begin
                os "\n  #(\n";
                os (seperator ",\n"
                    (List.map (fun (n,g) ->
                        "    ." ^ n ^ "(" ^ 
                            (match g with
                            | GInt(i) -> string_of_int i
                            | GFloat(f) -> string_of_float f
                            | GString(s) -> "\"" ^ s ^ "\""
                            | GUnquoted(s) -> s) ^ ")") g));
                os "\n  )"
            end;
            os (" _" ^ string_of_int id ^ "\n");
            os "  (\n";
            os (seperator ",\n" (List.concat [
                List.map (fun (n,s) -> "    ." ^ n ^ "(" ^ name s ^ ")") i;
                List.map (fun (n,s) -> "    ." ^ n ^ "(" ^ name s ^ ")") o;
                List.map (fun (n,s) -> "    ." ^ n ^ "(" ^ name s ^ ")") t;
            ]));
            os "\n  );\n";
        end
        | _ -> raise Not_found
    in
    List.iter write_inst inst;

    os ("endmodule\n")


