(* 
 * hardcaml - hardware design in OCaml
 *
 *   (c) 2014 MicroJamJar Ltd
 *
 * Author(s): andy.ray@ujamjar.com
 * Description: 
 *
 *)

(** Simplified HardCaml API that includes tri-states - used for toplevel module generation *)

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
    
    (* instantiations *)
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

(* start circuit *)
val circuit : string -> unit
(* complete circuit, add to database *)
val end_circuit : unit -> unit
(* find circuit in database *)
val find_circuit : string -> circuit

val width : signal -> int

val mk_input : string -> int -> signal
val mk_output : string -> int -> signal
val mk_tristate : string -> int -> signal

val mk_wire : int -> signal
val mk_triwire : int -> signal

val (<==) : signal -> signal -> unit

val is_connected : signal -> bool

val inst : 
    ?g:(string * generic) list -> 
    ?i:(string * signal) list -> 
    ?o:(string * signal) list -> 
    ?t:(string * signal) list -> string -> unit

val (==>) : 'a -> 'b -> 'a * 'b

val const : string -> signal
val constz : int -> signal
val mux : signal -> signal list -> signal
val concat : signal list -> signal
val select : signal -> int -> int -> signal

module type Config = sig
    val structural_const : bool
    val structural_mux : bool
    val structural_concat : bool
    val structural_select : bool
end

val prefix : string

(* the comb API must be (rebuilt) between each circuit *)
module Base(C : Config) : Comb.T with type t = signal

(* progressively more structural APIs *)
module Base0 : Comb.T with type t = signal
module Base1 : Comb.T with type t = signal (* + mux,concat,select *)
module Base2 : Comb.T with type t = signal (* + consts *)

val write_verilog : (string -> unit) -> circuit -> unit

