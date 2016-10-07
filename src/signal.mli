(* 
 * hardcaml - hardware design in OCaml
 *
 *   (c) 2014 MicroJamJar Ltd
 *
 * Author(s): andy.ray@ujamjar.com
 * Description: 
 *
 *)

(** Hardware design datatype suitable for simulation and netlist generation *)

exception Failure of string

(** Signal data type and low-level functions *)
module Types : 
sig

    (** simple operators *)
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
    module UidMap : (Map.S with type key = uid)
    module UidSet : (Set.S with type elt = uid)

    (** internal structure for tracking signals *)
    type signal_id = 
        {
            s_id : uid;
            mutable s_names : string list;
            s_width : int;
            (** making this mutable turns hardcaml from pretty functional
              * to pretty imperative.  however, if used carefully and only
              * with the library, we can provide a potentially easier way
              * of changing the graph structure in some cases *)
            mutable s_deps : signal list
        }

    (** main signal data type *)
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

    (** returns the unique id of the signal *)
    val uid : signal -> uid
    
    val depo : signal -> 
        < sel : signal;
          op1 : signal;
          op2 : signal * signal;
          data : signal list;
        >

    (** returns the signals dependancies *)
    val deps : signal -> signal list
    
    (** returns the list of names assigned to the signal *)
    val names : signal -> string list
    
    (** width of the signal *)
    val width : signal -> int

    (** is the signal a register? *)
    val is_reg : signal -> bool
    
    (** is the signal a memory? *)
    val is_mem : signal -> bool

    (** is the signal an instantiation? *)
    val is_inst : signal -> bool

    (** is the signal a constant? *)
    val is_const : signal -> bool
    
    (** is the signal a part selection? *)
    val is_select : signal -> bool
    
    (** is the signal a wire? *)
    val is_wire : signal -> bool
    
    (** is the signal the given operator? *)
    val is_op : signal_op -> signal -> bool

    (** return the (binary) string representing a constants value *)
    val const_value : signal -> string

    (** creates a new signal uid *)
    val new_id : unit -> uid
    
    (** resets the signal identifiers *)
    val reset_id : unit -> unit
    
    (** constructs a signal_id type *)
    val make_id : int -> signal list -> signal_id

    (** return string representation of operator *)
    val string_of_op : signal_op -> string

    val to_string : signal -> string

    (** perform a recursive structural comparison of two signals *)
    val structural_compare : ?check_names:bool -> ?check_deps:bool -> 
        signal -> signal -> bool

end

open Comb

(** internal signal implementation *)
module Base : (T with type t = Types.signal)

(** Combinatorial signal API *)
module Comb : (S with type t = Types.signal)

module Const_prop : sig
  module Comb : (S with type t = Types.signal)
end

(** Sequential logic (register + memories) *)
module Seq :
sig

    (** type of registers with asychronous reset *)
    val r_async : Types.register

    (** type of registers with synchronous reset *)
    val r_sync : Types.register

    (** type of registers with no reset *)
    val r_none : Types.register

    (** type of registers with sync and async reset *)
    val r_full : Types.register

    (** create a register *)
    val reg : Types.register -> Types.signal -> Types.signal -> Types.signal

    (** create a register with feedback of output to input via user function *)
    val reg_fb : Types.register -> Types.signal -> int -> (Types.signal -> Types.signal) -> Types.signal

    (** creates a pipeline of registers *)
    val pipeline : int -> Types.register -> Types.signal -> Types.signal -> Types.signal

    (** creates a memory of the given size *)
    val memory : size:int -> spec:Types.register -> 
        we:Types.signal -> w:Types.signal -> d:Types.signal -> 
        r:Types.signal -> Types.signal

    (** Synthesizable, dual address, single clock ram with read before write behaviour *)
    val ram_rbw : size:int -> spec:Types.register -> 
        we:Types.signal -> wa:Types.signal -> d:Types.signal -> 
        re:Types.signal -> ra:Types.signal -> 
        Types.signal 

    (** Synthesizable, dual address, single clock ram with write before read behaviour *)
    val ram_wbr : size:int -> spec:Types.register -> 
        we:Types.signal -> wa:Types.signal -> d:Types.signal -> 
        re:Types.signal -> ra:Types.signal -> 
        Types.signal 

    (** simple dual port RAM's with 1:N or N:1 bus resizing *)
    val ram_1xn : 
        ?ram:(size:int -> spec:Types.register -> 
              we:Types.signal -> wa:Types.signal -> d:Types.signal -> 
              re:Types.signal -> ra:Types.signal -> Types.signal) -> 
        ?ram_type:Types.register -> ?reg_type:Types.register -> 
        we:Types.signal -> w:Types.signal -> d:Types.signal -> 
        re:Types.signal -> r:Types.signal -> 
        Types.signal 

end

module Instantiation :
sig
    open Types

    type instobj = < i : string -> signal; o : string -> signal >

    val (==>) : 'a -> 'b -> 'a * 'b

    val inst : ?lib:string -> ?arch:string ->
               string -> (string * parameter) list -> 
               (string * signal) list -> (string * int) list ->
               instobj

end

(** Guarded assignments.
 
    Allows code to be written in a similar manner to verilog always or vhdl
    process blocks.  'if' and 'switch' control constructs are provided. ($==)
    is used for assignment.

    Code is written as lists of assignments, if and control statements.

    variables;

{[
    let var = g_wire (zero 8) in
    let var = g_reg r_sync enable 8 in
]}

    assignment;

{[
    var $== exp;
]}

    if statements;

{[
    g_if condition [ ... ] [ ... ]
]}

    switch statements;

{[
    g_switch condition [
    consti 3 0 [ ... ]
    consti 3 1 [ ... ]
    consti 3 2 [ ... ]
    consti 3 3 [ ... ]
    ]
]}

    signals;

{[
    let (s:signal) = q (v:guarded) in
]}

    compilation;

{[
    compile [ ... ]
]}

    example;

{[
    let state = g_reg r_sync enable 2 in
    let a = g_wire 8 in
    compile [
        g_if (q a) ==: consti 8 4 [
            a $== consti 8 2
        ]
        g_switch (q state) [
            (consti 2 0) [
                a $== 3;
                state $== const 2 1;
            ]
            (consti 2 1) [
                a $== 2;
                state $== const 2 2;
            ]
            (consti 2 2) [
                a $== 1;
                state $== const 2 3;
            ]
            (consti 2 3) [
                a $== 0;
                state $== const 2 4;
            ]
        ]
    ]
    let state = q state in
    let a = q a in
    ....
]}
*)
module Guarded : 
sig

    (** the type of statements in guarded assigments *)
    type statement
    type statements = statement list
 
    (** the type of variables in guarded assignments *)
    type guarded_var_i

    class variable : guarded_var_i ->
        object
            method g : guarded_var_i
            method q : Types.signal
        end

    type 'a case = 'a * statements
    type 'a cases = 'a case list

    (** create a wire *)
    val g_wire : Types.signal -> variable
    
    (** create a register *)
    val g_reg : Types.register -> Types.signal -> int -> variable

    (** create a register with variable semantics *)
    val g_var : Types.register -> Types.signal -> int -> variable

    (** create a pipeline of registers *)
    val g_pipeline : int -> Types.register -> Types.signal -> int -> variable

    (** if statement *)
    val g_if : Types.signal -> statements -> statements -> statement

    (** else if branch *)
    val g_elif : Types.signal -> statements -> statements -> statements 

    (** if sel then [...] else [] *)
    val g_when : Types.signal -> statements -> statement

    (** if sel then [] else [...] *)
    val g_unless : Types.signal -> statements -> statement

    (** switch statement *)
    val g_switch : Types.signal -> Types.signal cases -> statement

    (** allows sequences of expressions to be inserted into the code.  a
        syntactic nicety. *)
    val g_proc : statements -> statement

    (** assignment *)
    val ($==) : variable -> Types.signal -> statement

    (** assignment with an integer constant - width is inferred *)
    val ($==.) : variable -> int -> statement

    (** compile statements to structural code *)
    val compile : statements -> unit

    (** statemachine abstraction *)
    val statemachine : Types.register -> Types.signal -> 'a list ->
        (variable * ('a cases -> statement) * ('a -> statement))

end

(** N read port by M write port memories *)
module Multiram : sig
  open Types

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

  type ram = size:int -> we:signal -> wa:signal -> d:signal -> re:signal -> ra:signal -> signal

  val ram : ?priority_write:bool -> ram:ram -> size:int -> spec:register -> 
      wr:signal write array -> rd:signal read array -> signal array

end

(** (revised) statemachine generator *)
module Statemachine : sig

  val statemachine : ?encoding:[ `binary | `onehot | `gray ] ->
    Types.register ->
    Types.signal ->
    'a list ->
    ('a -> Types.signal) *                      (* is_state *)
    ('a Guarded.cases -> Guarded.statement) *   (* switch *)
    ('a -> Guarded.statement)                   (* next *)

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

(** Generate register logic parameterised over reset/clear/enable types and defaults *)
module Make_seq(S : Seq_spec) : Seq

