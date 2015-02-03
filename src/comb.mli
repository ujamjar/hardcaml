(* 
 * hardcaml - hardware design in OCaml
 *
 *   (c) 2014 MicroJamJar Ltd
 *
 * Author(s): andy.ray@ujamjar.com
 * Description: 
 *
 *)

(** Combinatorial API *)

(** Type required to generate the full combinatorial API *)
module type T = 
sig

    type t 
    
    (** the empty signal *)
    val empty : t
    
    (** returns the width of a signal *)
    val width : t -> int
    
    (** creates a constant *)
    val const : string -> t
    
    (** concatenates a list of signals *)
    val concat : t list -> t
    
    (** multiplexer *)
    val mux : t -> t list -> t
    
    (** select a range of bits *)
    val select : t -> int -> int -> t
    
    (** creates an unassigned wire *)
    val wire : int -> t
    
    (** names a signal *)
    val (--) : t -> string -> t
    
    (** bitwise and *)
    val (&:) : t -> t -> t
    
    (** bitwise or *)
    val (|:) : t -> t -> t
    
    (** bitwise xor *)
    val (^:) : t -> t -> t
    
    (** bitwise not *)
    val (~:) : t -> t
    
    (** addition *)
    val (+:) : t -> t -> t
    
    (** subtraction *)
    val (-:) : t -> t -> t
    
    (** signed multiplication *)
    val ( *: ) : t -> t -> t
    
    (** unsigned multiplication *)
    val ( *+ ) : t -> t -> t
    
    (** equality *)
    val (==:) : t -> t -> t
    
    (** less than *)
    val (<:) : t -> t -> t
    
    (** assigns to wire *)
    val (<==) : t -> t -> unit
    
    (** create string from signal *)
    val to_string : t -> string

    (** get value as int (if possible) *)
    val to_int : t -> int

    (** create binary string from signal (if possible) *)
    val to_bstr : t -> string

end

(** Full combinatorial API *)
module type S =
sig
   
    type t 
    
    (** the empty signal *)
    val empty : t
    
    (** names a signal
     
        [let a = a -- "a" in ...]
     
        signals may have multiple names. *)
    val ( -- ) : t -> string -> t
    
    (** returns the width (number of bits) of a signal.
       
        [let w = width s in ...] 
    *)
    val width : t -> int
    
    (** convert binary string to constant *)
    val constb : string -> t
    
    (** convert integer to constant *)
    val consti : int -> int -> t
    val consti32 : int -> int32 -> t
    val consti64 : int -> int64 -> t

    (** convert unsigned hex string to constant *)
    val consthu : int -> string -> t
    
    (** convert signed hex string to constant *)
    val consths : int -> string -> t
    
    (** convert decimal string to constant*)
    val constd : int -> string -> t
    
    (** convert verilog style string to constant *)
    val constv : string -> t

    (** convert IntbitsList to constant *)
    val constibl : int list -> t

    (** convert verilog style or binary string to constant *)
    val const : string -> t

    (** concatenates a list of signals - the msb of the head of the list will
        become the msb of the result. 
        
        [let c = concat \[a; b; c\] in ...] *)
    val concat : t list -> t
    
    (** same as [concat] except empty signals are first filtered out *)
    val concat_e : t list -> t

    (** concatenate two signals. 
     
        [let c = a @: b in ...]

        equivalent to [concat \[a;b\]] *)
    val ( @: ) : t -> t -> t
    
    (** logic 1 *)
    val vdd : t
    
    (** logic 0 *)
    val gnd : t
    
    (** [zero w] makes a the zero valued constant of width [w] *)
    val zero : int -> t
    
    (** [one w] makes a one valued constant of width [w] *)
    val ones : int -> t
    
    (** [ones w] makes a constant of all ones of width [w] *)
    val one : int -> t
    
    (** [select s hi lo] selects bits in the range [hi]...[lo] inclusive from s.
        The selection indices must fall within the range of [s] and [hi] >= [lo]. *)
    val select : t -> int -> int -> t
    
    (** same as [select] except invalid indices return [empty] *)
    val select_e : t -> int -> int -> t

    (** select a single bit *)
    val bit : t -> int -> t
    
    (** get most significant bit *)
    val msb : t -> t
    
    (** get least significant bits *)
    val lsbs : t -> t
    
    (** get least significant bit *)
    val lsb : t -> t
    
    (** get most significant bits *)
    val msbs : t -> t
    
    (** [drop_bottom s n] drop bottom [n] bits of [s] *)
    val drop_bottom : t -> int -> t
    
    (** [drop_top s n] drop top [n] bits of [s] *)
    val drop_top : t -> int -> t
    
    (** [sel_bottom s n] select bottom [n] bits of [s] *)
    val sel_bottom : t -> int -> t
    
    (** [sel_top s n] select top [n] bits of [s] *)
    val sel_top : t -> int -> t
    
    (** [insert ~t ~f n] insert [f] into [t] as postion [n] *)
    val insert : t:t -> f:t -> int -> t
    
    (** *)
    val sel : t -> (int * int) -> t

    (** multiplexer. 
     
        [let m = mux sel inputs in ...]
      
        Given [l] = [List.length inputs] and [w] = [width sel] the following conditions must hold.

        [l] <= 2**[w], [l] >= 2

        If [l] < 2**[w], the last input is repeated.

        All inputs provided must have the same width, which will
        in turn be equal to the width of [m]. *)
    val mux : t -> t list -> t
    
    (** [mux2 c t f] 2 input multiplexer.  Selects [t] if [c] is high otherwise [f].
        
        [t] and [f] must have same width and [c] must be 1 bit.

        Equivalent to [mux c \[f; t\]] *)
    val mux2 : t -> t -> t -> t
    
    val mux_init : t -> int -> (int -> t) -> t

    (** case mux *)
    val cases : t -> t -> (int * t) list -> t

    (** priority mux (with default) *)
    val pmux : (t * t) list -> t -> t

    (** log depth priority mux (no default) *)
    val pmuxl : (t * t) list -> t

    (** onehot priority mux (default=0) *)
    val pmux1h : (t * t) list -> t

    (** logical and *)
    val (&:) : t -> t -> t
    val (&:.) : t -> int -> t

    (** a <>:. 0 &: b <>:. 0 *)
    val (&&:) : t -> t -> t

    (** logical or *)
    val (|:) : t -> t -> t
    val (|:.) : t -> int -> t
    
    (** a <>:. 0 |: b <>:. 0 *)
    val (||:) : t -> t -> t

    (** logic xor *)
    val (^:) : t -> t -> t
    val (^:.) : t -> int -> t
    
    (** logical not *)
    val ( ~: ) : t -> t
    
    (** addition *)
    val ( +: ) : t -> t -> t
    val ( +:. ) : t -> int -> t

    (** subtraction *)
    val ( -: ) : t -> t -> t
    val ( -:. ) : t -> int -> t
    
    (** negation *)
    val negate : t -> t
    
    (** unsigned multiplication *)
    val ( *: ) : t -> t -> t
    
    (** signed multiplication *)
    val ( *+ ) : t -> t -> t
    
    (** equality *)
    val ( ==: ) : t -> t -> t
    val (==:.) : t -> int -> t
    
    (** inequality *)
    val ( <>: ) : t -> t -> t
    val (<>:.) : t -> int -> t
    
    (** less than *)
    val ( <: ) : t -> t -> t
    val (<:.) : t -> int -> t
    (* added due to clash with camlp5 *)
    val lt : t -> t -> t
    
    (** greater than *)
    val ( >: ) : t -> t -> t
    val (>:.) : t -> int -> t
    
    (** less than or equal to *)
    val ( <=: ) : t -> t -> t
    val (<=:.) : t -> int -> t
    
    (** greater than or equal to *)
    val ( >=: ) : t -> t -> t
    val (>=:.) : t -> int -> t
    
    (** signed less than *)
    val ( <+ ) : t -> t -> t
    val (<+.) : t -> int -> t
    
    (** signed greater than *)
    val ( >+ ) : t -> t -> t
    val (>+.) : t -> int -> t
    
    (** signed less than or equal to *)
    val ( <=+ ) : t -> t -> t
    val (<=+.) : t -> int -> t
    
    (** signed greated than or equal to *)
    val ( >=+ ) : t -> t -> t
    val (>=+.) : t -> int -> t

    (** create string from signal *)
    val to_string : t -> string
    
    (** get value as unsigned int (if possible) *)
    val to_int : t -> int

    (** get value as signed int (if possible) *)
    val to_sint : t -> int

    val to_int32 : t -> int32
    val to_sint32 : t -> int32
    val to_int64 : t -> int64
    val to_sint64 : t -> int64

    (** create binary string from signal *)
    val to_bstr : t -> string

    (** convert signal to a list of bits, msb first *)
    val bits : t -> t list
    
    (** [to_array s] convert signal [s] to array of bits with lsb at index 0 *)
    val to_array : t -> t array

    (** [of_array a] convert array [a] of bits to signal with lsb at index 0 *)
    val of_array : t array -> t

    (** creates an unassigned wire *)
    val wire : int -> t

    (** creates an assigned wire *)
    val wireof : t -> t
    
    (** assigns to wire *)
    val ( <== ) : t -> t -> unit
    val assign : t -> t -> unit
    
    (** creates an input *)
    val input : string -> int -> t
    
    (** creates an output *)
    val output : string -> t -> t
    
    (** global clock *)
    val clock : t
    
    (** global asynchronous reset *)
    val reset : t
    
    (** global synchronous clear *)
    val clear : t
    
    (** global enable *)
    val enable : t

    (** repeat signal n times *)
    val repeat : t -> int -> t

    (** split signal in half *)
    val split : t -> t * t

    (** shift left logical *)
    val sll : t -> int -> t

    (** shift right logical *)
    val srl : t -> int -> t

    (** shift right arithmetic *)
    val sra : t -> int -> t

    (** shift by variable amount *)
    val log_shift : (t -> int -> t) -> t -> t -> t

    (** unsigned resize *)
    val uresize : t -> int -> t

    (** signed resize *)
    val sresize : t -> int -> t

    (** unsigned resize by +1 bit *)
    val ue : t -> t

    (** signed resize by +1 bit *)
    val se : t -> t

    (** fold 'op' though list *)
    val reduce : ('a -> 'a -> 'a) -> 'a list -> 'a

    (** reverse bits *)
    val reverse : t -> t

    (** Counts from 0 to (max-1) then from zero again.  
        If max == 1<<n, then a comparator is not generated and overflow arithmetic used instead *)
    val mod_counter : int -> t -> t

    (** creates a tree of operations.  The arity of the operator is configurable *)
    val tree : int -> ('a list -> 'a) -> 'a list -> 'a

    (** convert binary to onehot *)
    val binary_to_onehot : t -> t

    (** convert onehot to binary *)
    val onehot_to_binary : t -> t

    (** convert binary to gray code *)
    val binary_to_gray : t -> t

    (** convert gray code to binary *)
    val gray_to_binary : t -> t

    (** create random constant vector of given size *)
    val srand : int -> t

    module type TypedMath = sig
        type v
        val of_signal : t -> v
        val to_signal : v -> t
        val (+:) : v -> v -> v
        val (-:) : v -> v -> v
        val ( *: ) : v -> v -> v
        val (<:) : v -> v -> v
        val (>:) : v -> v -> v
        val (<=:) : v -> v -> v
        val (>=:) : v -> v -> v
        val (==:) : v -> v -> v
        val (<>:) : v -> v -> v
        val resize : v -> int -> v
    end

    (* general arithmetic on unsigned signals.  operands and results are resized
     * to fit as appropriate *)
    module Unsigned : TypedMath 

    (* general arithmetic on signed signals.  operands and results are resized
     * to fit as appropriate *)
    module Signed : TypedMath

    (** Unsigned operations compatible with type t *)
    module Uop : TypedMath with type v := t

    (** Signed operations compatible with type t *)
    module Sop : TypedMath with type v := t

end

(** Generates the full combinatorial API *)
module Make : functor (Bits : T) -> (S with type t = Bits.t) 

