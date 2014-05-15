(* 
 * hardcaml - hardware design in OCaml
 *
 *   (c) 2014 MicroJamJar Ltd
 *
 * Author(s): andy.ray@ujamjar.com
 * Description: 
 *
 *)

module RawBuilder(B : Bits.ArraybitsBase) : sig

    type t = 
        {
            data : B.barray;
            width : int;
        }

    val empty : t
    val width : t -> int

    val to_string : t -> string
    val to_int : t -> int
    val to_bstr : t -> string

    val copy : t -> t -> unit

    val const : string -> t
    val vdd : t
    val gnd : t

    val wire : int -> t
    val (--) : t -> string -> t 

    val (&:) : t -> t -> t -> unit
    val (|:) : t -> t -> t -> unit
    val (^:) : t -> t -> t -> unit

    val (~:) : t -> t -> unit

    val (+:) : t -> t -> t -> unit
    val (-:) : t -> t -> t -> unit

    val (==:) : t -> t -> t -> unit
    val (<:) : t -> t -> t -> unit

    val mux : t -> t -> t list -> unit

    val concat : t -> t list -> unit
    val select : t -> t -> int -> int -> unit
    
end

