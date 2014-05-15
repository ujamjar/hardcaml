(* 
 * hardcaml - hardware design in OCaml
 *
 *   (c) 2014 MicroJamJar Ltd
 *
 * Author(s): andy.ray@ujamjar.com
 * Description: 
 *
 *)

(** Floating point data type (todo) *)

(** floating point type configuration *)
module type FloatSpec = sig
    val exponent_bits : int
    val mantissa_bits : int
end

(** specification of 32 bit floating point numbers *)
module Float32 : FloatSpec

(** specification of 64 bit floating point numbers *)
module Float64 : FloatSpec

module type Float = sig
    (* base signal type *)
    type bt
    (* floating point number. *)
    type t
    val negate : t -> t
    val (+:) : t -> t -> t
    val (-:) : t -> t -> t
    val ( *: ) : t -> t -> t
    val (==:) : t -> t -> bt
    val (<>:) : t -> t -> bt
    val (<:) : t -> t -> bt
    val (>:) : t -> t -> bt
    val (<=:) : t -> t -> bt
    val (>=:) : t -> t -> bt
end

module type FloatBase = sig

    include Float

    val sign : t -> bt
    val exponent : t -> bt
    val mantissa : t -> bt

    type ordered_float = 
        {
            a_lt_b : bt;
            exp_diff : bt;
            min : t;
            max : t;
            renorm : bt;
            state : string;
        }

    val fmin : ordered_float -> t
    val fmax : ordered_float -> t

    module Const : sig
        val max_exp : int
        val bias : int
        val max_adj_exp : int
        val min_adj_exp : int
        val bits : int
        val zero : t
        val min_denormal : t
        val max_denormal : t
    end

    val to_float : t -> float
    val of_float : float -> t
    val of_signal : bt -> t
    val to_signal : t -> bt
    val to_string : t -> string
    val pretty_printer : Format.formatter -> t -> unit
    val pretty_printer_ord : Format.formatter -> ordered_float -> unit

    val fmux2 : bt -> t -> t -> t
    val order : (t * t) -> ordered_float
    val align : ordered_float -> ordered_float
    val add : ordered_float -> ordered_float
    val leading_ones : bt -> bt
    val leading_zeros : bt -> bt
    val normalize : ordered_float -> ordered_float
    val round_mantissa : ordered_float -> ordered_float
    val clip_exponent : ordered_float ->ordered_float
    val check_nan : t -> t

    val fadd : t -> t -> t
    val test_fadd : t -> t -> t * t * 
        ordered_float * ordered_float * ordered_float *
        ordered_float * ordered_float * ordered_float * 
        t * t

    val mul_mantissa : t -> t -> bt * bt
    val mul_exponent : t -> t -> bt -> bt

end

(* Construct a floating point module based on the specifications *)
module Make(B : Comb.S)
           (S : FloatSpec)
           (R : Fixed.Round(B).Unsigned) : 
    (FloatBase with type bt = B.t)

module Tagged(B : Comb.S)(G : FloatBase) : Float

