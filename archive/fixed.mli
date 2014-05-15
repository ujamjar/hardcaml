(* 
 * hardcaml - hardware design in OCaml
 *
 *   (c) 2014 MicroJamJar Ltd
 *
 * Author(s): andy.ray@ujamjar.com
 * Description: 
 *
 *)

module Round(B : Comb.S) : sig
    open B

    val negInfinity : t
    val posInfinity : t
    val toZero : t
    val awayFromZero : t
    val tieToNegInfinity : t
    val tieToPosInfinity : t
    val tieToZero : t
    val tieAwayFromZero : t
    val tieToNearestEven : t
    val tieToNearestOdd : t

    module type Generic = sig
        val sel : t
    end

    module type Unsigned = sig
        val f : int -> t -> t          
    end
    module Unsigned : sig
        module NegInfinity : Unsigned
        module PosInfinity : Unsigned
        module ToZero : Unsigned
        module AwayFromZero : Unsigned
        module TieToNegInfinity : Unsigned
        module TieToPosInfinity : Unsigned
        module TieToZero : Unsigned
        module TieAwayFromZero : Unsigned
        module TieToNearestEven : Unsigned
        module TieToNearestOdd : Unsigned
        module Generic(G : Generic) : Unsigned
    end

    module type Signed = sig
        val f : int -> t -> t          
    end
    module Signed : sig
        module NegInfinity : Signed
        module PosInfinity : Signed
        module ToZero : Signed
        module AwayFromZero : Signed
        module TieToNegInfinity : Signed
        module TieToPosInfinity : Signed
        module TieToZero : Signed
        module TieAwayFromZero : Signed
        module TieToNearestEven : Signed
        module TieToNearestOdd : Signed
        module Generic(G : Generic) : Signed
    end
end

module Overflow(B : Comb.S) : sig
    open B

    module type Unsigned = sig 
        val f : int -> int -> t -> t
    end
    module Unsigned : sig
        module Wrap : Unsigned
        module Saturate : Unsigned
    end

    module type Signed = sig
        val f : int -> int -> t -> t
    end
    module Signed : sig
        module Wrap : Signed
        module Saturate : Signed
    end

end

module type Fixed = sig
    type bt
    type t = { s : bt; fp : int; }

    val mk : int -> bt -> t
    val int : t -> bt
    val frac : t -> bt
    val signal : t -> bt
    val width_int : t -> int
    val width_frac : t -> int

    val to_float : t -> float

    val select_int : t -> int -> bt
    val select_frac : t -> int -> bt
    val select : t -> int -> int -> t
    val norm : t list -> t list
    val norm2 : t -> t -> t * t
    val const : int -> int -> float -> t

    val (+:) : t -> t -> t
    val (-:) : t -> t -> t
    val ( *: ) : t -> t -> t
    val (==:) : t -> t -> bt
    val (<>:) : t -> t -> bt
    val (<:) : t -> t -> bt
    val (<=:) : t -> t -> bt
    val (>:) : t -> t -> bt
    val (>=:) : t -> t -> bt

    val mux : bt -> t list -> t

    val resize : t -> int -> int -> t
end

module Signed
    (B : Comb.S)
    (R : Round(B).Signed) 
    (O : Overflow(B).Signed) : (Fixed with type bt = B.t)

module Unsigned
    (B : Comb.S)
    (R : Round(B).Unsigned) 
    (O : Overflow(B).Unsigned) : (Fixed with type bt = B.t)

