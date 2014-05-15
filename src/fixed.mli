(* 
 * hardcaml - hardware design in OCaml
 *
 *   (c) 2014 MicroJamJar Ltd
 *
 * Author(s): andy.ray@ujamjar.com
 * Description: 
 *
 *)

module Make(B : Comb.S) : sig

    type unsigned
    type signed
    
    type 'a round 
    type 'a overflow 
    
    module type Round = sig
        type t 
        val neg_infinity : t
        val pos_infinity : t
        val to_zero : t
        val away_from_zero : t
        val tie_to_neg_infinity : t
        val tie_to_pos_infinity : t
        val tie_to_zero : t
        val tie_away_from_zero : t
        val tie_to_nearest_even : t
        val tie_to_nearest_odd : t
        val generic : B.t -> t
        val eval : t -> int -> B.t -> B.t
    end

    module type Overflow = sig
        type t 
        val wrap : t
        val saturate : t
        val eval : t -> int -> int -> B.t -> B.t
    end

    module type Fixed = sig
        type t
        val mk : int -> B.t -> t
        val int : t -> B.t
        val frac : t -> B.t
        val signal : t -> B.t
        val width_int : t -> int
        val width_frac : t -> int

        val to_float : t -> float

        val select_int : t -> int -> B.t
        val select_frac : t -> int -> B.t
        val select : t -> int -> int -> t
        val norm : t list -> t list
        val norm2 : t -> t -> t * t
        val const : int -> int -> float -> t

        val (+:) : t -> t -> t
        val (-:) : t -> t -> t
        val ( *: ) : t -> t -> t
        val (==:) : t -> t -> B.t
        val (<>:) : t -> t -> B.t
        val (<:) : t -> t -> B.t
        val (<=:) : t -> t -> B.t
        val (>:) : t -> t -> B.t
        val (>=:) : t -> t -> B.t

        val mux : B.t -> t list -> t

        val resize : t -> int -> int -> t
    end

    module Unsigned : sig
        module Round : (Round with type t = unsigned round)
        module Overflow : (Overflow with type t = unsigned overflow)
        module type Spec = sig
            val round : Round.t
            val overflow : Overflow.t
        end
        module Make(S : Spec) : Fixed
    end

    module Signed : sig
        module Round : (Round with type t = signed round)
        module Overflow : (Overflow with type t = signed overflow)
        module type Spec = sig
            val round : Round.t
            val overflow : Overflow.t
        end
        module Make(S : Spec) : Fixed
    end
        
end


