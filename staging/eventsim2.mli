(* 
 * hardcaml - hardware design in OCaml
 *
 *   (c) 2014 MicroJamJar Ltd
 *
 * Author(s): andy.ray@ujamjar.com
 * Description: 
 *
 *)

open HardCaml

module B : Comb.S

type value = B.t
type time = int

module Signal : sig

    type t = 
        {
            (* current value *)
            mutable value : value;
            (* time of last transition *)
            mutable time : time;
            (* debug name *)
            name : string;
        }

    val mk : string -> int -> t

end

module Event : sig

    type t = 
        {
            (* time of even *)
            time : time;
            (* value of event *)
            value : value;
            (* signal to change *)
            signal : Signal.t;
        }

    val mk : time -> value -> Signal.t -> t

end

module Process : sig

    type t = 
        {
            sensitivity : Signal.t list;
            run : unit -> unit;
        }

    val mk : Signal.t list -> (unit -> unit) -> t

end



