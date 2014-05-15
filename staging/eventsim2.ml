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

module B = Bits.Comb.IntbitsList

type value = B.t
type time = int

module Signal = struct

    type t = 
        {
            (* current value *)
            mutable value : value;
            (* time of last transition *)
            mutable time : time;
            (* debug name *)
            name : string;
        }

    let mk name width = 
        {
            value = B.zero width;
            time = 0;
            name = name;
        }

end

module Event = struct

    type t = 
        {
            (* time of event *)
            time : time;
            (* value of event *)
            value : value;
            (* signal to change *)
            signal : Signal.t;
        }

    let mk time value signal = 
        {
            time = time;
            value = value;
            signal = signal;
        }

end

module Process = struct

    type t = 
        {
            sensitivity : Signal.t list;
            run : unit -> unit
        }

    let mk sensitivity run = 
        {
            sensitivity = sensitivity;
            run = run;
        }

    let (<==) a b = 

end

let a = Signal.mk "a" 8
let b = Signal.mk "b" 8

let proc = Process.mk [a] (fun () -> 
(*    b <== after 1 a;  *)
    ()
)





