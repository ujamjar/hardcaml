type gen = [ `gen of ('a list -> unit) -> unit ] as 'a

val delay : ?n:int -> gen -> gen

val seq : gen list -> gen

val run : ?n:int -> sim:'a HardCaml.Cyclesim.Api.cyclesim -> gen -> unit

