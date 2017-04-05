open HardCaml

module Make(B : Comb.S)(I : Interface.S)(O : Interface.S) : sig
  
  include Tb.S
    with type state = unit
     and type b = B.t
     and type 'a i = 'a I.t
     and type 'a o = 'a O.t

  val make : 
    string -> 
    (Signal.Comb.t I.t -> Signal.Comb.t O.t) -> 
    (unit -> unit) * (unit -> B.t I.t -> unit * B.t O.t * B.t O.t)

end

