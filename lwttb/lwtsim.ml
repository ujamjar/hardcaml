open HardCaml

module Make(B : Comb.S)(I : Interface.S)(O : Interface.S) = struct

  module S = Interface.Sim(B)(I)(O)

  let make name f = 
    let circ,sim,i,o,n = S.make name f in
    let reset () = Cyclesim.Api.reset sim in
    let cycle () i' = 
      ignore @@ I.map2 (:=) i i';
      Cyclesim.Api.cycle sim;
      (), O.map (!) o, O.map (!) n
    in
    reset, cycle

  include Tb.Make(struct type state = unit end)(B)(I)(O)

end

