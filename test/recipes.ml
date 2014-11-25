open HardCaml
open Signal.Comb
open Recipe
open Monad

(* The interface to the serial multiplier *)

module I = interface
  start[1] a[8] b[8]
end

module O = interface
  fin[1] mult[8]
end

let step (a,b,acc) = 
  (sll a 1, srl b 1, mux2 (lsb b) (acc +: a) acc)

let mult a b acc = 
  perform
    (* iterate *)
    SVar.while_ (fun b -> b <>:. 0) b 
      STuple3.(apply step (a, b, acc));
    (* return result *)
    return acc

let sys a_in b_in = 
  perform 
    (* create registers *)
    a <-- newVar ~name:"var_a" 8;
    b <-- newVar ~name:"var_b" 8;
    acc <-- newVar ~name:"var_acc" 8;
    (* set inputs and clear accumulator *)
    _ <-- par [
      SVar.apply (fun _ -> a_in) a;
      SVar.apply (fun _ -> b_in) b; 
      SVar.set acc (zero 8);
    ];
    (* serial multiply *)
    acc <-- mult a b acc;
    (* return output *)
    acc <-- SVar.read acc;
    return acc

let top i = 
  let fin, mult = follow i.I.start @@ sys i.I.a i.I.b in
  O.{ fin; mult }

module B = Bits.Comb.IntbitsList
module G = Interface.Gen(B)(I)(O)
module S = Cyclesim.Api
module Vcd = Vcd_ext.Make(B)

(*let recipe = sys (input "a" 8) (input "b" 8)
let fin, mult = follow (input "start" 1) recipe
let circ = Circuit.make "serial_mult" [ output "fin" fin; output "mult" mult ]
let () = Rtl.Verilog.write print_string circ
*)

let circ,sim,i,o = G.make "serial_mult" top
let () = Rtl.Verilog.write print_string circ

let test =
  let open I in
  let sim = Vcd.gtkwave ~args:"-S test/gwShowall.tcl" sim in
  let enable = S.in_port sim "enable" in
  S.reset sim;

  for i=0 to 1 do S.cycle sim done;
  enable := B.vdd;
  for i=0 to 1 do S.cycle sim done;
  i.a := B.consti 8 11;
  i.b := B.consti 8 17;
  for i=0 to 3 do S.cycle sim done;
  i.start := B.vdd;
  S.cycle sim;
  i.start := B.gnd;
  for i=0 to 10 do S.cycle sim done;

  ignore @@ input_line stdin




