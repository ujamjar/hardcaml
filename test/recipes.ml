open HardCaml
open Signal.Comb
open Recipe
open Monad

let step (a,b,acc) = 
  (srl a 1, sll b 1, mux2 (lsb b) (acc +: a) a)

let mult a b = 
  perform
    (* init accumulator *)
    acc <-- newVar 8;
    Var.set acc (zero 8);
    (* iterate *)
    Var.while_ (fun b -> b <>:. 0) b 
      Tuple3.(apply step (a, b, acc));
    (* return result *)
    return acc

let sys a_in b_in = 
  perform 
    (* create registers *)
    a <-- newVar 8;
    b <-- newVar 8;
    (* set to inputs *)
    Var.apply (fun _ -> a_in) a;
    Var.apply (fun _ -> b_in) b;
    (* serial multiply *)
    acc <-- mult a b;
    (* return output *)
    acc <-- Var.read acc;
    return acc

let recipe = sys (input "a" 8) (input "b" 8)
let fin, mult = follow (input "start" 1) recipe
let circ = Circuit.make "serial_mult" [ output "fin" fin; output "mult" mult ]
let () = Rtl.Verilog.write print_string circ

