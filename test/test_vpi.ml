#require "hardcaml"

open HardCaml
open Signal.Comb
open Signal.Seq

let f a b = 
  let c = a +: b in
  let d = reg r_full enable c in
  c, d

let c, d = f (input "a" 2) (input "b" 2)
let c, d = output "c" c, output "d" d

let circ = Circuit.make "testadd" [ c; d ]

module B = Bits.Comb.IntbitsList
module Co = Cosim.Make(B)
module S = Cyclesim.Api

let sim = Co.make ~dump_file:"testadd.vcd" circ

let clear = S.in_port sim "clear"
let enable = S.in_port sim "enable"
let a = S.in_port sim "a"
let b = S.in_port sim "b"
let c = S.out_port sim "c"
let d = S.out_port sim "d"


