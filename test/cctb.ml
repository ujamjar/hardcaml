(* Demonstrate generator based testbenches built with
 * HardCamlCCTB
 *
 * The key concept is to be able to write multiple generators,
 * interacting with different subsets of a circuits interface,
 * and having them all synchronised with the simulators
 * cycle function automatically.
 *
 * Using Delimcc we model python-esque generators with a yield
 * function.  Each generator reads like a mini testbench which
 * calls the yield function to perform a simulation cycle.
 *
 * The CCTB library synchronises all the relevant generators
 * with the simulation.
 *
 * The example below models a circuit with an independant 
 * adder and multiplier.  Different generators drive the
 * two operations, and monitor the outputs.
 *
 * It also demonstrates how new generators and be 'yield'ed
 * dynamically.
 *)

(* utop "test/cctb.ml" *)
#require "hardcaml,hardcaml.cctb,hardcaml-waveterm";;

open HardCaml
open Api
open Comb
open Seq

module W = HardCamlWaveTerm.Wave.Make(HardCamlWaveTerm.Wave.Bits(B))
module Ws = HardCamlWaveTerm.Sim.Make(B)(W)
module Widget = HardCamlWaveTerm.Widget.Make(B)(W)

open Printf

let f a b = reg r_sync enable (a +: b)
let g a b = reg r_sync enable (a *: b)

let a = input "a" 8
let b = input "b" 8
let x = input "x" 8
let y = input "y" 8
let c = output "c" (f a b)
let d = output "d" (g x y)
let circ = Circuit.make "foo" [c;d]
let sim = Cyclesim.make circ
let sim, waves = Ws.wrap sim 

let clear = Cs.in_port sim "clear"
let enable = Cs.in_port sim "enable"
let a = Cs.in_port sim "a"
let b = Cs.in_port sim "b"
let x = Cs.in_port sim "x"
let y = Cs.in_port sim "y"
let c = Cs.out_port_next sim "c"
let d = Cs.out_port_next sim "d"

(********************************************************************)

(********************************************************************)
open HardCamlCCTB.Cycles

(* reset, enable and start generators *)
let rec start_gen = `gen begin fun yield ->
  clear := B.vdd;
  yield [];
  clear := B.gnd;
  enable := B.vdd;
  yield [
    delay add_gen; (* run adder inputs *)
    delay (seq [mul_gen; mul_gen]); (* run multiplier inputs, twice *)
    delay ~n:2 (show 'a' c); (* show adder output *)
    delay ~n:2 (show 'm' d); (* show multiplier output *)
  ]
end

(* set the adder inputs for 10 cycles *)
and add_gen = `gen begin fun yield ->
  for i=0 to 9 do
    a := B.consti 8 (i+1);
    b := B.consti 8 (i+1);
    yield [];
  done
end

(* set the multiplier inputs for 5 cycles *)
and mul_gen = `gen begin fun yield ->
  for i=0 to 4 do
    x := B.consti 8 (i+1);
    y := B.consti 8 (i+1);
    yield [];
  done
end

(* show outputs *)
and show s p = `gen begin fun yield ->
  while true do
    printf "%c = %i\n" s (B.to_int !p);
    yield []
  done
end

(* run testbench *)
let test () = 
  let () = run ~n:18 ~sim start_gen in
  let waveform = new Widget.waveform () in
  let waves = W.{ cfg={default with wave_width=2}; waves } in
  waveform#set_waves waves;
  Lwt_main.run @@ Widget.run_widget waveform

let () = test ()


