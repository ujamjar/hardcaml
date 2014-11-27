open HardCaml
open Signal.Comb
open Recipe
open Monad

module B = Bits.Comb.IntbitsList
module S = Cyclesim.Api
module Vcd = Vcd_ext.Make(B)

module Test_mult = struct

  (* interface to the serial multiplier *)
  module I = interface
    start[1] a[8] b[8]
  end

  module O = interface
    fin[1] mult[8]
  end

  module State = interface
    a[8] b[8] acc[8]
  end
  module SState = Same(State)

  (* shift 'a' up and 'b' down while 'b' does not equal zero.  
  * Add 'b' to the accumulator when the lsb of 'b' is non-zero. *)
  let step s = 
    let open State in
    { a = sll s.a 1; b = srl s.b 1; acc=mux2 (lsb s.b) (s.acc +: s.a) s.acc }

  let mult a_in b_in = 
    let open State in
    perform 
      (* create registers *)
      state <-- SState.newVar();
      (* set inputs and clear accumulator *)
      SState.apply (fun _ -> { a=a_in; b=b_in; acc=zero 8 }) state;
      (* serial multiplier *)
      SVar.while_ (fun b -> b <>:. 0) state.b 
        SState.(apply step state);
      (* return output *)
      acc <-- SVar.read state.acc;
      return acc

  let top i = 
    let fin, mult = follow i.I.start @@ mult i.I.a i.I.b in
    O.{ fin; mult }

  (* simulation *)

  module G = Interface.Gen(B)(I)(O)

  let test () =
    let open I in
    let circ,sim,i,o = G.make "serial_mult" top in
    (*let () = Rtl.Verilog.write print_string circ*) 

    let sim = Vcd.gtkwave ~args:"-S test/gwShowall.tcl" sim in
    let enable = S.in_port sim "enable" in
    S.reset sim;

    enable := B.vdd;
    i.a := B.consti 8 11;
    i.b := B.consti 8 17;
    i.start := B.vdd;
    S.cycle sim;
    i.start := B.gnd;
    for i=0 to 10 do S.cycle sim done;

    ignore @@ input_line stdin

end

(* need arrays, indexed by signals, to complete this *)
module Test_sha1 = struct

  module I = interface
    start[1] d[32] d_valid[1]
  end

  module O = interface
    fin[1] hash{|5|}[32] redundant[1]
  end

  module State = interface
    counter[7] h{|5|}[32] (*w{|16|}[32]*)
  end
  module SState = Same(State)

  let sha1 i = 
    let open I in
    let open State in
    perform
      state <-- SState.newVar();
      (* reset state *)
      SState.apply (fun _ -> 
        { 
          counter=zero 7; 
          h=Array.map (consti32 32) 
            [| 0x67452301l; 0xEFCDAB89l; 0x98BADCFEl; 0x10325476l; 0xC3D2E1F0l |];
          (*w=Array.init 16 (fun _ -> consti 32 0);*)
        }) state;
      SVar.while_ (fun s -> s <:. 16) state.counter
        begin perform
          (SVar.apply (fun d -> d +:. 1) state.counter);
        end;
      SState.while_ (fun s -> s.counter <:. 20) state
        begin perform
          SVar.apply (fun d -> d +:. 1) state.counter;
        end;
      SState.while_ (fun s -> s.counter <:. 40) state
        begin perform
          SVar.apply (fun d -> d +:. 1) state.counter;
        end;
      SState.while_ (fun s -> s.counter <:. 60) state
        begin perform
          SVar.apply (fun d -> d +:. 1) state.counter;
        end;
      SState.while_ (fun s -> s.counter <:. 80) state
        begin perform
          SVar.apply (fun d -> d +:. 1) state.counter;
        end;
      SState.read state

  let top i = 
    let fin, state = follow i.I.start @@ sha1 i in
    O.{ 
      fin; 
      hash=state.State.h;
      redundant = reduce (|:) (I.to_list @@ I.map lsb i);    
    }

  module G = Interface.Gen(B)(I)(O)

  let test () =
    let open I in
    let circ,sim,i,o = G.make "sha1" top in

    let sim = Vcd.gtkwave ~args:"-S test/gwShowall.tcl" sim in
    let enable = S.in_port sim "enable" in
    S.reset sim;

    enable := B.vdd;
    i.start := B.vdd;
    S.cycle sim;
    i.start := B.gnd;
    for i=0 to 90 do S.cycle sim done;

    ignore @@ input_line stdin

  (*let () = test ()*)

end


