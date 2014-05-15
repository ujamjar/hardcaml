(* 
 * hardcaml - hardware design in OCaml
 *
 *   (c) 2014 MicroJamJar Ltd
 *
 * Author(s): andy.ray@ujamjar.com
 * Description: 
 *
 *)

(** Basic Xilinx FPGA primitives *)
module type S = 
sig

    open Signal.Types

    val lut : int64 -> signal -> signal

    val muxcy : signal -> signal -> signal -> signal

    val inv : signal -> signal

    val xorcy : signal -> signal -> signal

    val muxf5 : signal -> signal -> signal -> signal

    val muxf6 : signal -> signal -> signal -> signal

    val muxf7 : signal -> signal -> signal -> signal

    val muxf8 : signal -> signal -> signal -> signal

    val fdce : signal -> signal -> signal -> signal -> signal

    val fdpe : signal -> signal -> signal -> signal -> signal

    val mult_and : signal -> signal -> signal
    
    val ram1s : signal -> signal -> signal -> signal -> signal
    
end

(** Allow expressions to generate LUT init values *)
module LutEqn :
sig

    type t 

    val i0 : t
    val i1 : t
    val i2 : t
    val i3 : t
    val i4 : t
    val i5 : t

    val gnd : t
    val vdd : t
    val (&:) : t -> t -> t
    val (|:) : t -> t -> t
    val (^:) : t -> t -> t
    val (~:) : t -> t 
    val (==:) : t -> t -> t
    val (<>:) : t -> t -> t

    val eval : int -> t -> int64

end

(** HardCaml simulation based models of Xilinx primitives *)
module HardCaml_api : S

(** Unisim library based Xilinx primitives *)
module Unisim : S

module type T =
sig

    open Signal.Types

    val x_lut : LutEqn.t -> signal -> signal

    val x_map : LutEqn.t -> signal list -> signal
    
    val x_and : signal -> signal -> signal

    val x_or : signal -> signal -> signal

    val x_xor : signal -> signal -> signal

    val x_not : signal -> signal

    val x_reduce_carry : bool -> (LutEqn.t -> LutEqn.t -> LutEqn.t) -> 
        signal -> signal -> signal -> signal

    val x_and_reduce : signal -> signal
   
    val x_or_reduce : signal -> signal

    val x_reduce_tree : (LutEqn.t -> LutEqn.t -> LutEqn.t) -> signal -> signal

    val x_add_carry : LutEqn.t -> signal -> signal -> signal -> (signal * signal)

    val x_add : signal -> signal -> signal
  
    val x_sub : signal -> signal -> signal

    val x_mux_add_carry : LutEqn.t -> signal -> signal -> (signal * signal) -> signal -> (signal * signal)

    (** [x_mux_add x (a,a') b] gives [(x ? a : a') + b] *)
    val x_mux_add : signal -> (signal * signal) -> signal -> signal

    (** [x_mux_sub x a (b,b')] gives [a - (x ? b : b')] *)
    val x_mux_sub : signal -> signal -> (signal * signal) -> signal

    val x_eq : signal -> signal -> signal

    val x_lt : signal -> signal -> signal

    val x_mux : signal -> signal list -> signal

    val x_mulu : signal -> signal -> signal

    val x_muls : signal -> signal -> signal

end

module type LutSize = sig val max_lut : int end
module Lut4 : LutSize
module Lut6 : LutSize

module XMake(X : S)(L : LutSize) : T

module XComb(Synth : T) : (Comb.T with type t = Signal.Types.signal)

(* combinatorial only transform *)
module XSynthesizeComb(X:S)(L:LutSize) : Transform.TransformFn
(* sequential and combinatorial transform TODO memories *)
module XSynthesize(X:S)(L:LutSize) : Transform.TransformFn

