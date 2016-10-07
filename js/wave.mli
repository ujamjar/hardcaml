module B = HardCaml.Bits.Comb.IntbitsList

type exarray = { mutable len : int; mutable data : B.t array; }
val make : unit -> exarray
val extend : exarray -> unit
val set : exarray -> int -> B.t -> unit
val get : exarray -> int -> B.t
val length : exarray -> int
val data : exarray -> B.t array

type wave = { name : string; nbits : int; data : exarray; }
type waves = wave array

val wrap : B.t HardCaml.Cyclesim.Api.cyclesim -> B.t HardCaml.Cyclesim.Api.cyclesim * wave array

module Gui :
  sig
    module D = Dom_html

    val render_1 :
      int * int ->
      int * int ->
      int -> D.canvasRenderingContext2D Js.t -> B.t array -> unit
    val render_n :
      ('a -> string) ->
      int * int ->
      int * int -> int -> D.canvasRenderingContext2D Js.t -> 'a array -> unit

    val mk_wave_table : #Dom.node Js.t -> int -> int -> wave array -> unit
  end

