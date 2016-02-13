(* 
 * hardcaml - hardware design in OCaml
 *
 *   (c) 2014 MicroJamJar Ltd
 *
 * Author(s): andy.ray@ujamjar.com
 * Description: 
 *
 *)

(** Circuit interfaces as module - used with camlp4 extension *)

module type S = sig
  type 'a t 
  val t : (string * int) t
  val map : ('a -> 'b) -> 'a t -> 'b t
  val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  val to_list : 'a t -> 'a list
end

module type Empty = sig 
  type 'a t = None
  val t : (string * int) t
  val map : ('a -> 'b) -> 'a t -> 'b t
  val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  val to_list : 'a t -> 'a list
end

module Empty : Empty

val combine : (module S) -> (module S) -> (module S)
val prefix : string -> (module S) -> (module S)
val postfix : string -> (module S) -> (module S)

module Gen(B : Comb.S)(I : S)(O : S) : sig
  val make : string -> (Signal.Comb.t I.t -> Signal.Comb.t O.t) ->
    (Circuit.t * B.t Cyclesim.Api.cyclesim * B.t ref I.t * B.t ref O.t * B.t ref O.t)
end

module Gen_cosim(B : Comb.S)(I : S)(O : S) : sig
  val make : string -> (Signal.Comb.t I.t -> Signal.Comb.t O.t) ->
    (Circuit.t * B.t Cyclesim.Api.cyclesim * B.t ref I.t * B.t ref O.t * B.t ref O.t)
end

module Circ(I : S)(O : S) : sig
  val make : string -> (Signal.Comb.t I.t -> Signal.Comb.t O.t) ->
    Circuit.t
end

module Sim(B : Comb.S)(I : S)(O : S) : sig
  val make : string -> (Signal.Comb.t I.t -> Signal.Comb.t O.t) ->
    (Circuit.t * B.t Cyclesim.Api.cyclesim * B.t ref I.t * B.t ref O.t * B.t ref O.t)
end

module Inst(I : S)(O : S) : sig
  val make : string -> Signal.Comb.t I.t -> Signal.Comb.t O.t
end

module Hier(I : S)(O : S) : sig
  val make : Circuit.Hierarchy.database -> string -> 
    (Signal.Comb.t I.t -> Signal.Comb.t O.t) ->
    Signal.Comb.t I.t -> Signal.Comb.t O.t
end

open Signal.Types

type param = string * int
type ienv = param -> signal
type oenv = param -> signal -> signal

module Tuple : sig type _ t end
module Curried : sig type (_,_) t end

module Fn : sig

  exception Parameter_validation of string * int * int

  val (!) : param -> signal Tuple.t

  val (@) : 'a Tuple.t -> 'b Tuple.t -> ('a * 'b) Tuple.t

  val ( @-> ) : param -> ('a, 'b) Curried.t -> (signal -> 'a, 'b) Curried.t 
  
  val returning : 'a Tuple.t -> ('a, 'a) Curried.t

  type ('a,'b) defn = ('a,'b) Curried.t * 'a

  val define : ('a,'b) Curried.t -> 'a -> ('a,'b) defn 

  val call : ('a,'b) defn -> 'a 

  type inst_env = 
    {
      input : ienv;
      output : oenv;
    }

  val inst : inst_env -> ('a,'b) defn -> 'b 

  val returns : ('a,'b) defn -> 'b -> signal list

  val ioenv : inst_env

  val circuit : string -> ('a,'b) defn -> Circuit.t

end


