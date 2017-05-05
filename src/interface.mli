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

module Gen_cosim2(SIM : Cosim2.Simulator)(B : Comb.S)(I : S)(O : S) : sig
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
  val make : 
    ?check_names:bool -> Circuit.Hierarchy.database -> string -> 
    (Signal.Comb.t I.t -> Signal.Comb.t O.t) ->
    Signal.Comb.t I.t -> Signal.Comb.t O.t
end

module type Ex = sig

  type 'a t

  val t : (string * int) t
  val to_list : 'a t -> 'a list

  val mapname : (string -> 'a) -> 'a t
  val mapbits : (int -> 'a) -> 'a t

  val zip2 : 'a t -> 'b t -> ('a * 'b) t
  val zip3 : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
  val zip4 : 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t
  val zip5 : 'a t -> 'b t -> 'c t -> 'd t -> 'e t -> ('a * 'b * 'c * 'd * 'e) t
  val zip6 : 'a t -> 'b t -> 'c t -> 'd t -> 'e t -> 'f t -> ('a * 'b * 'c * 'd * 'e * 'f) t

  val map : 
    ('a -> 'b) -> 
    'a t -> 'b t
  val map2 : 
    ('a -> 'b -> 'c) ->
    'a t -> 'b t -> 'c t
  val map3 : 
    ('a -> 'b -> 'c -> 'd) ->
    'a t -> 'b t -> 'c t -> 'd t
  val map4 : 
    ('a -> 'b -> 'c -> 'd -> 'e) ->
    'a t -> 'b t -> 'c t -> 'd t -> 'e t
  val map5 : 
    ('a -> 'b -> 'c -> 'd -> 'e -> 'f) ->
    'a t -> 'b t -> 'c t -> 'd t -> 'e t -> 'f t
  val map6 : 
    ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g) ->
    'a t -> 'b t -> 'c t -> 'd t -> 'e t -> 'f t -> 'g t

  val iter : 
    ('a -> unit) -> 
    'a t -> unit
  val iter2 : 
    ('a -> 'b -> unit) ->
    'a t -> 'b t -> unit
  val iter3 : 
    ('a -> 'b -> 'c -> unit) ->
    'a t -> 'b t -> 'c t -> unit
  val iter4 : 
    ('a -> 'b -> 'c -> 'd -> unit) ->
    'a t -> 'b t -> 'c t -> 'd t -> unit
  val iter5 : 
    ('a -> 'b -> 'c -> 'd -> 'e -> unit) ->
    'a t -> 'b t -> 'c t -> 'd t -> 'e t -> unit
  val iter6 : 
    ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> unit) ->
    'a t -> 'b t -> 'c t -> 'd t -> 'e t -> 'f t -> unit

  val offsets : ?rev:bool -> unit -> int t

  module type S = sig

    type b
    type ifs 

    val wire : unit -> ifs
    val wiren : unit -> ifs
    val consti : int -> ifs
    val zero : ifs
    val one : ifs
    val ones : ifs

    val (&:) : ifs -> ifs -> ifs
    val (|:) : ifs -> ifs -> ifs
    val (^:) : ifs -> ifs -> ifs
    val (~:) : ifs -> ifs

    val ( +: ) : ifs -> ifs -> ifs
    val ( -: ) : ifs -> ifs -> ifs
    val ( *: ) : ifs -> ifs -> ifs
    val ( *+ ) : ifs -> ifs -> ifs

    val pack : ?rev:bool -> ifs -> b
    val unpack : ?rev:bool -> b -> ifs

    module L : sig
      type 'a l = 'a list t
      val empty : unit -> 'a l
      val rev : 'a l -> 'a l
      val map : ('a t -> 'b t) -> 'a l -> 'b l
      val cons : 'a t -> 'a l -> 'a l
      val hd : 'a l -> 'a t
      val tl : 'a l -> 'a l
      val of_list : 'a t list -> 'a l
      val to_list : 'a l -> 'a t list
    end

    val mux : b -> ifs list -> ifs
    val mux2 : b -> ifs -> ifs -> ifs
    val concat : ifs list -> ifs
    val select : int -> int -> ifs -> ifs
    val msb : ifs -> ifs
    val msbs : ifs -> ifs
    val lsb : ifs -> ifs
    val lsbs : ifs -> ifs
  
  end

  module Make(B : Comb.S) : S
    with type b = B.t
    and  type ifs = B.t t
  
end

module Ex(X : S) : Ex with type 'a t = 'a X.t

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


