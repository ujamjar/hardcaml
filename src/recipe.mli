(** Hardware generation in an imperative style *)

(** create sequential hardware designs using [if], [while] and [assignment] *)

open Signal.Comb

type var
type inp
type env
type 'a recipe

module Monad : sig
  val return : 'a -> 'a recipe
  val bind : 'a recipe -> ('a -> 'b recipe) -> 'b recipe
  val (>>=) : 'a recipe -> ('a -> 'b recipe) -> 'b recipe
  val (>>) : 'a recipe -> 'b recipe -> 'b recipe
end

(** skip 1 cycle *)
val skip : unit recipe

(** skip n cycles *)
val wait : int -> unit recipe

(** perform recipes in parallel.  [comb_fin] controls the finish signal
    generation.  When false and extra cycle is taken after the recipes
    complete to generate the [fin] signal.  Otherwise extra combinatorial
    logic is generated to ensure the [fin] signal toggles on the same
    cycle as the last recipe to complete. *)
val par : ?comb_fin:bool -> 'a recipe list -> 'a list recipe

val par2 : ?comb_fin:bool -> 'a recipe -> 'b recipe -> ('a * 'b) recipe

val (|||) : 'a recipe -> 'b recipe -> ('a * 'b) recipe

(** [cond c t f]  performs [t] if [c] is high, otherwise performs [f] *) 
val cond : t -> 'a recipe -> 'b recipe -> unit recipe

(** [iter c t] perform [t] while [c] is high *)
val iter : t -> 'a recipe -> 'a recipe

(** perform recipe forever *)
val forever : 'a recipe -> 'a recipe

(** wait until [t] is low *)
val waitWhile : t -> unit recipe

(** wait until [t] is high *)
val waitUntil : t -> unit recipe 

(** follow recipe and get result *)
val follow : t -> 'a recipe -> t * 'a 

(** create an new [n] bit register *)
val newVar : ?name:string -> int -> var recipe 

(** read value of register *)
val readVar : var -> t recipe 

(** assign list of registers - takes 1 cycle *)
val assign : (var * t) list -> unit recipe 

(** write register with value *)
val writeVar : var -> t -> unit recipe 

(** modify current value of resgiter *)
val modifyVar : (t -> t) -> var -> unit recipe 

(** read a register, modify value, write a second register *)
val rewriteVar : (t -> t) -> var -> var -> unit recipe 

module type Same = sig
  type 'a same 
  val smap : (var -> t) -> var same -> t same
  val szip : var same -> t same -> (var * t) list
  val newVar : unit -> var same recipe
  val read : var same -> t same recipe
  val rewrite : (t same -> t same) -> var same -> var same -> unit recipe
  val apply : (t same -> t same) -> var same -> unit recipe
  val set : var same -> t same -> unit recipe
  val ifte : (t same -> t) -> var same -> 'a recipe -> 'b recipe -> unit recipe
  val while_ : (t same -> t) -> var same -> 'a recipe -> 'a recipe
end

module Same(X : Interface.S) : Same with type 'a same = 'a X.t
module SVar : Same with type 'a same = 'a
module SList : Same with type 'a same = 'a list
module SArray : Same with type 'a same = 'a array
module STuple2 : Same with type 'a same = 'a * 'a
module STuple3 : Same with type 'a same = 'a * 'a * 'a

