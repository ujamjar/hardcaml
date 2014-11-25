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

val skip : unit recipe
val wait : int -> unit recipe
val par2 : ?comb_fin:bool -> 'a recipe -> 'b recipe -> ('a * 'b) recipe
val (|||) : 'a recipe -> 'b recipe -> ('a * 'b) recipe
val par : ?comb_fin:bool -> 'a recipe list -> 'a list recipe
val cond : t -> 'a recipe -> 'b recipe -> unit recipe
val iter : t -> 'a recipe -> 'a recipe
val forever : 'a recipe -> 'a recipe
val waitWhile : t -> unit recipe 
val waitUntil : t -> unit recipe 
val follow : t -> 'a recipe -> t * 'a 
val newVar : ?name:string -> int -> var recipe 
val readVar : var -> t recipe 
val assign : (var * t) list -> unit recipe 
val writeVar : var -> t -> unit recipe 
val modifyVar : (t -> t) -> var -> unit recipe 
val rewriteVar : (t -> t) -> var -> var -> unit recipe 

module type Same = sig
  type 'a same 
  val smap : (var -> t) -> var same -> t same
  val szip : var same -> t same -> (var * t) list
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

