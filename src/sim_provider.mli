module type S = functor (B : Bits.Ext.Comb.S) -> sig
  val make : Circuit.t -> B.t Cyclesim.Api.cyclesim
end

val add_provider : string -> (module S) -> unit
val get_provider : string -> (module S)
val load_provider : string -> unit
val load_provider_from_package : string -> string -> unit
