open HardCaml

module B = Bits.Comb.ArraybitsInt32

(** [compile_shared_lib name] compile, with gcc, [<name>.c] to [lib<name>.so] *)
val compile_shared_lib : string -> unit

(** Load the shared library (recommend using an absolute path) and create a simulator *)
val make_from_shared_lib : string -> B.t Cyclesim.Api.cyclesim

(** compile and load a C simulator (default name is "tmp") *)
val make : ?name:string -> Circuit.t -> B.t Cyclesim.Api.cyclesim

