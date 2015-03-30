open HardCaml

module type S = functor (B : Bits_ext.S) -> sig
  val make : Circuit.t -> B.t Cyclesim.Api.cyclesim
end

let providers = ref []

let add_provider name modl = providers := (name,modl) :: !providers

let get_provider name = List.assoc name !providers 

let load_provider name = 
  let name = Dynlink.adapt_filename name in
  Dynlink.loadfile name

let load_provider_from_package package plugin = 
  let ch = Unix.open_process_in ("ocamlfind query -format \"%d\" " ^ package) in
  let path = input_line ch in
  let _ = Unix.close_process_in ch in
  load_provider (Filename.concat path plugin)

