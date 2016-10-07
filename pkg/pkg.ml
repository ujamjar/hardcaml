#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let ctypes = Conf.with_pkg ~default:false "ctypes-foreign"
let camlp4 = Conf.with_pkg ~default:false "camlp4"

let mlpack ?(en=true) prefix = 
  if en then 
    [
      Pkg.mllib (prefix ^ ".mlpack");
      Pkg.lib (prefix ^ ".cmi");
      Pkg.lib (prefix ^ ".cmti");
      Pkg.lib (prefix ^ ".cmx");
    ]
  else 
    []

let () = 
  Pkg.describe "hardcaml" @@ fun c ->
  let ctypes = Conf.value c ctypes in
  let camlp4 = Conf.value c camlp4 in
  Ok (
    mlpack "src/HardCaml" @
    mlpack "dynlink/HardCamlDynlink" @
    mlpack "js/HardCamlJS" @
    mlpack ~en:ctypes "csim/HardCamlCSim" @
    (if camlp4 then [ Pkg.lib "syntax/pa_hardcaml.cmo" ] else [])
  )

