#!/usr/bin/env ocaml
#use "topfind"
#require "topkg,astring"
open Topkg

let ctypes = Conf.with_pkg ~default:false "ctypes"
let ctypes_foreign = Conf.with_pkg ~default:false "ctypes-foreign"
let camlp4 = Conf.with_pkg ~default:false "camlp4"
let js_of_ocaml = Conf.with_pkg ~default:false "js_of_ocaml"
let lwt = Conf.with_pkg ~default:false "lwt"

let mlpack ?cond name =  
  let dir = Fpath.dirname name in
  let base = Fpath.basename name in
  let parse contents =
    let lines = String.cuts ~sep:'\n' contents in
    let add_mod acc l =
      let m = String.trim @@ match String.cut ~sep:'#' l with
      | None -> l
      | Some (m, _ (* comment *)) -> m
      in
      if m = "" then acc else m :: acc
    in
    Ok (List.fold_left add_mod [] lines)
  in
  let modls = (* modules within the pack *)
    let name = Fpath.(dir // base ^ ".mlpack") in
    OS.File.read name >>= parse
  in
  let intf modls = (* install interfaces for modules in the library - .cmti/.mli *)
    Ok (List.map 
      (fun m ->
         let name = Fpath.(dir // Astring.String.Ascii.uncapitalize m) in
           Pkg.lib ?cond ~exts:Exts.(exts [".cmti"; ".mli"]) name
      ) modls)
  in
  let name = Fpath.(dir // base) in
  let pkg l = 
    let lib = 
      Pkg.lib ?cond 
        ~exts:Exts.(exts [".a"; ".cmi"; ".cma"; ".cmx"; ".cmxa"; ".cmxs"; ".cmti"]) 
        name 
    in
    Ok (lib :: l)
  in
  (modls >>= intf >>= pkg) |> Log.on_error_msg ~use:(fun () -> [])

let () = 
  Pkg.describe "hardcaml" @@ fun c ->
  let ctypes = Conf.value c ctypes && Conf.value c ctypes_foreign in
  let camlp4 = Conf.value c camlp4 in
  let js_of_ocaml = Conf.value c js_of_ocaml in
  let lwt = Conf.value c lwt in
  Ok (
    mlpack "src/HardCaml" @
    mlpack "dynlink/HardCamlDynlink" @
    mlpack ~cond:(js_of_ocaml && lwt) "js/HardCamlJS" @
    mlpack ~cond:ctypes "csim/HardCamlCSim" @
    (if camlp4 then 
       [ Pkg.lib ~exts:Exts.(exts [".cmo"; ".cmx"; ".cmi"; ".cmti"]) "syntax/pa_hardcaml" ] 
     else [])
  )

