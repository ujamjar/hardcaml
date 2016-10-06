open Ocamlbuild_plugin

let () = dispatch @@ function
  | Before_options -> Options.use_ocamlfind := true
  | _ -> ()


