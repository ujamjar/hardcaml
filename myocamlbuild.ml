open Ocamlbuild_plugin

let () = dispatch @@ function
  | Before_options -> Options.use_ocamlfind := true
  | After_rules -> begin
    rule "-> iocaml.js"
      ~deps:[]
      ~prods:["iocaml.js"]
      (fun env _ ->
        Cmd (S (List.map (fun x -> A x)
          [
            "jsoo_mktop";
              "-verbose";
              "-dont-export-unit"; "gc"; 
              "-top-syntax"; "lwt.syntax"; 
              "-top-syntax"; "js_of_ocaml.syntax";
              "-top-syntax"; "hardcaml.syntax";
              "-export-package"; "lwt";
              "-export-package"; "js_of_ocaml";
              "-export-package"; "hardcaml";
              "-export-package"; "iocamljs-kernel";
              "-jsopt"; "+dynlink.js"; "-jsopt"; "+weak.js";
              "-jsopt"; "+toplevel.js"; "-jsopt"; "+nat.js";
              "-jsopt"; "-I"; "-jsopt"; "./";
              "-o"; "iocaml.byte";
          ]));
      );
    rule "iocaml.js -> kernel.hardcaml.js"
      ~deps:["iocaml.js"]
      ~prods:["kernel.hardcaml.js"]
      (fun env _ ->
         Cmd (S[
           A"cat"; Sh"*.cmis.js";
             Sh"`opam config var lib`/iocamljs-kernel/kernel.js"; A"iocaml.js"; Sh">";
             A"kernel.hardcaml.js"
         ]))
  end
  | _ -> ()


