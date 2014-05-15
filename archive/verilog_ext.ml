(* 
 * hardcaml - hardware design in OCaml
 *
 *   (c) 2014 MicroJamJar Ltd
 *
 * Author(s): andy.ray@ujamjar.com
 * Description: 
 *
 *)

open HardCaml

let write chan circ =
    let b = Verilog.write circ in
    output_string chan (Buffer.contents b)

module Testbench = 
struct

    let write chan a b c = 
        let b = Verilog.Testbench.write a b c in
        output_string chan (Buffer.contents b)

end

module Hierarchy = 
struct

    open Signal.Types

    let vwrite = write
    let inst_name = function
        | Signal_inst(_,_,i) -> i.inst_name
        | _ -> failwith "expecting instantiation"

    let rec write ?(transforms=[]) database path circ = 
        
        let name = Circuit.name circ in
        let f = open_out (path ^ name ^ ".v") in
        (* write this module *)
        let circ = 
            List.fold_left (fun circ fn ->
                Transform.rewrite_circuit fn circ) circ transforms
        in
        vwrite f circ;
        close_out f;
        (* find instantiations *)
        let insts = Circuit.search Circuit.id 
            (fun l s -> if is_inst s then s::l else l)
            [] (Circuit.outputs circ)
        in
        List.iter (fun inst ->
            let name = inst_name inst in
            try
                match Circuit.Hierarchy.get database name with
                    | Some(c) -> write ~transforms:transforms database path c
                    | None -> ()
            with 
            | Circuit.Failure(e) -> failwith ("error generating " ^ name ^ ": " ^ e)
        ) insts

end

