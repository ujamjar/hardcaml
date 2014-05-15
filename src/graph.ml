(* 
 * hardcaml - hardware design in OCaml
 *
 *   (c) 2014 MicroJamJar Ltd
 *
 * Author(s): andy.ray@ujamjar.com
 * Description: 
 *
 *)

open Printf
open Utils
open Signal.Types
open Signal.Comb
open Circuit

(* write a DOT file with rank information - looks absolutely terrible *)
let write_dot_rank chan circuit = 

    (*
    let get_name signal = 
        (*
        match names signal with
        | [] -> "_" ^ Int64.to_string (uid signal)
        | h::t -> h
        *)
        Int64.to_string (uid signal)
    in*)

    let outputs = List.fold_left 
        (fun set signal -> UidSet.add (uid signal) set) 
        UidSet.empty (Circuit.outputs circuit)
    in

    (* create a suitable fanout mapping *)
    let fdeps s = 
        match s with
        | Signal_mem(_,_,_,m) -> [List.hd (deps s)]
        | Signal_reg(_) -> [List.hd (deps s)]
        | _ -> deps s
    in
    let fanout = find_fanout fdeps (Circuit.outputs circuit) in
    let find_fanout signal = 
        try UidSet.elements (UidMap.find signal fanout)
        with _ -> []
    in

    let uids l = List.map uid l in

    (* we start at the inputs, and traverse forward to the outputs, effectively
     * using depth first search *)
    let rec dfs visited ranks signals = 
        match signals with
        | [] -> ranks
        | _ ->
            (* add these signals to the visited set *)
            let visited = List.fold_left 
                (fun set signal -> UidSet.add signal set) 
                visited signals
            in
            (* find fanout from this level *)
            let signals = 
                List.concat (List.map 
                    (fun signal -> find_fanout signal) signals)
            in
            (* filter out already visited signals, and outputs *)
            let signals = List.filter 
                (fun signal -> (not (UidSet.mem signal visited)) &&
                               (not (UidSet.mem signal outputs))) 
                signals 
            in
            (* create set of uids of nodes at this rank *)
            let rank = 
                List.fold_left 
                    (fun set signal -> UidSet.add signal set)
                    UidSet.empty signals
            in
            if rank = UidSet.empty then ranks
            else
                dfs visited (rank :: ranks) signals
    in
    let ranks = 
        dfs UidSet.empty [] (Circuit.inputs circuit |> uids) |> 
        List.map UidSet.elements 
    in

    (* create the output level and add it to the ranks *)
    let ranks = 
        List.map (fun s -> uid s) (Circuit.outputs circuit) :: ranks |>
        List.rev
    in
    let nranks = List.length ranks in

    
    (* write the bit to the left *)
    
    fprintf chan "digraph %s {\n" (Circuit.name circuit);
    
    for i=0 to nranks-1 do
        fprintf chan "%i" i;
        if i <> nranks-1 then
            fprintf chan " -> ";
    done;
    fprintf chan "\n";
    iteri (fun i s ->
        fprintf chan " { rank=same; %i [shape=plaintext];\n" i;
        List.iter (fun s -> 
            fprintf chan "  _%Li;\n" s
        ) s;
        fprintf chan "}\n";
    ) ranks;

    (* write edges *)
    Circuit.search 
        (fun a s ->
            List.iter (fun d ->
                fprintf chan "_%Li -> _%Li;\n" (uid d) (uid s)
            ) (fdeps s)
        )
        (fun a s -> a)
        ()
        (Circuit.outputs circuit);

    fprintf chan "}\n"

(* GDL file with manhatten layout - looks much, much nicer *)
let write_gdl ?(names=false) ?(widths=false) ?(consts=true) ?(clocks=false)
    chan circuit = 

    let quote s = "\"" ^ s ^ "\"" in

    fprintf chan "graph: {\n";
    
    let props = [
        "title", quote (Circuit.name circuit);
        "manhattenedges", "yes";
        "inportsharing", "no";
        "outportsharing", "yes";
        "node.bordercolor", "lightblue";
    ]
    in
    let props = 
        if widths then 
            ("display_edge_labels", "yes") :: props
        else 
            props
    in

    (* write list of default attributes *)
    List.iter (fun (a,b) ->
        fprintf chan "%s: %s\n" a b
    ) props;

    let folds c s = List.fold_left 
        (fun s n -> if s = "" then n else n ^ c ^  s) "" s 
    in
    
    let name s = 
        let names = Signal.Types.names s in
        match names with
        | [] -> ""
        | h :: [] -> h
        | h :: t -> h ^ " (" ^ folds "," t ^ ")"
    in

    let write_node 
        ?(border="invisible") ?(shape="box") ?(label="") 
        ?(bordercolour="") ?(colour="") ?(textcolour="")
        signal =
        fprintf chan "node: { title: \"%Li\" " (uid signal);
        let name = if label = "" || names then name signal else "" in
        (match label,name with
        | "","" -> fprintf chan "label: \"none\" " 
        | _,"" -> fprintf chan "label: \"%s\" " label
        | "",_ -> fprintf chan "label: \"\\fI%s\" " name
        | _ -> fprintf chan "label: \"%s\\n\\fI%s\" " label name);
        fprintf chan "shape: %s " shape;
        fprintf chan "borderstyle: %s " border;
        if textcolour <> "" then
            fprintf chan "textcolor: %s " textcolour;
        if bordercolour <> "" then
            fprintf chan "bordercolor: %s " bordercolour;
        if colour <> "" then
            fprintf chan "color: %s " colour;
        fprintf chan " }\n"
    in

    let is_rom s = 
        match s with
        | Signal_op(_,Signal_mux) ->
            List.fold_left (fun b s -> b && is_const s) true (List.tl (deps s))
        | _ -> false
    in
    let reg_deps s =
        match s with
        | Signal_reg(_,r) ->
            (if clocks then [r.reg_clock] else []) @
            [List.hd (deps s); r.reg_enable]
        | _ -> []
    in
    let mem_deps s =
        match s with
        | Signal_mem(_,_,r,m) ->
                [List.hd (deps s); r.reg_enable; 
                 m.mem_read_address;
                 m.mem_write_address]
        | _ -> []
    in

    let is_input s = Circuit.is_input circuit s in
    let is_output s = Circuit.is_output circuit s in

    (* write nodes *)
    let write_node s = 
        match s with
        | Signal_empty -> 
            write_node ~label:"empty" s
        | Signal_const(i,c) ->
            write_node ~label:(hstr_of_bstr Unsigned c) s
        | Signal_wire(i,_) ->
             if Signal.Types.names s = [] then write_node ~textcolour:"lightgrey" ~label:"wire" s
             else if is_input s then write_node ~textcolour:"red" s
             else if is_output s then write_node ~textcolour:"red" s
             else write_node ~textcolour:"lightgrey" s
        | Signal_select(i,hi,lo) -> 
            write_node ~textcolour:"lightgrey" ~label:(sprintf "[%i:%i]" hi lo) s
        | Signal_op(i,op) -> 
            (match op with
            | Signal_add -> write_node ~border:"solid" ~shape:"circle" ~label:"+" s
            | Signal_sub -> write_node ~border:"solid" ~shape:"circle" ~label:"-" s
            | Signal_mulu -> write_node ~border:"solid" ~shape:"circle" ~label:"*" s
            | Signal_muls -> write_node ~border:"solid" ~shape:"circle" ~label:"*+" s
            | Signal_and -> write_node ~border:"solid" ~shape:"circle" ~label:"&" s
            | Signal_or -> write_node ~border:"solid" ~shape:"circle" ~label:"|" s
            | Signal_xor -> write_node ~border:"solid" ~shape:"circle" ~label:"^" s
            | Signal_eq -> write_node ~border:"solid" ~shape:"circle" ~label:"=" s
            | Signal_not -> write_node ~border:"solid" ~shape:"circle" ~label:"~" s
            | Signal_lt -> write_node ~border:"solid" ~shape:"circle" ~label:"<" s
            | Signal_cat -> write_node ~border:"solid" ~shape:"trapeze" ~label:"cat" s
            | Signal_mux -> 
                if is_rom s then
                    let els = List.length (deps s) - 1 in
                    write_node ~border:"solid" ~shape:"box" 
                    ~label:(sprintf "rom%i" els) s
                else
                    write_node ~border:"solid" ~shape:"uptrapeze" ~label:"mux" s
            )
        | Signal_reg(i,r) ->
            write_node ~bordercolour:"lightblue" ~textcolour:"white" 
                ~colour:"black" ~border:"solid" ~label:"reg" s
        | Signal_mem(i,_,_,m) ->
            write_node ~bordercolour:"lightblue" ~textcolour:"white" 
                ~colour:"black" ~border:"solid" ~label:(sprintf "mem%i" m.mem_size) s
        | Signal_inst(_,_,i) ->
            write_node ~border:"solid" ~label:(sprintf "inst\n%s" i.inst_name) s
    in

    (* specialised dependancies *)
    let deps s = 
        if is_rom s then [List.hd (deps s)]
        else if is_reg s then reg_deps s
        else if is_mem s then mem_deps s
        else deps s
    in

    (* write edges *)
    let write_edges () = 
        search 
            (fun a s -> 
                let deps = deps s |> List.filter ((<>) empty) in
                let deps = 
                    if consts then deps
                    else deps |> List.filter (fun s -> not (is_const s))
                in
                if deps <> [] && s <> empty then
                    (List.iter (fun d ->
                        (* Note; labels always specified, even if they are disabled *)
                        fprintf chan 
                            "edge: { source: \"%Li\" target: \"%Li\" "
                            (uid d) (uid s);
                        if ((is_wire s) && (not (is_output s))) || (is_select s) then
                            fprintf chan "arrowstyle: none ";
                        fprintf chan 
                            "color:lightgrey thickness: 1 label: \"%i\" }\n"
                            (width d);
                    ) deps;
                    List.fold_left (fun a s -> UidSet.add (uid s) a) a (s :: deps))
                else
                    a
            )
            (fun a _ -> a)
            UidSet.empty
            (Circuit.outputs circuit)
    in
    let nodes = write_edges () in

    UidSet.iter (fun u -> write_node (Circuit.signal_of_uid circuit u)) nodes;

    fprintf chan "}\n"

let aisee3 ?(args="") 
    ?(names=false) ?(widths=false) ?(consts=true) ?(clocks=false) 
    circuit = 
    let name,file = Filename.open_temp_file "aisee3" ".gdl" in
    write_gdl ~names:names ~widths:widths ~consts:consts ~clocks:clocks file circuit;
    close_out file;
    ignore (Unix.open_process_in ("aisee3 " ^ name ^ " " ^ args))

