(* 
 * hardcaml - hardware design in OCaml
 *
 *   (c) 2014 MicroJamJar Ltd
 *
 * Author(s): andy.ray@ujamjar.com
 * Description: 
 *
 *)

open Astring
open Signal.Types
open Signal.Base

module UidMap = Signal.Types.UidMap
module UidSet = Signal.Types.UidSet

exception Failure of string
let failwith str = raise (Failure str)

module Mangler = 
struct

    type t = (string, int) Hashtbl.t

    let lookup tbl name = 
        try Hashtbl.find tbl name with _ -> -1

    let addfresh tbl name = 
        if lookup tbl name <> -1 then failwith ("Cannot add fresh name to the mangler that already exists: " ^ name);
        Hashtbl.add tbl name 0

    let make reserved = 
        let tbl = Hashtbl.create 1000 in
        List.iter (fun name -> addfresh tbl name) reserved;
        tbl

    let rec mangle tbl name = 
        let v = lookup tbl name in
        if v = -1 then
        begin
            Hashtbl.add tbl name 0;
            name
        end
        else
        begin
            Hashtbl.replace tbl name (v+1);
            mangle tbl (name ^ "_" ^ string_of_int v)
        end

end

type t = 
    {
        (* circuit name *)
        circ_name : string;
        (* map id's to signals *)
        circ_id_to_sig : signal UidMap.t;
        (* circuit inputs *)
        circ_inputs : signal list;
        (* circuit outputs *)
        circ_outputs : signal list;
        (* fanout's *)
        circ_fanout :  UidSet.t UidMap.t;
        (* fanin's *)
        circ_fanin :  UidSet.t UidMap.t;
    }

let rec search1' set pre post arg signal = 
    if UidSet.mem (uid signal) set then
        (arg, set)
    else
        let set = UidSet.add (uid signal) set in
        let arg = pre arg signal in
        let arg,set = search' set pre post arg (deps signal) in
        let arg = post arg signal in
        arg, set
and search' set pre post arg signals =
    List.fold_left (fun (arg,set) s -> search1' set pre post arg s) (arg,set) signals

let search1 pre post arg signal = fst (search1' UidSet.empty pre post arg signal)
let search pre post arg signal = fst (search' UidSet.empty pre post arg signal)

let id arg signal = arg

let find_inputs outputs = 
    let build_input_list arg signal =
        match signal with
        | Signal_wire(_,d) ->
            if !d = empty then
                match List.length (names signal) with
                | 0 -> failwith "Input found with no name"
                | 1 -> signal :: arg
                | _ -> failwith ("Input found with multiple names: " ^ String.concat ~sep:" " (names signal))
            else 
                arg
        | _ -> 
            arg
    in
    search build_input_list id [] outputs

let find_signals f outputs = 
    let f a s = if f s then s::a else a in
    search f id [] outputs

let find_signals_by_name name = 
    let f s = List.mem name (Signal.Types.names s) in
    find_signals f

let map_ids_to_sigs outputs =
    let add_signal map signal = 
        let uid = uid signal in
        UidMap.add uid signal map
    in
    search add_signal id UidMap.empty outputs

let find_fanout deps outputs = 
    let add_fanin map signal = 
        let uid_target = uid signal in
        List.fold_left (fun map driver ->
            let uid_driver = uid driver in
            let l = try UidMap.find uid_driver map with | _ -> [] in
            UidMap.add uid_driver (uid_target :: l) map
        ) map (deps signal)
    in
    let m = search add_fanin id UidMap.empty outputs in
    UidMap.map 
        (fun v -> List.fold_left (fun s v -> UidSet.add v s) UidSet.empty v) 
        m

let find_fanin deps m = 
    UidMap.map 
        (fun s -> List.fold_left 
                    (fun s v -> UidSet.add (uid v) s) 
                    UidSet.empty (deps s)
        ) m

(* return list of signals where 'f signal' evaluates to true *)
let filter f outputs = 
    search (fun arg signal -> if f signal then signal :: arg else arg) id [] outputs

exception Found_CombinatorialLoop of string

(* well this wont work...we really want to be able to backtrack during the
 * recursion to be honest (continuation passing? exceptions?) 
let check_combinatorial_loops outputs = 
    let wires = filter (function Signal_wire(_) -> true | _ -> false) outputs in
    (* search from each wire *)
    List.iter (fun wire ->
        search1 (fun _ x -> 
            if uid x = uid wire then 
                raise (Found_CombinatorialLoop(to_string wire))
        ) id () wire
    ) wires
*)

let check_output signal = 
    match signal with
    | Signal_wire(_) ->
        if deps signal <> [] then
            match List.length (names signal) with
            | 0 -> failwith "Output found with no name"
            | 1 -> () 
            | _ -> failwith "Output has multiple names"
        else 
            failwith "Output must be an assigned wire"
    | _ -> 
        failwith "Output must be a wire"

let set_of_signals signals = 
    List.fold_left (fun set signal -> UidSet.add (uid signal) set) UidSet.empty signals

let make name outputs = 
    (* check that all outputs are assigned wires with 1 name *)
    List.iter check_output outputs;
    (* check for combinatorial loops *)
    (* check_combinatorial_loops outputs; *)
    (* construct the circuit *)
    let map = map_ids_to_sigs outputs in
    {
        circ_name = name;
        circ_id_to_sig = map;
        circ_inputs = find_inputs outputs;
        circ_outputs = outputs;
        circ_fanout = find_fanout deps outputs;
        circ_fanin = find_fanin deps map;
    }

let inputs c = c.circ_inputs
let outputs c = c.circ_outputs
let name c = c.circ_name

module IntPairMap = Map.Make(struct
    type t = uid * int
    let compare = compare
end)

(* mangle all names - creating a map of signal uid's to names 
 * First, add all inputs, outputs and reserved words - they must not be
 * mangled or things get very confusing.  Then the rest of the signals. *)
let mangle_names reserved prefix circuit =
    let inputs = inputs circuit in
    let outputs = outputs circuit in
    let ios = inputs @ outputs in
    let ioset = set_of_signals ios in
    (* initialise the mangler with all reserved words and the inputs+outputs *)
    let mangler = Mangler.make ((List.map (fun x-> List.hd (names x)) ios) @ reserved) in
    (* initialize the name map with inputs and outputs *)
    let name_map = List.fold_left (fun map signal -> 
        IntPairMap.add (uid signal,0) (List.hd (names signal)) map) IntPairMap.empty ios in
    (* add name to mangler and name map *)
    let generated_name map uid = 
        let name = Mangler.mangle mangler (prefix ^ Int64.to_string uid) in
        IntPairMap.add (uid,0) name map
    in
    let add_name map signal = 
        let is_io signal = UidSet.mem (uid signal) ioset in 
        if is_io signal || signal = empty then
            (* IO signal names are handled seperately (they are not mangled) *)
            map
        else if names signal = [] then
            (* generated name *)
            generated_name map (uid signal)
        else
            (* mangle signal names *)
            fst (List.fold_left (fun (map,i) name -> 
                let name = Mangler.mangle mangler name in
                IntPairMap.add (uid signal,i) name map, i+1
            ) (map,0) (names signal))
    in
    (* add special case for memories and instantiations *)
    let add_name map signal =
        match signal with
        | Signal_mem(_,u,_,_) -> add_name (generated_name map u) signal
        | Signal_inst(_,u,_) -> add_name (generated_name map u) signal
        | _ -> add_name map signal
    in 
    let name_map = search (fun map signal -> add_name map signal) id name_map outputs in 
    let lookup name index = IntPairMap.find (name,index) name_map in
    lookup

let is_input circuit signal = List.mem signal (inputs circuit)
let is_output circuit signal = List.mem signal (outputs circuit)
let signal_of_uid circuit uid = UidMap.find uid circuit.circ_id_to_sig

let signal_map c = c.circ_id_to_sig

let structural_compare c0 c1 = 
    try 
        (* check full structural comparision from each output *)
        List.fold_left2 
            (fun b s t -> b && (Signal.Types.structural_compare s t))
            true (outputs c0) (outputs c1)
    with Not_found ->
        false

(*
let dump circuit = 
    Printf.printf "name: %s\n" (name circuit);
    let sname signal = 
        let uid = Int64.to_string (uid signal) in
        let name = 
            let rec str names = 
                match names with
                | [] -> ""
                | [a] -> a
                | a::t -> a ^ " " ^ str t
            in
            str (names signal)
        in
        if name="" then uid
        else uid^"["^name^"]"
    in
    Printf.printf "inputs:\n";
    List.iter (fun s -> Printf.printf "  %s\n" (sname s)) (inputs circuit);
    Printf.printf "outputs:\n";
    List.iter (fun s -> Printf.printf "  %s\n" (sname s)) (outputs circuit);
    Printf.printf "nodes:\n";
    UidMap.iter (fun uid s -> Printf.printf "  %s\n" (Signal.Comb.to_string s))
        circuit.circ_id_to_sig;

    Printf.printf "done.\n"
*)

(*
odule Plugin = 
struct

    type param_elt = 
        | Int of int
        | String of string
        | Float of float
        | Bool of bool
        | List of param_elt list
    type param = string * param_elt

    let get_int = function
        | Int(n) -> n
        | _ -> failwith ("Not an int param") 

    let get_string = function
        | String(n) -> n
        | _ -> failwith ("No a string param") 

    let get_float = function
        | Float(n) -> n
        | _ -> failwith ("No a float param")

    let get_bool = function
        | Bool(n) -> n
        | _ -> failwith ("No a bool param") 

    let get_list = function
        | List(n) -> n
        | _ -> failwith ("Not a list param") 

    type input = string * int
    type get_inputs = param list -> input list
    type get_circuit = param list -> 
                         (string * Signal.Types.signal) list -> 
                         t

    type loadable_circuit =
        {
            params : param list;
            get_inputs : get_inputs;
            get_circuit : get_circuit;
        }

    let module_linker_var : loadable_circuit list ref = ref []

    let load name config_params config_inputs = 
        let _ = Dynlink.loadfile (Dynlink.adapt_filename name) in
        List.map (fun m ->
            (* get circuit parameters *)
            let params = m.params in
            (* config parameters *)
            let params = config_params params in
            (* get circuit inputs *)
            let inputs = m.get_inputs params in
            (* config inputs  - but only those that are settable *)
            let inputs = config_inputs inputs in
            (* create signal inputs *)
            let inputs = List.map (fun (n,w) -> n, Signal.Comb.input n w) inputs in
            (* build circuit *)
            let circuit = m.get_circuit params inputs in
            (* return the circuit outputs for further processing *)
            circuit
        ) !module_linker_var

    (* called by the plugin to register the module with the runtime *)
    let register m = 
        module_linker_var := 
            List.map (fun (p,i,c) ->
            {
                params = p;
                get_inputs = i;
                get_circuit = c;
            }) m

end
*)

module Hierarchy = 
struct

    (* during description we will call an 'inst' method.
     * this will create an 'inst' node and also generate the
     * actual sub-circuit.  The sub-circuit will be added to
     * the 'database' here.
     *
     * When verilog gets written instantiations will be looked
     * up in the 'database'.  If they exist they will be written 
     * as well.  This way we can create a hierarchical design 
     * and have all the verilog generated automatically.
     *
     * One (serious) complication is the naming and generics of
     * circuits.  Basically, we dont support generics in HardCaml
     * but thats not to say 2 different instantiations of some
     * circuit is not parameterized in some way.  That is 2 circuits
     * with the same name may not have the same implementation and 
     * we need to detect this.
     *
     * The best way to do this would seem to be to compare 
     * the generated circuits when they are added, and if they
     * do not match (structurally) we need to rename the circuit.
     * If they do match then just share it.
     *
     * A simpler alternative would be to just always rename them,
     * but especially for oft used small elements (ie simple registers)
     * this will force massive replication.
     *)

    type entry = 
        {
            name : string; (* actual circuit name *)
            mangled_name : string; (* mangled circuit name *)
            circ : t; (* the circuit *)
        }

    type database =
        {
            mangler : Mangler.t;
            mutable entries : entry list; (* a list might not be efficient *)
        }

    let empty () = 
        {
            mangler = Mangler.make [];
            entries = [];
        }
    
    (* add a circuit to the database, and return an instantiation for it,
     * with a possibly mangled name *)
    let add database circ = 
        let name = name circ in
        (* find the existing circuits which started off with this 
         * (un-mangled) name *)
        let ents,_ = 
            List.partition (fun e -> e.name = name) database.entries 
        in
        try 
            (* look for a circuit which exactly matches *)
            let rec find e = 
                match e with
                | [] -> raise Not_found
                | e'::t ->
                    if structural_compare e'.circ circ then
                        e'
                    else
                        find t
            in
            let e = find ents in
            (* if we get here, we have a match, so construct an inst *)
            e.mangled_name 

        with Not_found ->
            (* add a new entry - nothing matches *)
            let mangled_name = Mangler.mangle database.mangler name in
            let e = 
                {
                    name = name;
                    mangled_name = mangled_name;
                    circ = { circ with circ_name = mangled_name };
                }
            in
            database.entries <- e :: database.entries;
            e.mangled_name 

    let get database name = 
        try 
            Some( (List.find (fun e -> e.mangled_name = name) database.entries).circ )
        with Not_found ->
            None

end

