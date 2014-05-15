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
open Printf

(*
 
(* might be usable as the ordered event list *)

module type LH_S =
sig

    type t
    type elt

    val empty : t
    val merge : t -> t -> t
    val insert : t -> elt -> t
    val min : t -> elt 
    val delete : t -> t
    val is_empty : t -> bool 

end

module LH_Make = functor (ORD : Set.OrderedType) ->
struct 

    exception Failure of string
    let failwith str = raise (Failure str)


    type elt = ORD.t 

    type t = 
        | E
        | T of int * elt * t * t

    let empty = E

    let rank = function
        | E -> 0
        | T(r,_,_,_) -> r

    let makeT x a b = 
        if compare (rank a) (rank b) >= 0 then
            T(rank b + 1, x, a, b)
        else
            T(rank a + 1, x, b, a)

    let rec merge h1 h2 = 
        match (h1,h2) with
        | (E, h) -> h
        | (h, E) -> h
        | T(_,x,a1,b1), T(_,y,a2,b2) ->
            if cggompare x y <= 0 then
                makeT x a1 (merge b1 h2)
            else
                makeT y a2 (merge h1 b2)

    let insert h x = merge (T(1,x,E,E)) h
    let min = function T(_,x,_,_) -> x | _ -> failwith "cant find min in empty heap"
    let delete = function T(_,x,a,b) -> merge a b | _ -> failwith "cant delete from empty tree"
    let is_empty = function E -> true | _ -> false

end
*)

module Make(B : Comb.S) = 
struct 

    type time = int
    type value = B.t
    type id = int

    let verbose' = ref false

    module Event = 
    struct
        type t = 
            {
                time : time;
                value : value;
                signal : id;
            }
        let compare a b = compare a.time b.time
        let time a = a.time
        let value a = a.value
        let signal a = a.signal
        let mk t v s = 
            {
                time = t;
                value = v;
                signal = s;
            }

        let to_string e = 
            "[" ^ 
            "t:" ^ string_of_int e.time ^ 
            " v:" ^ B.to_string e.value ^
            " s:" ^ string_of_int e.signal ^ 
            "]"

    end

    module type OrderedEventsSig =
    sig

        type t
        val empty : t
        val lowest_time : t -> time option
        val pop_lowest : t -> (t * Event.t list)
        val count : t -> int
        val add : t -> Event.t -> t
        val add_list : Event.t list -> t -> t
        val to_list : t -> Event.t list
    end

    module OrderedEvents : OrderedEventsSig =
    struct

        type t = Event.t list

        let empty = []

        let lowest_time el = 
            try
                Some (Event.time (List.hd (List.sort (Event.compare) el)))
            with _ ->
                None

        let pop_lowest el = 
            let time = match lowest_time el with 
            | Some(t) -> t 
            | None -> failwith "cant pop lowest elements" in
            let matches = List.filter (fun x -> Event.time x = time) el in
            let others = List.filter (fun x -> Event.time x <> time) el in
            others, matches

        let count = List.length

        let add el e = e :: el

        let add_list els e = els @ e

        let to_list e = e

    end

    module SignalDatabase =
    struct

        (* hmmm..we need to use a global database of signals as we can't fully
         * detect those signals which are part of the simulation just from the
         * sensitivity lists of the processes.  In particular memories, and
         * registers are a problem.  We could make them work properly by including
         * enables etc in the sensitivity lists, but this makes much more work
         * for the simulator to do.  More esoteric things would also not be
         * possible (though their use is somewhat debatable). *)
        type t = 
            {
                name : string; (* XXX debugging *)
                (* prev : value ref - last value - accesible from vhdl *)
                value : value ref;
                id : int;
            }
        let empty = { name=""; value=ref B.empty; id=0 }

        (* simple resizable array *)
        type database = 
            {
                mutable data : t array;
                mutable length : int;
            }

        let database = 
            {
                data = Array.create 4 empty;
                length = 0;
            }

        let compare a b = compare a b

        let mk_id () = 
            let x = database.length in
            database.length <- database.length + 1;
            x

        let add s = 
            let size = Array.length database.data in
            if size <= database.length then
            begin
                let data = database.data in (* old array *)
                database.data <- Array.create (size*2) empty;
                Array.iteri (fun i x -> database.data.(i) <- x) data;
            end;
            database.data.(s.id) <- s

        let get_signals () = 
            Array.init 
                database.length 
                (fun i -> database.data.(i)) 

        let mk value name = 
            let s = 
                {
                    name = name;
                    value = ref value;
                    id = mk_id ();
                }
            in
            add s; s

        (* accessing parts of signals *)
        let value s = !(s.value)
        let ref s = s.value
        let name s = s.name
        let id s = s.id
        let set s d = s.value := d

    end

    module Process = 
    struct
        (* API used in processes *)
        module Control = 
        struct 
            type t = 
                {
                    (* create a simulator event *)
                    event : time -> value -> SignalDatabase.t -> Event.t;
                    (* test if there was an event on a signal *)
                    active : SignalDatabase.t -> bool;
                    (* get value of signal *)
                    value : SignalDatabase.t -> value;
                }
            let mk event active value = 
                {
                    event = event;
                    active = active;
                    value = value;
                }
        end
        (* type of processes *)
        type t = 
            {
                name : string; (* to make debugging easier *)
                sensitivity_list : SignalDatabase.t list;
                (*run : Event.t list -> time -> time -> Event.t list;*)
                run : Control.t -> time -> time -> Event.t list;
            }

        let sensitivity_list p = p.sensitivity_list
        let run p = p.run
        let mk n s r =
            let r' e t d = 
                printf "%10s[%3i][%3i]: " n t d;
                let e = r e t d in
                List.iter (fun e -> printf "%s " (Event.to_string e)) e;
                printf "\n";
                e
            in
            let r = if !verbose' then r' else r in
            {
                name = n;
                sensitivity_list = s;
                run = r;
            }

    end

    module Id = 
    struct
        type t = id
        let compare = compare
    end

    module IdSet = Set.Make(Id)
    module IdMap = Map.Make(Id)
    module SignalSet = Set.Make(SignalDatabase)
    module SignalMap = Map.Make(SignalDatabase)

    module Simulator = 
    struct

        (* no more event *)
        exception SimOver
        (* simulation failed for some reason *)
        exception SimError of string

        let verbose = false
        let failwith s = raise (SimError s)
        let simover () = raise SimOver

        type simulation = 
            {
                signals : SignalDatabase.t array;
                signal_event : bool array;
                drivers : IdSet.t array;
                processes : Process.t array;
                control : Process.Control.t;
                event_list : OrderedEvents.t;
            }

        let make processes = 
            (* get signal database *)
            let signals = SignalDatabase.get_signals () in
            if verbose then 
                (printf "found %i signals\n" (Array.length signals); 
                Array.iter (fun s -> printf "%i " (SignalDatabase.id s)) signals; 
                printf "\n");

            let find_signal signal = 
                let j = ref 0 in
                for i=0 to Array.length signals - 1 do
                    if signal = signals.(i) then
                    begin
                        j := i
                    end
                done;
                !j
            in
                
            (* create drivers based on process sensitivity lists *)
            let drivers = Array.init (Array.length signals) (fun i -> IdSet.empty) in
            let processes = Array.of_list processes in
            Array.iteri 
                (fun i p ->
                    List.iter 
                        (fun s -> let s = find_signal s in drivers.(s) <- IdSet.add i drivers.(s)) 
                        (Process.sensitivity_list p)
                ) processes;
            let signal_event = Array.init (Array.length signals) (fun _ -> false) in
            let signal_find s = SignalDatabase.id s in
            let ctrl = 
                Process.Control.mk
                    (fun t v s -> Event.mk t v (signal_find s))
                    (fun s -> signal_event.(signal_find s))
                    (fun s -> SignalDatabase.value (signals.(signal_find s)));
            in
            (* simulation structure *)
            {
                signals = signals;
                signal_event = signal_event;
                drivers = drivers;
                processes = processes;
                control = ctrl;
                event_list = OrderedEvents.empty;
            }

        let add sim e = { sim with event_list = OrderedEvents.add_list e sim.event_list }

        let set_all_active sim b = 
            for i=0 to Array.length sim.signal_event - 1 do
                sim.signal_event.(i) <- b;
            done

        let init 
            ?(active=false)    (* set all signals to this state initially *)
            ?(initial=false)  (* only run processes with no sensitivity list *)
            sim = 
            (* run all processes once with all signals set to active to get initial event list *)
            (* XXX lets try only those with no sensitivy list  XXX *)
            set_all_active sim active;
            let sim = 
                { sim with event_list =
                    OrderedEvents.add_list
                        (Array.fold_left (fun e p ->
                            if initial && (Process.sensitivity_list p <> []) then e
                            else Process.run p sim.control 0 (-1) @ e
                        ) [] sim.processes) OrderedEvents.empty
                }
            in
            set_all_active sim false;
            sim

        (* run a list of events, possibly generating new ones *)
        let run_events sim events time delta = 
            (* filter events to only those which cause a change in value *)
            let events = 
                List.filter 
                    (fun e -> Event.value e <> SignalDatabase.value sim.signals.(Event.signal e))
                    events
            in
            (* work out the set of processes we need to run *)
            let processes = List.fold_left (fun p e ->
                    IdSet.union sim.drivers.(Event.signal e) p
                ) IdSet.empty events
            in
            if verbose then (
                printf "processes\n";
                IdSet.iter (fun p -> printf " %i " p) processes;
                printf "\n");
            (* apply changes to signal values *)
            List.iter (fun e -> 
                SignalDatabase.set sim.signals.(Event.signal e) (Event.value e);
                sim.signal_event.(Event.signal e) <- true
            ) events;
            (* run the processes affected by these signals and get a new set of events *)
            let new_events = 
                let run p e = (Process.run p sim.control time delta) @ e in
                IdSet.fold 
                    (fun p e -> run sim.processes.(p) e) 
                    processes []
            in
            if verbose then printf "exe    new=%i\n" (List.length new_events);
            (* reset signal events *)
            set_all_active sim false;
            (* return simulation with new event list *)
            { sim with event_list = 
                List.fold_left 
                    (fun a e -> OrderedEvents.add a e) 
                    sim.event_list new_events }

        (* run a delta step *)
        let rec delta_step sim time delta = 
            let next_time = match OrderedEvents.lowest_time sim.event_list with
            | Some(t) -> t
            | None -> simover () in
            if verbose then (
                printf "delta  next=%4i time=%4i pending=%4i\n" next_time time
                    (OrderedEvents.count sim.event_list);
                Array.iter (fun s -> printf " [%s]" (B.to_string (SignalDatabase.value s))) sim.signals;
                printf "\n");
            if next_time = time then
                let events, next_events = OrderedEvents.pop_lowest sim.event_list in
                if verbose then (
                    printf "events next=%4i now =%4i\n" (List.length next_events) (OrderedEvents.count events);
                    List.iter (fun e -> 
                        printf " time=%4i value=%s signal=%4i\n" 
                            (Event.time e) (B.to_string (Event.value e)) (Event.signal e)
                    ) next_events);
                delta_step 
                    (run_events {sim with event_list=events} next_events time delta) 
                    time (delta+1);
            else 
                sim

        (* run simulation to given time *)
        let rec run sim time = 
            let next_time = match OrderedEvents.lowest_time sim.event_list with
            | Some(t) -> 
                if verbose then printf "run    next=%4i time=%4i pending=%4i\n" t time
                    (OrderedEvents.count sim.event_list);
                t
            | None -> simover () in
            if verbose then 
                List.iter (fun e ->
                    printf " time=%4i value=%s signal=%4i\n" 
                        (Event.time e) (B.to_string (Event.value e)) (Event.signal e)
                ) (OrderedEvents.to_list sim.event_list);
            if next_time < time then
                run (delta_step sim next_time 0) time
            else
                sim

        module Circuit =
        struct

            let verbose = false

            (* hackery, jiggery *)
            let smk x n = 
                let y = SignalDatabase.mk x n in
                if !verbose' then printf " > %20s w=%4i id=%4i\n" n (B.width x) (SignalDatabase.id y);
                y
            let sid = SignalDatabase.id

            open Signal.Types
            open Circuit
            module S = Signal.Comb

            open Process.Control

            (* map from signals -> Signal.t *)

            let build circuit = 
                let const b n = smk (B.constb b) n in
                let mk w n = smk (B.zero w) n in
                let mk_signals (smap,p,i) s = 
                    let names = if s = S.empty then ["empty"] else names s in
                    let name n = if names=[] then n else List.hd names in
                    let s' = match s with
                    | Signal_empty -> 
                        smk B.empty (name "empty")
                    | Signal_const(_) -> 
                        const (const_value s) (name "const")
                    | Signal_op(_,op) -> 
                        mk (S.width s) (name ("op" ^ Signal.Types.string_of_op op))
                    | Signal_wire(_,d) -> 
                        mk (S.width s) (name "wire")
                    | Signal_select(_,h,l) -> 
                        mk (S.width s) (name "sel")
                    | Signal_reg(_,r) -> 
                        mk (S.width s) (name "reg")
                    | Signal_mem(_,_,r,m) -> 
                        mk (S.width s) (name "mem")
                    | Signal_inst(_) -> failwith "Instantiations are not supported in simulation"
                    in 
                    UidMap.add (uid s) s' smap, p, i
                in
                let null = Process.mk "null" [] (fun _ _ _ -> []), None in
                let mk_processes (smap,p,i) s = 
                    let find s = UidMap.find (uid s) smap in
                    let p',i' = match s with
                    | Signal_empty -> null
                    | Signal_const(_) -> null
                    | Signal_op(_,op) ->
                    begin 
                        let op' = op in
                        let deps = deps s in
                        let op2 op = 
                            let a = find (List.nth deps 0) in
                            let b = find (List.nth deps 1) in
                            let d = find s in
                            Process.mk ("op2"^string_of_op op') [a;b] (fun e t _ -> 
                                [e.event t (op (e.value a) (e.value b)) d]),
                            None
                        in 
                        match op with
                        | Signal_add -> op2 B.(+:) 
                        | Signal_sub -> op2 B.(-:) 
                        | Signal_mulu -> op2 B.( *: ) 
                        | Signal_muls -> op2 B.( *+ )
                        | Signal_and -> op2 B.(&:)
                        | Signal_or -> op2 B.(|:)
                        | Signal_xor -> op2 B.(^:)
                        | Signal_eq -> op2 B.(==:)
                        | Signal_not -> 
                            let a = find (List.nth deps 0) in
                            let d = find s in
                            Process.mk "op~:" [a] (fun e t _ -> 
                                [e.event t (B.(~:) (e.value a)) d]), 
                            None
                        | Signal_lt -> op2 B.(<:)
                        | Signal_cat -> 
                            let deps = List.map find deps in
                            let q = find s in
                            Process.mk "cat" deps (fun e t d ->
                                [e.event t (B.concat (List.map e.value deps)) q]),
                            None
                        | Signal_mux -> 
                            let deps = List.map find deps in
                            let ctrl,data = List.hd deps, List.tl deps in
                            let q = find s in
                            Process.mk "mux" deps (fun e t d ->
                                [e.event t 
                                    (B.mux (e.value ctrl) (List.map e.value data)) 
                                    q]),
                            None
                    end
                    | Signal_wire(_,d) -> 
                        let name = try List.hd (names s) with _ -> "wire" in
                        if !d = S.empty then 
                            (* input node.  We need to do something clever here as
                             * this is how the testbench will be interfaced *)
                            let input_signal = mk (S.width s) (name ^ "_in") in
                            let s' = find s in
                            Process.mk "input"
                                [ input_signal ] 
                                (fun e t _ -> 
                                    [e.event t (e.value input_signal) s']),
                            Some(s,input_signal)
                        else
                            let d = find !d in
                            let s = find s in
                            Process.mk "wire"
                                [d] 
                                (fun e t _ -> 
                                    [e.event t (e.value d) s]),
                            None
                    | Signal_select(_,h,l) -> 
                        let x = find (List.hd (deps s)) in
                        let q = find s in
                        Process.mk "sel" [x] (fun e t d ->
                            [e.event t (B.select (e.value x) h l) q]),
                        None
                    | Signal_reg(_,r) -> 
                        let d = find (List.hd (deps s)) in
                        let clock = find r.reg_clock in
                        let clock_level = find r.reg_clock_level in
                        let has_reset = r.reg_reset <> S.empty in
                        let reset = find r.reg_reset in
                        let reset_level = find r.reg_reset_level in 
                        let reset_value = find r.reg_reset_value in 
                        let has_clear = r.reg_clear <> S.empty in
                        let clear = find r.reg_clear in
                        let clear_level = find r.reg_clear_level in
                        let clear_value = find r.reg_clear_value in
                        let has_enable = r.reg_enable <> S.empty in
                        let enable = find r.reg_enable in
                        let q = find s in
                        Process.mk "reg" 
                            [clock;reset;
                             (* XXX we dont really want these in the sensitivity list,
                              * as they are not really on what we trigger.  However,
                              * we use the sensitivity list as the essential guide
                              * to what signals actually exist.  
                              * This is where the whole scheme effectively fails! *)
                             clock_level; 
                             (* reset;reset_level;reset_value;
                             clear;clear_value;clear_value;
                             enable;d *)
                            ] (fun e t _ ->
                            if has_reset && (e.value reset = e.value reset_level) then
                                [e.event t (e.value reset_value) q]
                            else if (e.active clock) && (e.value clock = e.value clock_level) then
                                if has_clear && (e.value clear = e.value clear_level) then
                                    [e.event t (e.value clear_value) q]
                                else if (not has_enable) || (e.value enable = B.vdd) then
                                    [e.event t (e.value d) q]
                                else
                                    []
                            else
                                []), 
                        None
                    | Signal_mem(_,_,r,m) -> failwith "FIXME: memories are not yet supported in event based simulation"
                    | Signal_inst(_) -> failwith "Instantiations are not supported in simulation"
                    in 
                    let i = match i' with None -> i | Some(i') -> i'::i in
                    smap, p' :: p, i
                in
                let smap,processes,inputs = 
                    Circuit.search 
                        mk_signals mk_processes (UidMap.empty,[],[]) 
                        (Circuit.outputs circuit)
                in
                (* create 1 more process, which reads all the outputs *)
                let outputs, output_reader = 
                    let find s = UidMap.find (uid s) smap in
                    let outputs' = Circuit.outputs circuit in
                    let outputs = List.map (fun o -> find o) outputs' in 
                    List.combine outputs' outputs, Process.mk "outputs" outputs (fun e _ _ -> 
                        (*printf " -> ";
                        List.iter (fun v ->
                            printf "%s " (B.to_string (e.value v))) outputs;*)
                        []
                    )
                in
                let processes = output_reader :: processes in
                if verbose then (
                    printf "created %i processes\n" (List.length processes);
                    List.iter 
                        (fun p -> 
                            printf ": ";
                            List.iter 
                                (fun s -> (* printf "%i : " (sid s) *) () ) 
                                (Process.sensitivity_list p);
                            printf "\n"
                        ) processes;
                );
                
                processes, inputs, outputs

            let rec find_signal io name = match io with
            | [] -> failwith ("failed to lookup signal name " ^ name)
            | (r,s) :: b ->
                if List.hd (names r) = name then s
                else find_signal b name

        end

    end

    (* convert API to cycle based simulator for convenience *)
    module Cyclesim = 
    struct

        open Process.Control
        open Cyclesim.Api

        type t = B.t
        type cyclesim = t Cyclesim.Api.cyclesim

        let make circuit = 
            let unwrap = function Some(x) -> x | _ -> failwith "Cant unwrap value" in
            let hp = 5 in
            let p,i,o = Simulator.Circuit.build circuit in
            let part name i = 
                List.partition (fun (s,ss) -> List.mem name (Signal.Types.names s)) i
            in
            (* pull out the reset and cycle signal(s) from the inputs *)
            let reset,others = part "reset" i in
            let clock,others = part "clock" others in
            let reset = try Some(snd (List.hd reset)) with _ -> None in
            let clock = try snd (List.hd clock) with _ -> failwith "no clock" in

            let name s = List.hd (Signal.Types.names s) in
            let width s = Signal.Comb.width s in
            (* inputs and outputs *)
            let inputs' = List.map (fun (s,ss) -> name s, s, ss, ref (B.zero (width s))) others in
            let outputs' = List.map (fun (s,ss) -> name s, s, ss, ref (B.zero (width s))) o in 

            (* create processes *)
            let clock_pr = 
                Process.mk "clock" [clock] (fun e t d ->
                    if d = -1 then [e.event 0 B.vdd clock]
                    else [e.event (t+hp) (B.(~:) (e.value clock)) clock])
            in
            let inputs_pr = 
                Process.mk "inputs" [clock] (fun e t d ->
                    if B.vdd = (e.value clock) then
                        (* copy inputs at rising edge of clock *)
                        List.map (fun (n,s,ss,x) -> e.event t (!x) ss) inputs'
                    else
                        [])
            in
            let outputs_pr = 
                Process.mk "outputs" [clock] (fun e t d ->
                    if B.gnd = (e.value clock) then
                    begin
                        (* copy outputs at falling edge of clock *)
                        List.iter (fun (n,s,ss,x) -> x := (e.value ss)) outputs' ;
                        []
                    end
                    else
                        [])
            in
            let sim = Simulator.make (inputs_pr::outputs_pr::clock_pr::p) in
            let sim = Simulator.init sim in
            let time = ref 0 in
            let sim = ref sim in
            let reset_fn () = 
                if reset <> None then
                begin
                    let reset = unwrap reset in
                    (* generate reset and clock events (need to be careful with the
                     * clock actually - there might be events there already *)
                    let events = 
                        [ 
                            Event.mk (!time+     0) B.vdd (SignalDatabase.id reset);
                            Event.mk (!time+(hp*2)) B.gnd (SignalDatabase.id reset);
                            Event.mk (!time+     0) B.vdd (SignalDatabase.id clock);
                            Event.mk (!time+    hp) B.gnd (SignalDatabase.id clock);
                        ] 
                    in
                    sim := Simulator.add !sim events;
                    sim := Simulator.run !sim !time;
                    time := !time + (hp*2)
                end
            in
            let cycle_fn = 
                (fun () -> 
                    sim := Simulator.run !sim !time;
                    time := !time + (hp*2))
            in
            {
                sim_in_ports = List.map (fun (a,b,c,d) -> a,d) inputs';
                sim_out_ports = List.map (fun (a,b,c,d) -> a,d) outputs';
                sim_internal_ports = [];
                sim_reset = reset_fn;
                sim_cycle = cycle_fn;
                sim_cycle_comb = (fun () -> ());
            }

    end

    (* generate VCD traces *)
    module Vcd = 
    struct

        module V = Vcd.Make(B)

    end

end


module Test = 
struct
    
    module S = Make(Bits.Comb.Int64bits)
    module B = Bits.Comb.Int64bits

    open S
    open Process.Control
(*
    let test = 
        try

            let s0 = Signal.mk 0 in
            let s1 = Signal.mk 0 in
            let s2 = Signal.mk 0 in
            let s3 = Signal.mk 0 in
            let s4 = Signal.mk 0 in
            let s5 = Signal.mk 0 in

            (* generation of 'initial' events *)
            let p0 = Process.mk [] (fun e t d -> 
                assert (t = 0 && d = (-1));
                printf "*** P0 time=%4i delta=%4i\n" t d;
                [ e.event 0 0 s0;
                  e.event 10 1 s3; e.event 10 2 s2;
                  e.event 20 7 s0; e.event 30 6 s1;
                ] ) 
            in
            let p1 = Process.mk [s0;s1] (fun e t d -> 
                printf "*** P1 time=%4i delta=%4i\n" t d;
                [e.event t ((e.value s0) + (e.value s1)) s4]
            ) 
            in
            let p2 = Process.mk [s2;s3] (fun e t d -> 
                printf "*** P2 time=%4i delta=%4i\n" t d;
                [e.event t ((e.value s2) + (e.value s3)) s5]
            ) in
            (* monitoring *)
            let p3 = 
                let sens = [s0;s1;s2;s3;s4;s5] in
                Process.mk sens
                    (fun e t d -> 
                        printf "*** P3 time=%4i delta=%4i : " t d;
                        List.iter 
                            (fun s -> 
                                printf "%i[%s] " 
                                    (e.value s) 
                                    (if e.active s then "*" else " "); 
                                flush stdout) 
                            sens;
                        printf "\n"; flush stdout;
                        []
                    ) 
            in

            let sim = Simulator.create [p0;p1;p2;p3] in
            let sim = Simulator.init sim in
            printf "Init done\n";
            flush stdout;

            ignore (Simulator.run sim 31);
            (* ignore (Simulator.delta_step sim 0 0); *)
            printf "Event sim.\n";
            ()

        with Simulator.SimOver ->
            printf "Sim over.\n"

*)

    open Signal.Comb
    open Signal.Seq

    (* this is a very simple test of just a combinational operator *)
    let test_0 ()= 
        try
            (* hardcaml circuit *)
            let a = input "a" 8 in
            let b = input "b" 8 in
            let c = a +: b in
            let c = output "c" c in
            let circuit = Circuit.make "testing" [c] in

            (* build the processes and signals for the DUT *)
            let processes,inputs,outputs = Simulator.Circuit.build circuit in

            (* access to port signals *)
            let i name = Simulator.Circuit.find_signal inputs name in
            (*let o name = Simulator.Circuit.find_signal outputs name in*)
            let a = i "a" in
            let b = i "b" in
            (*let c = o "c" in*)

            (* now we have to create our driver processes *)
            let drive_adder = 
                Process.mk "driver"
                    [] 
                    (fun e t _ -> 
                        [
                            e.event 30 (B.consti 8 4) a; 
                            e.event 30 (B.consti 8 5) b; 
                            e.event 35 (B.consti 8 6) b; 
                        ]
                    )
               (*
                  process begin
                      wait for 30 ns;
                      a <= 4;
                      b <= 5;
                      wait for 5 ns;
                      b <= 6;
                      wait;
                  end process;
                *)
            in
            let processes = drive_adder :: processes in

            (* build the simulator *)
            let sim = Simulator.make processes in
            let sim = Simulator.init sim in
            let sim = Simulator.run sim 50 in
            ignore (sim)
        with 
        | Simulator.SimOver -> printf "sim over.\n"
        | Simulator.SimError(s) -> printf "sim error: %s\n" s

    (* SR latch.  The key point is we can model circuits like these under an
       event driven simulator - combinatorial feedback. *)
    let test_1 () = 
        try
            let r = input "r" 1 in
            let s = input "s" 1 in
            let q,qb = wire 1, wire 1 in
            let q' = ~: (r &: qb) in
            let qb' = ~: (s &: q) in
            q <== q';
            qb <== qb';
            let circuit = Circuit.make "testing" [ output "q" q; output "qb" qb ] in
            let processes,inputs,outputs = Simulator.Circuit.build circuit in
            let i name = Simulator.Circuit.find_signal inputs name in
            let o name = Simulator.Circuit.find_signal outputs name in
            let s = i "s" in
            let r = i "r" in
            let q = o "q" in
            let qb = o "qb" in
            let reader =
                Process.mk "sr-latch" [q;qb] (fun e t d ->
                    printf "t=%i,%i q=%s qb=%s" 
                        t d
                        (B.to_string (e.value q))
                        (B.to_string (e.value qb));
                    []
                )
                (* example of somewhat-equivalent VHDL
                   process (q,qb) is
                       variable t : time := simulator'time; -- current simulation time
                       variable d : time := simulator'delta; -- current simulation delta
                       variable l : line;
                   begin
                       write(t); write(d);
                       write(q); write(qb); 
                       writeline(output,l);
                   end process;
                *)
            in
            let writer = 
                Process.mk "writer" [] (fun e t d ->
                    [
                        e.event 10 (B.consti 1 1) s; 
                        e.event 10 (B.consti 1 0) r; 
                        e.event 20 (B.consti 1 1) s; 
                        e.event 20 (B.consti 1 1) r; 
                        e.event 30 (B.consti 1 0) s; 
                        e.event 30 (B.consti 1 1) r; 
                        e.event 40 (B.consti 1 1) s; 
                        e.event 40 (B.consti 1 1) r; 
                        e.event 50 (B.consti 1 0) s; 
                        e.event 50 (B.consti 1 0) r; 
                    ]
                )
            in
            let sim = Simulator.make (writer::reader::processes) in
            let sim = Simulator.init sim in
            let sim = Simulator.run sim 60 in
            ignore (sim)
        with 
        | Simulator.SimOver -> printf "sim over.\n"
        | Simulator.SimError(s) -> printf "sim error: %s\n" s


    (* master-slave flip-flop *)
    let test_2 () = 
        try
            let d = input "d" 1 in
            let c = input "c" 1 in
            let sr_latch s r = 
                let q,qb = wire 1, wire 1 in
                let q' = ~: (r &: qb) in
                let qb' = ~: (s &: q) in
                q <== q';
                qb <== qb';
                q,qb
            in
            let msff c d = 
                let stage c d = 
                    let q,_ = sr_latch (d &: c) ((~: d) &: c) in
                    q
                in
                stage (~: c) (stage c d)
            in
            let q = msff c d in

            let circuit = Circuit.make "testing" [ output "q" q; ] in
            let processes,inputs,outputs = Simulator.Circuit.build circuit in
            let i name = Simulator.Circuit.find_signal inputs name in
            (*let o name = Simulator.Circuit.find_signal outputs name in*)
            let c = i "c" in
            let d = i "d" in
            (*let q = o "q" in*)
            let clk = SignalDatabase.mk B.gnd "master_clock" in

            (* delay the clock by 1 tick to see if it makes any difference - 
             * it doesn't, but still matches the vhdl simulation.  
             * I wonder if this flip-flop style requires real delays, not just
             * delta delays *)
            let clock = 
                Process.mk "mclock" [clk] (fun e t d ->
                    [e.event (t+5) (B.(~:) (e.value clk)) clk])
                (*
                   signal clk = 0;
                   process begin
                       wait for 5 ns;
                       clk <= ~clk;
                   end process;
                 * *)
            in
            let delay = 
                Process.mk "clock" [clk] (fun e t d ->
                    [e.event (t+1) (e.value clk) c])
            in
              (* 
                 process (clk)
                 begin
                     c <= clk after 1 ns;
                 end process;
               *)

            let writer = 
                Process.mk "writer" [] (fun e t _ ->
                    [
                        e.event 10 (B.consti 1 1) d; 
                        e.event 20 (B.consti 1 1) d; 
                        e.event 30 (B.consti 1 0) d; 
                        e.event 40 (B.consti 1 1) d; 
                        e.event 50 (B.consti 1 0) d; 
                    ]
                )
            in
            let sim = Simulator.make (clock::delay::writer::processes) in
            let sim = Simulator.init sim in
            let sim = Simulator.run sim 100 in
            ignore (sim)
        with 
        | Simulator.SimOver -> printf "sim over.\n"
        | Simulator.SimError(s) -> printf "sim error: %s\n" s

    (* simple register test *)
    let test_3 () = 
        try
            let d = input "d" 1 in
            let q = reg r_sync enable d in

            let circuit = Circuit.make "testing" [ output "q" q; ] in
            let processes,inputs,outputs = Simulator.Circuit.build circuit in
            let i name = Simulator.Circuit.find_signal inputs name in
            (*let o name = Simulator.Circuit.find_signal outputs name in*)
            let clock = i "clock" in
            (*let reset = i "reset" in*)
            (*let clear = i "clear" in*)
            let enable = i "enable" in
            let d = i "d" in
            (*let q = o "q" in*)

            let clock = 
                Process.mk "clock" [clock] (fun e t d ->
                    [e.event (t+5) (B.(~:) (e.value clock)) clock])
                (* 
                   signal clock = 0;
                   process (clock) begin
                       wait for 5 ns;
                       clock <= ~clock;
                   end process;
                 *)
            in
            (*let reset = 
                Process.mk "reset" [] (fun e t d ->
                    [
                        e.event 0 (B.vdd) reset;
                        e.event 10 (B.gnd) reset;
                    ]
                )
            in*)
            let enable = 
                Process.mk "enable" [] (fun e t d ->
                    [
                        e.event 10 (B.vdd) enable;
                    ]
                )
            in
            let writer = 
                Process.mk "writer" [] (fun e t _ ->
                    [
                        e.event 10 (B.consti 1 1) d; 
                        e.event 20 (B.consti 1 1) d; 
                        e.event 30 (B.consti 1 0) d; 
                        e.event 40 (B.consti 1 1) d; 
                        e.event 50 (B.consti 1 0) d; 
                    ]
                )
            in
            let sim = Simulator.make (clock:: (* reset::*) enable::writer::processes) in
            let sim = Simulator.init sim in
            let sim = Simulator.run sim 100 in
            ignore (sim)
        with 
        | Simulator.SimOver -> printf "sim over.\n"
        | Simulator.SimError(s) -> printf "\n\n*** sim error: %s\n\n" s

end

