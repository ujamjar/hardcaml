(* 
 * hardcaml - hardware design in OCaml
 *
 *   (c) 2014 MicroJamJar Ltd
 *
 * Author(s): andy.ray@ujamjar.com
 * Description: 
 *
 *)

(* New simulator.  We need to fix the following issues;
 
 * the old simulator didn't really work properly in terms of reading
   outputs at the correct time.  In some (but not all) cases an output
   would not read correctly after a simulation step, even though the correct
   sequence of values would actually be generated.  Specifically combinatorial
   logic after a register update step didn't read correctly.

 * The use of the Comb API for internal calculation is very inefficient,
   especially regarding memory usage.  All this can be worked out before hand
   and should be calculated efficiently using integer arrays.

 * (possible) the fix for the first issue can be resolved by performing
   a conditional calculation of dependancies i.e. what updates based on 
   what inputs/register etc.  If we make this generic it might be possible
   to perform a multi-clock based simulation.  This definitely complicates
   the simulator and a simple fallback based mode will be wanted for sure.
   Even so it might be well worth it.  Note that this would introduce a proper
   notion of time so a new VCD writer would be required.

*)

(* The first thing to work out is a dependancy scheduler. 
 
   We want to give a number of inputs and calculate what gets updated.
   This is a recursive calculation so new dependancies are calculated and
   scheduled.

   We want to control where we stop - ie at registers and/or outputs.

*)

open Signal.Types
open Circuit

let (|>) a f = f a
let (>>) f g x = g (f x)
let (<<) f g x = f (g x)

type uset = UidSet.t
type umap = uset UidMap.t

type fan = 
    {
        i : umap;
        o : umap;
    }

let fanin fan uid = try UidMap.find uid fan.i with Not_found -> UidSet.empty
let fanout fan uid = try UidMap.find uid fan.o with Not_found -> UidSet.empty

let search1 fan inputs = 
    (* find fanout *)
    let fanout = UidSet.fold (fanout fan >> UidSet.union) inputs UidSet.empty in
    let reqs = UidSet.fold (fanin fan >> UidSet.union) fanout UidSet.empty in
    fanout, reqs 

let rec searchr fan inputs (fo,r) = 
    let fanout, reqs = search1 fan inputs in
    if fanout = UidSet.empty then (fo,r)
    else
        searchr fan fanout
            (UidSet.union fanout fo, UidSet.union reqs r)

let search (fan1,fanr) inputs = 
    (* 1st level *)
    let fanout, reqs = search1 fan1 inputs in
    (* other levels *)
    searchr fanr fanout (fanout,reqs)

let remove_empty m = 
    UidMap.map (UidSet.filter (fun u -> u <> (uid Signal_empty))) m

let mk_fans circ = 
    let f1 = { i = circ.circ_fanin; o = circ.circ_fanout } in
    let f uid set = 
        let signal = UidMap.find uid circ.circ_id_to_sig in
        if is_reg signal || is_mem signal then UidSet.empty
        else set
    in
    let fr = { f1 with o = UidMap.mapi f f1.o } in
    let filter f = { i = remove_empty f.i; o = remove_empty f.o } in
    filter f1, filter fr

let schedule1 fan ready remaining = 
    let ready' = 
        UidSet.fold 
            (fun uid ready' ->
                let reqs = fanin fan uid in
                if UidSet.subset reqs ready then UidSet.add uid ready'
                else ready') remaining UidSet.empty
    in
    if ready' = UidSet.empty then failwith "cannot schedule any signals"
    else ready'

let rec schedule fan ready remaining = 
    if remaining = UidSet.empty then []
    else
        let ready' = schedule1 fan ready remaining in
        ready' :: (schedule fan (UidSet.union ready' ready) 
                                (UidSet.diff remaining ready')) 

let uidset_of_list l = List.fold_right (uid >> UidSet.add) l UidSet.empty

let compile circ = 
    let fans = mk_fans circ in
    let s_inputs = uidset_of_list circ.circ_inputs in
    search fans s_inputs


