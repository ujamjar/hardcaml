(* 
 * hardcaml - hardware design in OCaml
 *
 *   (c) 2014 MicroJamJar Ltd
 *
 * Author(s): andy.ray@ujamjar.com
 * Description: 
 *
 *)

open Signal.Types

type uset = UidSet.t
type umap = uset UidMap.t

type fan = 
    {
        i : umap;
        o : umap;
    }

val fanin : fan -> uid -> uset
val fanout : fan -> uid -> uset

val search1 : fan -> uset -> uset * uset
val searchr : fan -> uset -> uset * uset -> uset * uset
val search : fan * fan -> uset -> uset * uset

val mk_fans : Circuit.t -> fan * fan

val schedule1 : fan -> UidSet.t -> UidSet.t -> UidSet.t

val schedule : fan -> UidSet.t -> UidSet.t -> UidSet.t list

val compile : Circuit.t -> uset * uset

