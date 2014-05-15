(* 
 * hardcaml - hardware design in OCaml
 *
 *   (c) 2014 MicroJamJar Ltd
 *
 * Author(s): andy.ray@ujamjar.com
 * Description: 
 *
 *)

(** Write circuit as graph.  Currently works quite well with aisee3; www.aisee.com *)

val write_dot_rank : Pervasives.out_channel -> Circuit.t -> unit

(** write a GDL (graph description language) file of the given circuit *)
val write_gdl : 
    ?names:bool -> ?widths:bool -> 
    ?consts:bool -> ?clocks:bool ->
    Pervasives.out_channel -> Circuit.t -> unit

(** launch aisee3 to visualize the given circuit *)
val aisee3 : 
    ?args:string -> 
    ?names:bool -> ?widths:bool -> 
    ?consts:bool -> ?clocks:bool ->
    Circuit.t -> unit

