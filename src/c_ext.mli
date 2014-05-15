(* 
 * hardcaml - hardware design in OCaml
 *
 *   (c) 2014 MicroJamJar Ltd
 *
 * Author(s): andy.ray@ujamjar.com
 * Description: 
 *
 *)

val csim : 
    (unit ->
        Bits_ext.Comb.BigarraybitsInt32.t Cyclesim.Api.cyclesim) 
        option ref

val compile : string -> unit

val write_c : string -> Circuit.t -> unit

val write_ml : string -> unit

val write_mli : string -> unit

val load : string -> 
    Bits_ext.Comb.BigarraybitsInt32.t 
        Cyclesim.Api.cyclesim

val make : string -> Circuit.t -> 
    Bits_ext.Comb.BigarraybitsInt32.t 
        Cyclesim.Api.cyclesim

val get_csim : unit ->
        Bits_ext.Comb.BigarraybitsInt32.t Cyclesim.Api.cyclesim
       

