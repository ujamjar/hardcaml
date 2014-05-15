(* 
 * hardcaml - hardware design in OCaml
 *
 *   (c) 2014 MicroJamJar Ltd
 *
 * Author(s): andy.ray@ujamjar.com
 * Description: 
 *
 *)

open Structural 

val reg : clock:signal -> en:signal -> signal -> signal
val reg_r : clock:signal -> reset:signal -> ?def:int -> en:signal -> signal -> signal
val reg_c : clock:signal -> clear:signal -> ?def:int -> en:signal -> signal -> signal
val reg_rc : clock:signal -> reset:signal -> clear:signal -> ?def:int -> en:signal -> 
    signal -> signal

val tristate_buffer : en:signal -> i:signal -> t:signal -> signal
