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

(* basic registers *)

let reg ~clock ~en d =
    let q = mk_wire (width d) in 
    let _ = inst (prefix^"reg")
        ~g:[ "b" ==> GInt(width d); ]
        ~i:[ "clock" ==> clock; "enable" ==> en; "d" ==> d ]
        ~o:[ "q" ==> q ]
    in
    q

let reg_r ~clock ~reset ?(def=0) ~en d =
    let q = mk_wire (width d) in 
    let _ = inst (prefix^"reg_r")
        ~g:[ "b" ==> GInt(width d); "default" ==> GInt(def) ]
        ~i:[ "clock" ==> clock; "reset" ==> reset; "enable" ==> en; "d" ==> d ]
        ~o:[ "q" ==> q ]
    in
    q

let reg_c ~clock ~clear ?(def=0) ~en d =
    let q = mk_wire (width d) in 
    let _ = inst (prefix^"reg_c")
        ~g:[ "b" ==> GInt(width d); "default" ==> GInt(def) ]
        ~i:[ "clock" ==> clock; "clear" ==> clear; "enable" ==> en; "d" ==> d ]
        ~o:[ "q" ==> q ]
    in
    q

let reg_rc ~clock ~reset ~clear ?(def=0) ~en d = 
    let q = mk_wire (width d) in 
    let _ = inst (prefix^"reg_rc")
        ~g:[ "b" ==> GInt(width d); "default" ==> GInt(def) ]
        ~i:[ "clock" ==> clock; "reset" ==> reset; "clear" ==> clear; 
             "enable" ==> en; "d" ==> d ]
        ~o:[ "q" ==> q ]
    in
    q

let tristate_buffer ~en ~i ~t =
    let o = mk_wire (width i) in
    let _ = inst (prefix^"tristate_buffer")
        ~g:[ "b" ==> GInt(width i) ]
        ~i:[ "i" ==> i ]
        ~o:[ "o" ==> o ]
        ~t:[ "t" ==> t ]
    in
    o
