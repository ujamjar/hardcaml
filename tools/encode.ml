(* 
 * hardcaml - hardware design in OCaml
 *
 *   (c) 2014 MicroJamJar Ltd
 *
 * Author(s): andy.ray@ujamjar.com
 * Description: 
 *
 *)

(* base64 encode png icons used in javscript wave viewer *)

let buffer = Buffer.create 1024

let rec read () = 
    try begin
        Buffer.add_char buffer (Char.chr (input_byte stdin));
        read ()
    end with _ -> 
        Buffer.contents buffer

let data = read ()
let data = Base64.encode data

let _ = output_string stdout "\"data:image/png;base64,"
let _ = output_string stdout data
let _ = output_string stdout "\"\n"


