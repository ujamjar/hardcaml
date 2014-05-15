(* 
 * hardcaml - hardware design in OCaml
 *
 *   (c) 2014 MicroJamJar Ltd
 *
 * Author(s): andy.ray@ujamjar.com
 * Description: 
 *
 *)

(* VCD (Verilog Change Dump) generation *)
module Make(S : Comb.S) = 
struct

    module Vcd = Vcd.Make(S)

    type t = S.t
    type cyclesim = t Cyclesim.Api.cyclesim

    let wrap chan sim = 
        let o s = 
            output_string chan s;
            flush chan
        in
        Vcd.wrap o sim

    let gtkwave ?(args="") sim = 
        let fifoname = Filename.temp_file "sim" "fifo" in
        Printf.printf "Created tempfile %s\n" fifoname;
        Unix.unlink fifoname;
        Unix.mkfifo fifoname 0o600;
        Printf.printf "Made fifo, launching shmidcat and gtkwave\n";
        ignore (Unix.open_process_in ("shmidcat " ^ fifoname ^ " | gtkwave -v -I " ^ args));
        let fifo = open_out fifoname in
        at_exit (fun () -> Printf.printf "Destroying FIFO\n"; close_out fifo; Unix.unlink fifoname);
        wrap fifo sim

end


