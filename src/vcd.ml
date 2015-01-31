(* 
 * hardcaml - hardware design in OCaml
 *
 *   (c) 2014 MicroJamJar Ltd
 *
 * Author(s): andy.ray@ujamjar.com
 * Description: 
 *
 *)

(** VCD (Verilog Change Dump) generation *)
module Make(S : Comb.S) = 
struct

    open S
    open Circuit

    open Cyclesim.Api

    type t = S.t

    let vcdmin = 33
    let vcdmax = 126 
    let vcdcycle = 10

    type trace = 
        {
            w : int;
            id : string;
            name : string;
            data : S.t ref;
            prev : string ref;
        }

    type cyclesim = t Cyclesim.Api.cyclesim

    let wrap os sim = 
        (*let name = "sim" in*)
        let osl s = os (s ^ "\n") in
        let si = string_of_int in
        let (^:^) a b = a ^ " " ^ b in

        (* id generator *)
        let gen_id = 
            let i = ref 2 in (* 0+1 are for clock and reset *)
            let range = vcdmax - vcdmin in
            let rec gen x = 
                let d = x / range in
                let m = x mod range in
                if d = 0 then [m] 
                else d :: gen (x-range) 
            in
            let code x = List.fold_left (fun a x -> 
                (String.make 1 (Char.chr (x + vcdmin))) ^ a) "" (gen x) in
            (fun () ->
                let x = !i in
                incr i;
                code x
            )
        in

        let write_var v d w =
            if w = 1 then 
                osl (d^v)
            else 
                osl ("b"^d^:^v)
        in

        (* list of signals to trace *)
        let trace signals = 
            let width s = String.length (to_bstr s) in
            let xs w = String.make w 'x' in
            List.map (fun (n,s) -> 
                {
                    w = width !s;
                    id = gen_id ();
                    name = n;
                    data = s;
                    prev = ref (xs (width !s));
                }
            ) signals
        in
        let trace_in = trace sim.sim_in_ports in
        let trace_out = trace sim.sim_out_ports in
        let trace_internal = trace sim.sim_internal_ports in

        (* filter out 'clock' and 'reset' *)
        let trace_in = List.filter (fun s -> s.name <> "clock" && s.name <> "reset") trace_in in

        (* write the VCD header *)
        let write_header() = 
            os "$date\n  ...\n$end\n";
            os "$version\n  HardCaml\n$end\n";
            os "$comment\n  Hardware design in ocaml\n$end\n";
            os "$timescale 1ns $end\n";
            os "$scope module inputs $end\n";
            os "$var wire 1 ! clock $end\n";
            os "$var wire 1 \" reset $end\n";
            let trv t = osl ("$var wire "^si t.w^:^t.id^:^t.name^:^"$end") in
            List.iter trv trace_in; 
            os "$upscope $end\n";
            os "$scope module outputs $end\n";
            List.iter trv trace_out; 
            os "$upscope $end\n";
            os "$scope module various $end\n";
            List.iter trv trace_internal; 
            os "$upscope $end\n";
            os "$enddefinitions $end\n";
            os "$dumpvars\n";
            os "x!\n";
            os "x\"\n";
            List.iter (fun t -> write_var t.id !(t.prev) t.w) trace_in;
            List.iter (fun t -> write_var t.id !(t.prev) t.w) trace_out;
            List.iter (fun t -> write_var t.id !(t.prev) t.w) trace_internal;
            os "$end\n";
        in
        let time = ref 0 in
        write_header();

        (* reset *)
        let write_reset () = 
          osl ("#"^si (!time));
          osl "0!";
          osl "1\"";
          List.iter (fun t -> write_var t.id (S.to_bstr !(t.data)) t.w; t.prev :=
              (S.to_bstr !(t.data))) trace_in;
          List.iter (fun t -> write_var t.id (S.to_bstr !(t.data)) t.w; t.prev :=
              (S.to_bstr !(t.data))) trace_out;
          List.iter (fun t -> write_var t.id (S.to_bstr !(t.data)) t.w; t.prev :=
              (S.to_bstr !(t.data))) trace_internal;
          time := !(time) + vcdcycle 
        in
        (* cycle *)
        let write_cycle () = 
          osl ("#"^si (!time));
          osl "1!";
          osl "0\"";
          List.iter (fun t -> 
              let data = S.to_bstr !(t.data) in
              if data <> !(t.prev) then
                  (write_var t.id data t.w; t.prev := data)
          ) trace_in;
          List.iter (fun t -> 
              let data = S.to_bstr !(t.data) in
              if data <> !(t.prev) then
                  (write_var t.id data t.w; t.prev := data)
          ) trace_out;
          List.iter (fun t -> 
              let data = S.to_bstr !(t.data) in
              if data <> !(t.prev) then
                  (write_var t.id data t.w; t.prev := data)
          ) trace_internal;
          osl ("#"^si (!(time) + (vcdcycle/2)));
          osl "0!";
          time := !(time) + vcdcycle 
        in
        { sim with
            sim_reset = (fun () -> reset sim; write_reset());
            sim_cycle_seq = (fun () -> cycle_seq sim; write_cycle());
        }

end

