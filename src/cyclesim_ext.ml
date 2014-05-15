(* 
 * hardcaml - hardware design in OCaml
 *
 *   (c) 2014 MicroJamJar Ltd
 *
 * Author(s): andy.ray@ujamjar.com
 * Description: 
 *
 *)

open Cyclesim

(** Wraps a simulator and allows it to be driven interactively from the command line *)
module Interactive(B : Comb.S) =
struct

    open Printf

    type command = 
        | Reset
        | Cycle of int 
        | Set of string * string
        | Print of string
        | Info
        | Quit

    let run chan sim = 

        let rec get_command() = 
            let line = 
                if chan <> stdin then input_line chan
                else read_line () 
            in
            let split str =
                let rec split l c chars = 
                    match chars with
                    | [] -> c :: l
                    | h :: t ->
                        if h = '#' then
                            split ("#" :: l) "" t
                        else if h = ' ' then
                            split (c :: l) "" t
                        else
                            split l (c ^ Char.escaped h) t
                in
                List.rev 
                    (List.filter ((<>) "") 
                                 (split [] "" 
                                        (Utils.list_of_string str)))
            in
            match split line with
            | [] -> 
                get_command ()
            | [ "!" ] -> 
                Reset
            | [ "!quit" ] ->
                Quit
            | [ "#" ] -> 
                Cycle 1
            | [ "?" ] ->
                Info
            | "#" :: n :: [] -> 
                (try Cycle (int_of_string n)
                with _ -> get_command ())
            | var :: [] -> 
                Print var
            | var :: value :: [] -> 
                Set (var, value)
            | _ -> 
                printf "Couldn't parse command";
                get_command ()
        in

        let info () = 
            List.iter (fun (n,v) ->
                printf "Input : [%4i] %s\n" (B.width !v) n
            ) (Api.in_ports sim);
            List.iter (fun (n,v) ->
                printf "Output: [%4i] %s\n" (B.width !v) n
            ) (Api.out_ports sim)
        in

        let get_interactive_input ()=
            let quit = ref false in
            let get_var name = 
                try Some(Api.in_port sim name)
                with _ -> 
                    try Some(Api.out_port sim name)
                    with _ -> None
            in
            let rec get_interactive_input () = 
                let command = try get_command() with _ -> Quit in
                (match command with
                | Reset -> 
                    (printf "Resetting simulator\n";
                    Api.reset sim)
                | Cycle n -> 
                    (printf "Cycling simulator %d times\n" n;
                    for i=0 to n-1 do Api.cycle sim done)
                | Info -> info()
                | Set(var, value) -> 
                    (match get_var var with 
                     | Some(x) -> 
                        (try x := B.const ((string_of_int (B.width !x)) ^ "'" ^ value)
                        with _ -> printf "Couldnt set value %s\n" value)
                     | None -> printf "Couldn't find %s\n" var)
                | Print(var) -> 
                    (match get_var var with 
                     | Some(x) -> 
                        (let b = B.to_bstr !x in
                        printf "Binary : %s\n" b;
                        printf "Decimal: %s\n" 
                            (Big_int.string_of_big_int
                                (Bits_ext.Utils_ext.big_int_of_bstr b)))
                     | None -> printf "Couldn't find %s\n" var)
                | Quit -> printf "Goodbye!\n"; quit := true);
                if not !quit then get_interactive_input ()
            in
            get_interactive_input ()
        in
        get_interactive_input ()

end


(* Binary IO dumping - Only supports the Nativeint Bigarray type *)
module BinaryIO =
struct

    type t = Bits_ext.Comb.BigarraybitsNativeint.t

    type binary_io =
        {
            read_inputs : bool;
            read_inputs_chan : Pervasives.in_channel;
            write_inputs : bool;
            write_inputs_chan : Pervasives.out_channel;
            compare_outputs : bool;
            compare_outputs_chan : Pervasives.in_channel;
            compare_error_fn : string -> t -> t -> unit;
            write_outputs : bool;
            write_outputs_chan : Pervasives.out_channel;
        }

    type mapped_data = 
        {
            data_offset : int;
            field_size : int;
            data : (string * (int * int)) list;
            a : Bits_ext.Utils_ext.bani;
        }

    let pbits = Utils.platform_bits 
    
    let setup_read chan = 
        let ti x = Nativeint.to_int x in
        let ba = Bigarray.Array1.map_file 
            (Unix.descr_of_in_channel chan) 
            Bigarray.nativeint Bigarray.c_layout false (-1)
        in
        (* read header *)
        let nelems = ti ba.{0} in
        let read_elem offset =
            let width = ti ba.{offset+0} in
            let name_len = ti ba.{offset+1} in
            let rec read_name str len off = 
                if len = 0 then str, off
                else
                    let c = Char.escaped (Char.chr (ti ba.{off})) in
                    read_name (str ^ c) (len-1) (off+1)
            in
            let name, off = read_name "" name_len (offset+2) in
            (name, width), off
        in
        let rec read_elements elems i offset = 
            if i = nelems then List.rev elems, offset
            else
                let elem, offset = read_elem offset in
                read_elements 
                    (elem :: elems)
                    (i+1) offset
        in
        let elements, offset = read_elements [] 0 2 in
        let elements, field_size = List.fold_left (fun (e,a) (n,w) ->
                let w = (w + pbits - 1) / pbits in
                (n, (w, a))::e, a + w
            ) ([],0) elements 
        in
        let elements = List.rev elements in
        List.iter (fun (n,(w,a)) -> Printf.printf "%s[%d] at %d\n" n w a) elements;
        {
            data_offset = offset;
            field_size = field_size;
            data = elements;
            a = ba;
        }

    let width = Bits_ext.Comb.BigarraybitsNativeint.width

    let read chan ports = 
        let elems = setup_read chan in
        let data = List.map (fun (n,d) -> 
                let w,o = List.assoc n elems.data in 
                if width !d <> w then failwith "Width mismatch";
                n,(d,w,o)
            ) ports
        in
        let offset = ref elems.data_offset in
        (fun () -> 
            List.iter (fun (n,(d,w,o)) ->
                let o = !offset + o in
                let d = fst !d in
                for i=0 to w-1 do
                    d.{i} <- elems.a.{o+i}
                done
            ) data;
            offset := !offset + elems.field_size;
        )

    let compare chan ports err_fn = 
        let elems = setup_read chan in
        let data = List.map (fun (n,d) -> 
                let w,o = List.assoc n elems.data in 
                if width !d <> w then failwith "Width mismatch";
                n,(d,w,o)
            ) ports
        in
        let offset = ref elems.data_offset in
        (fun () -> 
            List.iter (fun (n,(d,w,o)) ->
                let o = !offset + o in
                let error = ref false in
                let d = fst !d in
                for i=0 to w-1 do
                    if d.{i} <> elems.a.{o+i} then error := true
                done;
                if !error then
                begin
                    (* if there was an error, then copy the signal and 
                       call the error function *)
                    let q = Bigarray.Array1.create 
                        Bigarray.nativeint Bigarray.c_layout w 
                    in
                    for i=0 to w-1 do
                        q.{i} <- elems.a.{o+i}
                    done;
                    err_fn n (d,w) (q,w) 
                end
            ) data;
            offset := !offset + elems.field_size;
        )

    let setup_write chan ports =
        (* this is probably bleedin' slow *)
        let wr x = Marshal.to_channel chan x [ Marshal.No_sharing ] in
        let wri x = wr (Nativeint.of_int x) in
        let wrc x = wri (Char.code x) in
        wri (List.length ports);
        List.map (fun (n,d) ->
            wri (width !d);
            wri (String.length n);
            String.iter wrc n;
            d, (width !d + pbits - 1) / pbits
        ) ports
    
    let write chan ports = 
        let ports = setup_write chan ports in
        (fun () ->
            List.iter (fun (d,o) ->
                let d = fst !d in
                for i=0 to o-1 do
                    Marshal.to_channel chan d.{i} [ Marshal.No_sharing ]
                done
            ) ports
        )

    let seq f () = List.iter (fun f -> f()) f 

    open Api

    (* wrap a simulator and read/write binary dump files *)
    let wrap' cfg reset cycle in_ports out_ports =
        let f0 = if cfg.read_inputs then [ read cfg.read_inputs_chan in_ports ] else [] in
        let f1 = if cfg.write_inputs then [ write cfg.write_inputs_chan in_ports ] else [] in
        let f2 = 
            if cfg.compare_outputs then 
                [ compare cfg.compare_outputs_chan out_ports cfg.compare_error_fn ] 
            else [] 
        in
        let f3 = if cfg.write_outputs then [ write cfg.write_outputs_chan out_ports ] else [] in
        seq (List.concat [ f0; f1; [reset]; f2; f3 ]),
        seq (List.concat [ f0; f1; [cycle]; f2; f3 ])

    let errfn name _ _ = failwith ("Mismatch: " ^ name)

    type cyclesim = t Api.cyclesim

    let wrap ?(rdin=None) ?(wrin=None) ?(cmpout=None) ?(cmpfn=errfn) ?(wrout=None) sim = 
        let ui i = match i with Some(x) -> x | _ -> stdin in
        let uo o = match o with Some(x) -> x | _ -> stdout in
        let reset, cycle =
            wrap' 
                {
                    read_inputs = rdin <> None;
                    read_inputs_chan = ui rdin;
                    write_inputs = wrin <> None;
                    write_inputs_chan = uo wrin;
                    compare_outputs = cmpout <> None;
                    compare_outputs_chan = ui cmpout;
                    compare_error_fn = cmpfn;
                    write_outputs = wrout <> None;
                    write_outputs_chan = uo wrout;
                }
                sim.sim_reset sim.sim_cycle sim.sim_in_ports sim.sim_out_ports
        in
        { sim with
            sim_reset = reset;
            sim_cycle = cycle;
        }

end

