(* 
 * hardcaml - hardware design in OCaml
 *
 *   (c) 2014 MicroJamJar Ltd
 *
 * Author(s): andy.ray@ujamjar.com
 * Description: 
 *
 *)

type 'a ports =
    {
        mutable ports : 'a array;
    }

let lookup f = 
    let x = ref None in
    fun b ->
      match !x with
        None -> let y = f b in x := Some y; y
      | Some x -> x

let resize zero ports size = 
    let len = Array.length ports.ports in
    if len < size then
    begin
        ports.ports <- Array.init size (fun i -> 
            if i < len then ports.ports.(i) else zero);
    end
    

let set_port zero ports elem data = 
    resize zero ports (elem+1);
    ports.ports.(elem) <- data

let get_port zero ports elem = 
    resize zero ports (elem+1);
    ports.ports.(elem)

let get_ports ports = ports.ports

let mk_ports () = { ports = [||] }

let check_ports zero name ports = 
    for i=0 to Array.length ports.ports-1 do 
        if ports.ports.(i) = zero then 
        begin
            failwith ("array port not set - " ^ name ^ ".(" ^ string_of_int i ^ ")")
        end
    done

let lookup_2 zero = 
    let arr = mk_ports () in
    fun f n ->
        let p' = get_port zero arr n in
        if p' = zero then 
            let p' = f n in
            set_port zero arr n p';
            p'
        else
            p'


