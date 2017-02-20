open Delimcc

type ('a, 'b) t = Done | More of 'a * ('b -> ('a, 'b) t)
                                      
let gen f =
  (*
   * Note: the first value to yield gets thrown away as the generator
   * has not yet started.
   *)
  let start _ =
    let p = Delimcc.new_prompt () in
    Delimcc.push_prompt p begin fun () ->
      f (fun x -> Delimcc.shift0 p (fun k -> More (x, k))); Done
    end in
  let next = ref start in

  fun rv ->
    match !next rv with
      | More (x, k) -> next := k; Some x
      | Done        -> None

(* type of testbench generators. [let foo = `gen (fun yield -> ... )] *)
type gen = [ `gen of ('a list -> unit) -> unit ] as 'a

(* wait n cycles (default 1), then run the generator *)
let delay ?(n=1) (`gen f) = `gen begin fun yield ->
  for i=0 to n-1 do
    yield [];
  done;
  f yield
end

(* run generators in sequence *)
let seq : (gen list -> gen) = fun l -> `gen begin fun yield ->
  List.iter (fun (`gen f) -> f yield) l
end

(* run the given generator for n cycles.  
   
   The generator can 'yield' new generators which will start running
   in the current cycle.
   
   If n is not specified, run until there are no more generators *)
let run ?n ~sim (driver : gen) = 
  let rec runsim' (n,m) drivers = 
    match drivers, n with
    | _, Some(n) when n = !m -> ()
    | [], None -> ()
    | _ -> 
      if drivers = [] then ()
      else
        let rec f = function
          | [] -> HardCaml.Cyclesim.Api.cycle sim; incr m; []
          | h::t -> 
            match h () with
            | Some (x:gen list) -> h :: f (t @ (List.map (fun (`gen x) -> gen x) x)) 
            | None -> f t
        in
        runsim' (n,m) (f drivers)
  in
  runsim' (n, ref 0) [(function `gen f -> gen f) driver]

