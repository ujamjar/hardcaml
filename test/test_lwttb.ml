(* test lwttb framework using cyclesim *)
module I = struct
  type 'a t = {
    clk : 'a;
    clr : 'a;
    ena : 'a;
    d : 'a[@bits 8];
  }[@@deriving hardcaml,show]
end

module O = struct
  type 'a t = {
    cnt : 'a[@bits 8];
    q : 'a[@bits 8];
  }[@@deriving hardcaml]
end

open HardCaml
open Signal.Comb
open I
open O

let f i = 
  let module Seq = (val (Signal.seq_sync ~clk:i.clk ~clr:i.clr) : Signal.Seq) in
  let cnt = Seq.reg_fb ~e:i.ena ~w:8 (fun d -> d +:. 1) in
  { cnt; q = i.d }

module B = HardCaml.Bits.Comb.IntbitsList
module Tb = HardCamlLWTTB.Lwtsim.Make(B)(I)(O)

open Lwt.Infix

let ppvec fmt v = Format.fprintf fmt "%s"
    (match v with None -> "?" | Some(v) -> string_of_int @@ B.to_int v)

(* simple counter test *)
let test_cntr sim = 
  let open Tb in
  let%lwt sim = set i.clr B.gnd sim in
  let%lwt sim = set i.ena B.vdd sim in
  let print sim = 
    let%lwt sim,o = cycle sim in
    let%lwt () = Lwt_io.printf "%i\n" (B.to_int o.cnt) in
    return sim
  in
  let%lwt sim = repeat 5 print sim in
  let%lwt sim = set i.clr B.vdd sim in
  let%lwt sim,_ = cycle sim in
  let%lwt sim = set i.clr B.gnd sim in
  let%lwt sim = repeat 5 print sim in
  return sim

let tb1 () = 
  let sim = Tb.make "tb1" f in
  Lwt_main.run @@ Tb.run sim test_cntr

(* test thread spawning *)
let test_spawner sim = 
  let open Tb in

  let%lwt sim = spawn ~log:(fun t -> Lwt_io.printf "====> 1\n")
    (fun sim ->
      let%lwt sim = setsome { inone with clr = Some(B.gnd); ena = Some(B.vdd) } sim in
      let%lwt sim = spawn 
        ~log:(fun t -> Lwt_io.printf "====> 4\n") 
        (fun sim -> repeat 11 return_cycle sim) sim 
      in
      repeat 7 return_cycle sim)
    sim
  in
  let%lwt sim = spawn ~log:(fun t -> Lwt_io.printf "====> 2\n")
    (fun sim -> repeat 3 return_cycle sim)
    sim
  in
  let%lwt sim = spawn ~log:(fun t -> Lwt_io.printf "====> 3\n")
    (fun sim -> repeat 9 return_cycle sim)
    sim
  in

  let%lwt sim = repeat 5
    (fun sim -> 
      cycle1 sim >>= fun (sim,o) -> 
      Lwt_io.printf "[%i]\n" (B.to_int o.cnt) >> 
      return sim) sim 
  in

  return sim

let tb2 () = 
  let sim = Tb.make "tb2" f in
  Lwt_main.run @@ Tb.run ~log:(fun t -> Lwt_io.printf "======> TOP\n") sim test_spawner

(* test inputs *)

let print_t id (sim : Tb.t) = 
  Lwt_io.printf "%s: %s\n" id (I.show ppvec sim.Tb.inputs)

let test_inputs sim = 
  let open Tb in
  let%lwt sim = 
    spawn
      ~log:(print_t "[1]")
      (fun sim -> 
         return sim >>=
         set i.ena B.vdd >>=
         set i.clr B.vdd >>=
         set i.d B.(consti 8 10) >>=
         return_cycle >>=
         spawn 
           ~log:(print_t "[2]")
           (fun sim -> 
              return sim >>=
              set i.ena B.vdd >>=
              set i.d B.(consti 8 20) >>=
              return_cycle) >>= 
         return_cycle >>=
         set i.d B.(consti 8 30) >>=
         return_cycle 
      ) sim
  in
  return sim >>=
  set i.ena B.gnd >>=
  set i.d B.(consti 8 100) >>=
  return_cycle >>= 
  set i.ena B.gnd >>=
  set i.d B.(consti 8 110) >>=
  return_cycle 

let tb3 () = 
  let sim = Tb.make "tb3" f in
  Lwt_main.run @@ Tb.run ~log:(print_t "top") sim test_inputs
  

