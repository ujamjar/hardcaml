open HardCaml

module type State = sig
  type state
end

module type S = sig

  type state
  type b
  type 'a i
  type 'a o

  type reset = unit -> state
  type cycle = state -> b i -> state * b o * b o

  type task_req  

  type t = private
    {
      vreq : task_req Lwt_mvar.t;
      vresp : (b o * b o) Lwt_mvar.t;
      children : t list;
      inputs : b option i;
      log : log option;
    }

  and log = t -> unit Lwt.t

  type task = t -> t Lwt.t

  val cycle1 : t -> (t * b o * b o) Lwt.t

  val cycle : ?n:int -> t -> (t * b o * b o) Lwt.t

  val spawn : ?log:log -> task -> t -> t Lwt.t

  val repeat : int -> task -> t -> t Lwt.t

  val delay : int -> task -> t -> t Lwt.t

  val return : 'a -> 'a Lwt.t

  val return_cycle : t -> t Lwt.t

  val i : bool i i

  val inone : b option i

  val set : bool i -> b -> t -> t Lwt.t

  val setsome : b option i -> t -> t Lwt.t

  val setall : b i -> t -> t Lwt.t

  val run : ?log:log -> (reset * cycle) -> task -> unit Lwt.t

end

module Make(State : State)(B : Comb.S)(I : Interface.S)(O : Interface.S) = struct
  include State
  open Lwt.Infix

  type b = B.t
  type 'a i = 'a I.t
  type 'a o = 'a O.t

  type reset = unit -> state
  type cycle = state -> b i -> state * b o * b o

  type task_req = 
    | Cycle of b option I.t 
    | Finish 

  type t = 
    {
      (* mailbox variables used to synchronise to the clock cycle *)
      vreq : task_req Lwt_mvar.t;
      vresp : (B.t O.t * B.t O.t) Lwt_mvar.t;
      (* child tasks *)
      children : t list;
      (* inputs *)
      inputs : B.t option I.t;
      (* cycle logging function *)
      log : log option;
    }


  and log = t -> unit Lwt.t

  type task = t -> t Lwt.t

  let merge i1 i2 = I.map2 
      (fun i1 i2 ->
        match i1, i2 with
        | _, Some(d) -> Some(d)
        | Some(d), _ -> Some(d)
        | _ -> None) i1 i2

  let inone = I.map (fun _ -> None) I.t

  let dolog t children inputs = 
    match t.log with
    | None -> Lwt.return ()
    | Some(f) -> f { t with children; inputs }

  let cycle1 t = 
    (* wait for active children to notify *)
    let%lwt children = Lwt_list.filter_map_p 
      (fun t -> 
        Lwt_mvar.take t.vreq >>= function Cycle i -> Lwt.return_some (t,i)
                                        | Finish -> Lwt.return_none) 
      t.children 
    in
    let children = List.map fst children and inputs = List.map snd children in
    let inputs = List.fold_left merge t.inputs inputs in
    let%lwt () = dolog t children inputs in
    (* notify parent *)
    let%lwt () = Lwt_mvar.put t.vreq (Cycle inputs) in
    (* wait for state from parent *)
    let%lwt o, n = Lwt_mvar.take t.vresp in
    (* broadcast to children *)
    let%lwt () = Lwt_list.iter_p (fun t -> Lwt_mvar.put t.vresp (o, n)) children in
    let t = { t with children; inputs=inone } in
    Lwt.return (t, o, n)

  let rec cycle ?(n=1) t = 
    if n < 1 then Lwt.fail_with "cycle must be for >= 1 cycle"
    else if n = 1 then
      cycle1 t
    else
      cycle1 t >>= fun (t,_,_) -> cycle ~n:(n-1) t

  let rec with_finish t = 
    if t.children = [] then 
      Lwt_mvar.put t.vreq Finish >> Lwt.return t
    else
      (* keep running while children are active *)
      let%lwt t,_,_ = cycle1 t in
      with_finish t

  let task' ?log () = 
    {
      vreq = Lwt_mvar.create_empty ();
      vresp = Lwt_mvar.create_empty ();
      children = [];
      inputs = inone;
      log;
    }

  let async task t = Lwt.async (fun () -> task t >>= with_finish)

  let spawn ?log k t = 
    (* generate communications variables *)
    let n = task' ?log () in
    (* run the thread *)
    let () = async k n in
    (* update children *)
    Lwt.return { t with children = n :: t.children }

  let rec repeat n f t = 
    if n <= 0 then Lwt.return t
    else
      let%lwt t = f t in
      repeat (n-1) f t

  let rec delay n f t = 
    if n <= 0 then f t
    else
      cycle t >>= fun (t,_,_) -> delay (n-1) f t

  let i = 
    I.map (fun (n,_) -> I.map (fun (m,_) -> n=m) I.t) I.t

  let set fld v t = 
    Lwt.return 
      { t with inputs = I.map2 (fun yn prv -> if yn then Some(v) else prv) fld t.inputs }

  let setsome v t = Lwt.return { t with inputs = merge t.inputs v }

  let setall v t = Lwt.return { t with inputs = I.map (fun x -> Some(x)) v }

  let return = Lwt.return

  let return_cycle sim = cycle1 sim >>= fun (sim,_,_) -> return sim

  let run ?log (reset, cycle) task =
 
    let t = task' ?log () in
    let () = async task t in

    let rec loop prev sim = 
      match%lwt Lwt_mvar.take t.vreq with
      | Cycle inputs -> begin
        (* apply inputs *)
        let inputs = 
          I.map2 (fun p d -> match d with
                             | None -> p
                             | Some(d) -> d) prev inputs
        in
        (* simulation cycle *)
        let sim, o, n = cycle sim inputs in
        (* send back outputs *)
        let%lwt () = Lwt_mvar.put t.vresp (o, n) in
        (* loop *)
        loop inputs sim
      end
      | Finish -> Lwt.return ()
    in

    loop 
      I.(map (fun (_,b) -> B.zero b) t) 
      (reset ())

end

