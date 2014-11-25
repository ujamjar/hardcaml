
open Signal.Comb
open Signal.Seq

type var = int
type inp = t * t (* enable * value *)
module VMap = Map.Make(struct type t = int let compare = compare end)
type env = 
  {
    freshId : var;
    writerInps : inp list VMap.t;
    outs : t VMap.t;
  }
type 'a recipe = Recipe of (t -> env -> (t * env * 'a)) 

let delay clr d = reg Signal.Types.({ r_sync with reg_clear_value=clr; }) enable d
let delayEn clr enable d = reg Signal.Types.({ r_sync with reg_clear_value=clr; }) enable d
let delayFb clr f = reg_fb Signal.Types.({ r_sync with reg_clear_value=clr }) enable (width clr) f
let setReset s r = delayFb gnd (fun q -> (s |: q) &: (~: r))

module Monad = struct

  let return a = 
    Recipe(fun start env -> (start, env, a))

  let bind (Recipe m) f = Recipe(fun start env ->
    let (fin0, env0, a) = m start env in
    let Recipe(f) = f a in
    let (fin1, env1, b) = f fin0 env0 in
    (fin1, env1, b))

  let (>>=) = bind

  let (>>) m f = bind m (fun _ -> f)

end 

open Monad

let skip = Recipe(fun start env -> (delay gnd start) -- "skip_fin", env, ())

let rec wait = function
  | 0 -> return ()
  | n -> skip >> wait (n-1)

let gen_par_fin comb_fin fin' fin = 
  let fin = setReset fin' fin in
  if comb_fin then fin' |: fin else fin

let par2 ?(comb_fin=true) (Recipe p) (Recipe q) = Recipe(fun start env ->
  let (fin0, env0, a) = p start env in
  let (fin1, env1, b) = q start env0 in
  let fin = wire 1 in
  let () = fin <== (gen_par_fin comb_fin fin0 fin &: gen_par_fin comb_fin fin1 fin) in
  (fin, env1, (a,b)))

let (|||) p q = par2 ~comb_fin:true p q

let par ?(comb_fin=true) r = Recipe(fun start env ->
  let finl, env, al = List.fold_left 
    (fun (finl,env,al) (Recipe r) ->
      let fin, env, a = r start env in
      (fin::finl,env,a::al)) ([],env,[]) r
  in
  let fin = wire 1 -- "par_fin" in
  let () = fin <== reduce (&:) (List.map (fun fin' -> gen_par_fin comb_fin fin' fin) finl) in
  (fin, env, List.rev al))

let cond c (Recipe p) (Recipe q) = Recipe(fun start env ->
  let (fin0, env0, _) = p (start &: c) env in
  let (fin1, env1, _) = p (start &: (~: c)) env in
  ((fin0 |: fin1) -- "cond_fin", env1, ()))

let iter c (Recipe p) = Recipe(fun start env -> 
  let ready = wire 1 -- "iter_ready" in
  let (fin, env', b) = p ((c &: ready) -- "iter_start") env in
  let () = ready <== (start |: fin) in
  (((~: c) &: ready) -- "iter_fin", env', b))

let forever p = iter vdd p
let waitWhile a = iter a skip
let waitUntil a = iter (~: a) skip

let follow start (Recipe r) = 
  let initialEnv = 
    {
      freshId = 0;
      (*readerInps = VMap.empty; (* we'll add a wire as we go *)*)
      writerInps = VMap.empty;
      outs = VMap.empty;
    }
  in
  let fin,env,a = r start initialEnv in
  (* connect writerInps to outs *)
  VMap.iter (fun v o -> 
    try 
      let inps = VMap.find v env.writerInps in 
      let enable = reduce (|:) (List.map fst inps) in
      let value = reduce (|:) (List.map (fun (e,v) -> mux2 e v (zero (width v))) inps) in
        (* repeat e (width v) &: v) inps) in (* or mux2 e v (zero n) *) *)
      o <== (delayEn (zero (width o)) enable value)
    with _ ->
      Printf.printf "unassign var; defaulting to zero";
      o <== (zero (width o)) (* unassign variable *)
  ) env.outs;
  fin, a

let createVar env a = 
  let v = env.freshId in
  v, { env with freshId = v+1; outs = VMap.add v a env.outs }

let ofList al = List.fold_left (fun m (k,v) -> VMap.add k v m) VMap.empty al

let addInps env al = 
  let merge _ a b = 
    match a,b with 
    | None,None -> None 
    | Some(a),None 
    | None,Some(a) -> Some(a) 
    | Some(a),Some(b) -> Some(a @ b)
  in
  { env with writerInps = VMap.merge merge (ofList al) env.writerInps }

let newVar ?name n = Recipe (fun start env ->
  let out = match name with None -> wire n | Some(x) -> (wire n) -- x in
  let v, env' = createVar env out in
  (start, env', v))

let readVar v = Recipe(fun start env -> (start, env, VMap.find v env.outs))

let assign al = Recipe(fun start env ->
  let al' = List.map (fun (a, b) -> a, [start,b]) al in
  (delay gnd start, addInps env al', ()))

let writeVar v a = assign [ v, a ]

let modifyVar f v = readVar v >>= fun a -> writeVar v (f a)

let rewriteVar f v w = readVar v >>= fun a -> writeVar w (f a)

module type Same = sig
  type 'a same 
  val smap : (var -> t) -> var same -> t same
  val szip : var same -> t same -> (var * t) list
  val read : var same -> t same recipe
  val rewrite : (t same -> t same) -> var same -> var same -> unit recipe
  val apply : (t same -> t same) -> var same -> unit recipe
  val set : var same -> t same -> unit recipe
  val ifte : (t same -> t) -> var same -> 'a recipe -> 'b recipe -> unit recipe
  val while_ : (t same -> t) -> var same -> 'a recipe -> 'a recipe
end

module Same(X : Interface.S) = struct
  type 'a same = 'a X.t
  let smap f t = X.map f t
  let szip x y = X.(to_list (map2 (fun a b -> a,b) x y))
  let read a = Recipe(fun start env -> (start, env, smap (fun a -> VMap.find a env.outs) a))
  let rewrite f a b = read a >>= fun x -> assign (szip b (f x))
  let apply f a  = rewrite f a a 
  let set a b = rewrite (fun _ -> b) a a
  let ifte f a p q = read a >>= fun b -> cond (f b) p q
  let while_ f a p = read a >>= fun b -> iter (f b) p
end    

module SVar = Same(struct
  type 'a t = 'a
  let t = "var", 0
  let map f a = f a
  let map2 f a b = f a b
  let to_list a = [a]
end)

module SList = Same(struct
  type 'a t = 'a list
  let t = []
  let map = List.map
  let map2 = List.map2
  let to_list a = a
end)

module SArray = Same(struct
  type 'a t = 'a array
  let t = [||]
  let map = Array.map
  let map2 f a b = Array.init (Array.length a) (fun i -> f a.(i) b.(i))
  let to_list = Array.to_list
end)

module STuple2 = Same(struct
  type 'a t = 'a * 'a
  let t = ("a",0), ("b", 0)
  let map f (a,b) = (f a, f b)
  let map2 f (a,b) (c, d) = (f a c, f b d)
  let to_list (a,b) = [ a; b ]
end)

module STuple3 = Same(struct
  type 'a t = 'a * 'a * 'a
  let t = ("a",0), ("b", 0), ("c", 0)
  let map f (a,b,c) = (f a, f b, f c)
  let map2 f' (a,b,c) (d,e,f) = (f' a d, f' b e, f' c f)
  let to_list (a,b,c) = [ a; b; c ]
end)

