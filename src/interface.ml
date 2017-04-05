(* 
 * hardcaml - hardware design in OCaml
 *
 *   (c) 2014 MicroJamJar Ltd
 *
 * Author(s): andy.ray@ujamjar.com
 * Description: 
 *
 *)

module type S = sig
    type 'a t 
    val t : (string * int) t
    val map : ('a -> 'b) -> 'a t -> 'b t
    val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
    val to_list : 'a t -> 'a list
end

module type Empty = sig 
    type 'a t = None
    val t : (string * int) t
    val map : ('a -> 'b) -> 'a t -> 'b t
    val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
    val to_list : 'a t -> 'a list
end

module Empty = struct
    type 'a t = None
    let t = None
    let map f x = None
    let map2 f x0 x1 = None
    let to_list x = []
end

let combine s t = 
    let module S = (val s : S) in
    let module T = (val t : S) in
    let module C = struct
        type 'a t = ('a S.t * 'a T.t) 
        let t = (S.t,T.t )
        let map f (s,t) = ((S.map f s), (T.map f t))
        let map2 f (s0,t0) (s1,t1) = ((S.map2 f s0 s1), (T.map2 f t0 t1)) 
        let to_list (s,t) = List.concat [ S.to_list s; T.to_list t ]
    end in
    (module C : S)

let prefix p m = 
    let module M = (val m : S) in
    let module N = struct
        include M
        let t = map (fun (n,b) -> p^n,b) t
    end in
    (module N : S)

let postfix p m = 
    let module M = (val m : S) in
    let module N = struct
        include M
        let t = map (fun (n,b) -> n^p,b) t
    end in
    (module N : S)

module Gen(B : Comb.S)(I : S)(O : S) = struct

    module S = Cyclesim.Make(B)
    module Cs = Cyclesim.Api

    let make name logic = 
        let outputs = logic I.(map (fun (n,b) -> Signal.Comb.input n b) t) in 
        let circuit = Circuit.make name 
            (O.to_list (O.map2 (fun (n,_) s -> Signal.Comb.output n s) O.t outputs))
        in
        let sim = S.make 
            ~internal:(Some(fun s -> Signal.Types.names s <> []))
            circuit in
        let inputs = I.(map (fun (n,b) -> try Cs.in_port sim n with _ -> ref B.(zero b)) t) in
        let outputs = O.(map (fun (n,b) -> try Cs.out_port sim n with _ -> ref B.(zero b)) t) in
        let next = O.(map (fun (n,b) -> try Cs.out_port_next sim n with _ -> ref B.(zero b)) t) in
        circuit, sim, inputs, outputs, next

end

module Gen_cosim(B : Comb.S)(I : S)(O : S) = struct

    module S = Cosim.Make(B)
    module Cs = Cyclesim.Api

    let make name logic = 
        let outputs = logic I.(map (fun (n,b) -> Signal.Comb.input n b) t) in 
        let circuit = Circuit.make name 
            (O.to_list (O.map2 (fun (n,_) s -> Signal.Comb.output n s) O.t outputs))
        in
        let sim = S.make 
            ~dump_file:(name ^ ".vcd")
            circuit in
        let inputs = I.(map (fun (n,b) -> try Cs.in_port sim n with _ -> ref B.(zero b)) t) in
        let outputs = O.(map (fun (n,b) -> try Cs.out_port sim n with _ -> ref B.(zero b)) t) in
        let next = O.(map (fun (n,b) -> try Cs.out_port_next sim n with _ -> ref B.(zero b)) t) in
        circuit, sim, inputs, outputs, next

end

module Gen_cosim2(B : Comb.S)(I : S)(O : S) = struct

    module S = Cosim2.Make(B)
    module Cs = Cyclesim.Api

    let make name logic = 
        let outputs = logic I.(map (fun (n,b) -> Signal.Comb.input n b) t) in 
        let circuit = Circuit.make name 
            (O.to_list (O.map2 (fun (n,_) s -> Signal.Comb.output n s) O.t outputs))
        in
        let sim = S.make 
            ~dump_file:(name ^ ".vcd")
            circuit in
        let inputs = I.(map (fun (n,b) -> try Cs.in_port sim n with _ -> ref B.(zero b)) t) in
        let outputs = O.(map (fun (n,b) -> try Cs.out_port sim n with _ -> ref B.(zero b)) t) in
        let next = O.(map (fun (n,b) -> try Cs.out_port_next sim n with _ -> ref B.(zero b)) t) in
        circuit, sim, inputs, outputs, next

end


module Circ(I : S)(O : S) = struct
    let make name logic = 
        let outputs = logic I.(map (fun (n,b) -> Signal.Comb.input n b) t) in 
        let circuit = Circuit.make name 
            (O.to_list (O.map2 (fun (n,_) s -> Signal.Comb.output n s) O.t outputs))
        in
        circuit
end

module Sim(B : Comb.S)(I : S)(O : S) = struct
    module Circ = Circ(I)(O)
    module S = Cyclesim.Make(B)
    module Cs = Cyclesim.Api
    let make name logic = 
        let circuit = Circ.make name logic in
        let sim = S.make 
            ~internal:(Some(fun s -> Signal.Types.names s <> []))
            circuit in
        let inputs = I.(map (fun (n,b) -> try Cs.in_port sim n with _ -> ref B.(zero b)) t) in
        let outputs = O.(map (fun (n,b) -> try Cs.out_port sim n with _ -> ref B.(zero b)) t) in
        let next = O.(map (fun (n,b) -> try Cs.out_port_next sim n with _ -> ref B.(zero b)) t) in
        circuit, sim, inputs, outputs, next
end

module Inst(I : S)(O : S) = struct
    let make name i =
        let inst = Signal.Instantiation.inst name []
            I.(to_list (map2 (fun (n,_) s -> n,s) t i))
            O.(to_list t)
        in
        O.(map (fun (n,_) -> inst#o n) t)
end

module Hier(I : S)(O : S) = struct
    module C = Circ(I)(O)
    module I = Inst(I)(O)
    let make db name logic =
        let circuit = C.make name logic in
        let name = Circuit.Hierarchy.add db circuit in
        I.make name
end

(* GADT based interface *)
open Signal.Types

type param = string * int
type ienv = param -> signal
type oenv = param -> signal -> signal

module Tuple : sig
  
  type _ t = private
    | S : param -> signal t
    | P : 'a t * 'b t -> ('a * 'b) t

  val (!) : param -> signal t

  val (@) : 'a t -> 'b t -> ('a * 'b) t

  val map : oenv -> 'a t -> 'a -> 'a

  val list : 'a t -> 'a -> signal list

  val params : 'a t -> param list

end = struct

  type _ t = 
    | S : param -> signal t
    | P : 'a t * 'b t -> ('a * 'b) t

  let (!) s = S s
  
  let (@) a b = P(a,b)

  let rec map : type a. oenv -> a t -> a -> a = fun env f t -> 
    match f,t with
    | S s, a -> env s a
    | P(a,b), (c,d) -> map env a c, map env b d

  let rec list : type a. a t -> a -> signal list = fun f t ->
    match f,t with
    | S _, s -> [s]
    | P(a,b), (c,d) -> List.concat [ list a c; list b d ]

  let rec params : type a. a t -> param list = function
    | S s -> [s]
    | P(a,b)  -> List.concat [ params a; params b ]

end

module Curried : sig

  type (_,_) t = private
    | R : 'a Tuple.t -> ('a,'a) t 
    | A : param * ('a,'b) t -> (signal -> 'a, 'b) t

  val ( @-> ) : param -> ('a, 'b) t -> (signal -> 'a, 'b) t 
  
  val returning : 'a Tuple.t -> ('a, 'a) t

  val get_return : ('a,'b) t -> 'b Tuple.t

end = struct

  type (_,_) t = 
    | R : 'a Tuple.t -> ('a,'a) t 
    | A : param * ('a,'b) t -> (signal -> 'a, 'b) t

  let (@->) a f = A(a, f)

  let returning p = R p

  let rec get_return : type a b. (a,b) t -> b Tuple.t = function
    | R p -> p
    | A(a,b) -> get_return b

end

module Fn : sig
  
  exception Parameter_validation of string * int * int

  val (!) : param -> signal Tuple.t

  val (@) : 'a Tuple.t -> 'b Tuple.t -> ('a * 'b) Tuple.t

  val ( @-> ) : param -> ('a, 'b) Curried.t -> (signal -> 'a, 'b) Curried.t 
  
  val returning : 'a Tuple.t -> ('a, 'a) Curried.t

  type ('a,'b) defn = ('a,'b) Curried.t * 'a

  val define : ('a,'b) Curried.t -> 'a -> ('a,'b) defn 

  val call : ('a,'b) defn -> 'a 

  type inst_env = 
    {
      input : ienv;
      output : oenv;
    }

  val inst : inst_env -> ('a,'b) defn -> 'b 

  val returns : ('a,'b) defn -> 'b -> signal list

  val ioenv : inst_env

  val circuit : string -> ('a,'b) defn -> Circuit.t

end = struct

  exception Parameter_validation of string * int * int

  let (!) = Tuple.(!)
  let (@) = Tuple.(@)
  let (@->) = Curried.(@->)
  let returning = Curried.returning

  type ('a,'b) defn = ('a,'b) Curried.t * 'a

  let define : type a b. (a,b) Curried.t -> a -> (a,b) defn = fun t f -> t,f

  (* this could be [let call (_, f) -> f].
     however, with this we can have parameter validation *)
  let rec call : type a b. (a,b) defn -> a = fun (t, f) ->
    let valid (n,b) s = if width s <> b then raise (Parameter_validation(n, b, width s)) in
    match t with
    | Curried.R p -> Tuple.map (fun p s -> valid p s; s) p f
    | Curried.A(a,b) -> (fun p -> valid a p; call (b,(f p)))

  type inst_env = 
    {
      input : ienv;
      output : oenv;
    }

  let rec inst : type a b. inst_env -> (a,b) defn -> b = fun env (t,f) ->
    match t with
    | Curried.R p -> Tuple.map env.output p f
    | Curried.A(a,b) -> inst env (b, (f (env.input a))) 

  let returns (t,f) r = Tuple.list (Curried.get_return t) r

  let ioenv = 
    {
      input = (fun (n,b) -> Signal.Comb.input n b);
      output = (fun (n,b) s -> Signal.Comb.output n s);
    }

  let circuit name defn = 
    Circuit.make name 
      (returns defn (inst ioenv defn))

end

