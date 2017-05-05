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

module Gen_cosim2(SIM : Cosim2.Simulator)(B : Comb.S)(I : S)(O : S) = struct

    module S = Cosim2.Make(SIM)(B)
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
    let make ?check_names db name logic =
        let circuit = C.make name logic in
        let name = Circuit.Hierarchy.add ?check_names db circuit in
        I.make name
end

(* Extended interfaces *)

module type Ex = sig

  type 'a t

  val t : (string * int) t
  val to_list : 'a t -> 'a list

  val mapname : (string -> 'a) -> 'a t
  val mapbits : (int -> 'a) -> 'a t

  val zip2 : 'a t -> 'b t -> ('a * 'b) t
  val zip3 : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
  val zip4 : 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t
  val zip5 : 'a t -> 'b t -> 'c t -> 'd t -> 'e t -> ('a * 'b * 'c * 'd * 'e) t
  val zip6 : 'a t -> 'b t -> 'c t -> 'd t -> 'e t -> 'f t -> ('a * 'b * 'c * 'd * 'e * 'f) t

  val map : 
    ('a -> 'b) -> 
    'a t -> 'b t
  val map2 : 
    ('a -> 'b -> 'c) ->
    'a t -> 'b t -> 'c t
  val map3 : 
    ('a -> 'b -> 'c -> 'd) ->
    'a t -> 'b t -> 'c t -> 'd t
  val map4 : 
    ('a -> 'b -> 'c -> 'd -> 'e) ->
    'a t -> 'b t -> 'c t -> 'd t -> 'e t
  val map5 : 
    ('a -> 'b -> 'c -> 'd -> 'e -> 'f) ->
    'a t -> 'b t -> 'c t -> 'd t -> 'e t -> 'f t
  val map6 : 
    ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g) ->
    'a t -> 'b t -> 'c t -> 'd t -> 'e t -> 'f t -> 'g t

  val iter : 
    ('a -> unit) -> 
    'a t -> unit
  val iter2 : 
    ('a -> 'b -> unit) ->
    'a t -> 'b t -> unit
  val iter3 : 
    ('a -> 'b -> 'c -> unit) ->
    'a t -> 'b t -> 'c t -> unit
  val iter4 : 
    ('a -> 'b -> 'c -> 'd -> unit) ->
    'a t -> 'b t -> 'c t -> 'd t -> unit
  val iter5 : 
    ('a -> 'b -> 'c -> 'd -> 'e -> unit) ->
    'a t -> 'b t -> 'c t -> 'd t -> 'e t -> unit
  val iter6 : 
    ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> unit) ->
    'a t -> 'b t -> 'c t -> 'd t -> 'e t -> 'f t -> unit

  val offsets : ?rev:bool -> unit -> int t

  module type S = sig

    type b
    type ifs 

    val wire : unit -> ifs
    val wiren : unit -> ifs
    val consti : int -> ifs
    val zero : ifs
    val one : ifs
    val ones : ifs

    val (&:) : ifs -> ifs -> ifs
    val (|:) : ifs -> ifs -> ifs
    val (^:) : ifs -> ifs -> ifs
    val (~:) : ifs -> ifs

    val ( +: ) : ifs -> ifs -> ifs
    val ( -: ) : ifs -> ifs -> ifs
    val ( *: ) : ifs -> ifs -> ifs
    val ( *+ ) : ifs -> ifs -> ifs

    val pack : ?rev:bool -> ifs -> b
    val unpack : ?rev:bool -> b -> ifs

    module L : sig
      type 'a l = 'a list t
      val empty : unit -> 'a l
      val rev : 'a l -> 'a l
      val map : ('a t -> 'b t) -> 'a l -> 'b l
      val cons : 'a t -> 'a l -> 'a l
      val hd : 'a l -> 'a t
      val tl : 'a l -> 'a l
      val of_list : 'a t list -> 'a l
      val to_list : 'a l -> 'a t list
    end

    val mux : b -> ifs list -> ifs
    val mux2 : b -> ifs -> ifs -> ifs
    val concat : ifs list -> ifs
    val select : int -> int -> ifs -> ifs
    val msb : ifs -> ifs
    val msbs : ifs -> ifs
    val lsb : ifs -> ifs
    val lsbs : ifs -> ifs
  
  end

  module Make(B : Comb.S) : S
    with type b = B.t
    and  type ifs = B.t t
  
end

module Ex(X : S) = struct

  type 'a t = 'a X.t

  let to_list = X.to_list
  let t = X.t

  let map = X.map
  let map2 = X.map2

  let mapname f = map (fun (n,_) -> f n) t
  let mapbits f = map (fun (_,b) -> f b) t

  let zip2 a b = map2 (fun a b -> a,b) a b
  let zip3 a b c = map2 (fun (a,b) c -> a,b,c) (zip2 a b) c
  let zip4 a b c d = map2 (fun (a,b) (c,d) -> a,b,c,d) (zip2 a b) (zip2 c d)
  let zip5 a b c d e = map2 (fun (a,b,c) (d,e) -> a,b,c,d,e) (zip3 a b c) (zip2 d e)
  let zip6 a b c d e f = map2 (fun (a,b,c) (d,e,f) -> a,b,c,d,e,f) (zip3 a b c) (zip3 d e f)

  let map3 fn a b c = map (fun (a,b,c) -> fn a b c) (zip3 a b c)
  let map4 fn a b c d = map (fun (a,b,c,d) -> fn a b c d) (zip4 a b c d)
  let map5 fn a b c d e = map (fun (a,b,c,d,e) -> fn a b c d e) (zip5 a b c d e)
  let map6 fn a b c d e f = map (fun (a,b,c,d,e,f) -> fn a b c d e f) (zip6 a b c d e f)

  let iter fn a = ignore @@ map fn a
  let iter2 fn a b = ignore @@ map2 fn a b
  let iter3 fn a b c = ignore @@ map3 fn a b c 
  let iter4 fn a b c d = ignore @@ map4 fn a b c d
  let iter5 fn a b c d e = ignore @@ map5 fn a b c d e
  let iter6 fn a b c d e f = ignore @@ map6 fn a b c d e f

  let offsets ?(rev=true) () = 
    let l = to_list t in
    let rec f l = function [] -> [] | (n, b) :: t -> (n, l) :: f (l+b) t in
    let s = f 0 (if rev then l else List.rev l) in
    mapname (fun n -> List.assoc n s)

  module type S = sig

    type b
    type ifs 

    val wire : unit -> ifs
    val wiren : unit -> ifs
    val consti : int -> ifs
    val zero : ifs
    val one : ifs
    val ones : ifs

    val (&:) : ifs -> ifs -> ifs
    val (|:) : ifs -> ifs -> ifs
    val (^:) : ifs -> ifs -> ifs
    val (~:) : ifs -> ifs

    val ( +: ) : ifs -> ifs -> ifs
    val ( -: ) : ifs -> ifs -> ifs
    val ( *: ) : ifs -> ifs -> ifs
    val ( *+ ) : ifs -> ifs -> ifs

    val pack : ?rev:bool -> ifs -> b
    val unpack : ?rev:bool -> b -> ifs

    module L : sig
      type 'a l = 'a list t
      val empty : unit -> 'a l
      val rev : 'a l -> 'a l
      val map : ('a t -> 'b t) -> 'a l -> 'b l
      val cons : 'a t -> 'a l -> 'a l
      val hd : 'a l -> 'a t
      val tl : 'a l -> 'a l
      val of_list : 'a t list -> 'a l
      val to_list : 'a l -> 'a t list
    end

    val mux : b -> ifs list -> ifs
    val mux2 : b -> ifs -> ifs -> ifs
    val concat : ifs list -> ifs
    val select : int -> int -> ifs -> ifs
    val msb : ifs -> ifs
    val msbs : ifs -> ifs
    val lsb : ifs -> ifs
    val lsbs : ifs -> ifs

  end

  module Make(B : Comb.S) = struct

    type b = B.t
    type ifs = b t

    let wiren () = map (fun (n,b) -> B.(wire b -- n)) t
    let wire () = mapbits B.wire 

    let consti i = mapbits (fun b -> B.consti b i) 
    let zero = consti 0
    let one = consti 1
    let ones = consti (-1)

    let (&:) = map2 B.(&:)
    let (|:) = map2 B.(|:)
    let (^:) = map2 B.(^:)
    let (~:) = map B.(~:)

    let ( +: ) = map2 B.( +: )
    let ( -: ) = map2 B.( -: )
    let ( *: ) = map2 B.( *: )
    let ( *+ ) = map2 B.( *+ )

    let pack ?(rev=true) d = 
      if rev then B.concat @@ List.rev @@ to_list d
      else B.concat @@ to_list d
    let unpack ?(rev=true) d = 
      let rec f l t = 
        match t with
        | [] -> []
        | (n,b)::t -> (n, (B.select d (b+l-1) l)) :: f (b+l) t
      in
      let s = f 0 (if rev then to_list t else List.rev (to_list t)) in
      mapname (fun n -> List.assoc n s)

    module L = struct

      type 'a l = 'a list t

      let empty () = X.map (fun _ -> []) X.t
      let rev l = X.map List.rev l
      let cons h t = X.map2 (fun h t -> h::t) h t
      let hd l = X.map List.hd l
      let tl l = X.map List.tl l

      let rec of_list = function
        | [] -> empty ()
        | h::t -> cons h (of_list t)

      let rec to_list l = 
        try 
          let h = hd l in
          let t = tl l in
          h :: to_list t
        with _ -> []

      let map f l =
        let rec g l = 
          try 
            let h = hd l in
            let t = tl l in
            f h :: g t
          with _ -> []
        in
        of_list (g l)

    end

    let mux s l = X.map (B.mux s) (L.of_list l)
    let mux2 s h l = mux s [l;h]
    let concat l = X.map B.concat (L.of_list l)
    let select h l d = X.map (fun d -> B.select d h l) d
    let msb = X.map B.msb 
    let msbs = X.map B.msbs
    let lsb = X.map B.lsb 
    let lsbs = X.map B.lsbs

  end

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

