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
        let inputs = I.(map (fun (n,_) -> Cs.in_port sim n) t) in
        let outputs = O.(map (fun (n,_) -> Cs.out_port sim n) t) in
        circuit, sim, inputs, outputs

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
        let inputs = I.(map (fun (n,_) -> Cs.in_port sim n) t) in
        let outputs = O.(map (fun (n,_) -> Cs.out_port sim n) t) in
        circuit, sim, inputs, outputs
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



