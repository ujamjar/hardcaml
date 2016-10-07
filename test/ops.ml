(* test the combinatorial APIs *)

module Test(T : HardCaml.Comb.S) = struct

  module B = HardCaml.Api.B

  let brand bits = 1 + Random.int bits

  let assert_same label t b =
    let t, b = T.to_string t, B.to_string b in
    if t <> b then begin
      failwith (label ^ ": " ^ t ^ " <> " ^ b)
    end

  let op2 label opt opb n max_bits = 
    for i=0 to n-1 do
      let bits = brand max_bits in
      let a, b = B.srand bits, B.srand bits in
      let c, d = T.constibl a, T.constibl b in
      assert_same label (opt c d) (opb a b) 
    done

  let opm label opt opb n max_bits = 
    for i=0 to n-1 do
      let bitsa, bitsb = brand max_bits, brand max_bits in
      let a, b = B.srand bitsa, B.srand bitsb in
      let c, d = T.constibl a, T.constibl b in
      assert_same label (opt c d) (opb a b) 
    done

  let op1 label opt opb n max_bits = 
    for i=0 to n-1 do
      let bits = brand max_bits in
      let a = B.srand bits in
      let c = T.constibl a in
      assert_same label (opt c) (opb a) 
    done

  let sel n max_bits = 
    let bits = brand max_bits in
    let h,l = Random.int bits, Random.int bits in
    let h,l = max h l, min h l in
    let a = B.srand bits in
    let c = T.constibl a in
    assert_same "sel" (T.select c h l) (B.select a h l)

  let mux n max_bits = 
    for i=0 to n-1 do
      let sbits = brand 8 in (* limit size of mux *)
      let dbits = brand max_bits in
      let size =
        let n = 1 lsl (sbits-1) in
        max 2 (n + Random.int (n+1))
      in
      let s = B.srand sbits in
      let d = Array.to_list @@ Array.init size (fun _ -> B.srand dbits) in
      let s' = T.constibl s in
      let d' = List.map T.constibl d in
      assert_same "mux" (T.mux s' d') (B.mux s d)
    done

  let cat n max_bits = 
    for i=0 to n-1 do
      let cnt = 1 + Random.int 8 in
      let d = Array.to_list @@ Array.init cnt (fun _ -> B.srand @@ brand max_bits) in
      let d' = List.map T.constibl d in
      assert_same "cat" (T.concat d') (B.concat d)
    done

  let test n max_bits = 
    op2 "add" T.(+:) B.(+:) n max_bits;
    op2 "sub" T.(-:) B.(-:) n max_bits;
    op2 "mulu" T.( *: ) B.( *: ) n max_bits;
    op2 "muls" T.( *+ ) B.( *+ ) n max_bits;
    op2 "and" T.(&:) B.(&:) n max_bits;
    op2 "or" T.(|:) B.(|:) n max_bits;
    op2 "xor" T.(^:) B.(^:) n max_bits;
    op1 "not" T.(~:) B.(~:) n max_bits;
    op2 "eq" T.(==:) B.(==:) n max_bits;
    op2 "lt" T.(<:) B.(<:) n max_bits;
    sel n max_bits;
    mux n max_bits;
    cat n max_bits

end


