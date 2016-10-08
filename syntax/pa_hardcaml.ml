(* 
 * hardcaml - hardware design in OCaml
 *
 *   (c) 2014 MicroJamJar Ltd
 *
 * Author(s): andy.ray@ujamjar.com
 * Description: 
 *
 *)

open Camlp4.PreCast
open Syntax

type ident = { lid : string; uid : string }
let mk_ident s = { lid = Astring.String.Ascii.lowercase s; uid = s }

type signal = 
    | Signal of ident * Syntax.Ast.expr 
    | Module of ident * string list 
    | Array of ident * Syntax.Ast.expr * Syntax.Ast.expr 
    | List of ident * Syntax.Ast.expr * Syntax.Ast.expr 

let mpath _loc m x = 
    Ast.idAcc_of_list
        ((List.map (fun x -> <:ident< $uid:x$ >>) m) @ [ <:ident< $lid:x$ >> ])

let _module_expr _loc if_ports = 
    let l = List.map (function
        | Signal(p,b) -> <:ctyp< $lid:p.lid$ : 'a >>
        | Module(p,m) -> <:ctyp< $lid:p.lid$ : $id:mpath _loc m "t"$ 'a>>
        | Array(p,l,b) -> <:ctyp< $lid:p.lid$ : array 'a>>
        | List(p,l,b) -> <:ctyp< $lid:p.lid$ : list 'a>>
        )
        if_ports 
    in
    (* map f x *)
    let m = List.map (function 
        | Signal(p,b) -> 
            <:rec_binding< $lid:p.lid$ = f x.$lid:p.lid$ >>
        | Module(p,m) -> 
            <:rec_binding< $lid:p.lid$ = $id:mpath _loc m "map"$ f x.$lid:p.lid$ >>
        | Array(p,l,b) -> 
            <:rec_binding< $lid:p.lid$ = Array.map f x.$lid:p.lid$ >>
        | List(p,l,b) -> 
            <:rec_binding< $lid:p.lid$ = List.map f x.$lid:p.lid$ >>
        ) 
        if_ports
    in
    (* map2 f x y *)
    let m2 = List.map (function
        | Signal(p,b) -> 
            <:rec_binding< $lid:p.lid$ = f x0.$lid:p.lid$ x1.$lid:p.lid$ >>
        | Module(p,m) -> 
            <:rec_binding< $lid:p.lid$ = $id:mpath _loc m "map2"$ f x0.$lid:p.lid$ x1.$lid:p.lid$ >>
        | Array(p,_,_) -> 
            <:rec_binding< $lid:p.lid$ = 
                Array.init (Array.length x0.$lid:p.lid$) 
                    (fun _i -> f x0.$lid:p.lid$.(_i) x1.$lid:p.lid$.(_i)) >>
        | List(p,_,_) -> 
            <:rec_binding< $lid:p.lid$ = List.map2 f x0.$lid:p.lid$ x1.$lid:p.lid$ >>
        ) 
        if_ports
    in
    (* t *)
    let t = List.map (function
        | Signal(p,b) -> <:rec_binding< $lid:p.lid$ = ($str:p.uid$, $b$)>>
        | Module(p,m) -> <:rec_binding< $lid:p.lid$ = $id:mpath _loc m "t"$>>
        | Array(p,n,b) -> <:rec_binding< $lid:p.lid$ = 
            Array.init ($n$) (fun _i -> (($str:p.uid$^string_of_int _i), $b$))>>
        | List(p,n,b) -> <:rec_binding< $lid:p.lid$ = 
            Array.to_list (Array.init ($n$) 
                (fun _i -> (($str:p.uid$^string_of_int _i), $b$)))>>
        )
        if_ports
    in
    (* make list *)
    let tl = List.map (function 
        | Signal(p,b) -> <:expr< [ x.$lid:p.lid$ ] >>
        | Module(p,m) -> <:expr< $id:mpath _loc m "to_list"$ x.$lid:p.lid$ >>
        | Array(p,_,_) -> <:expr< Array.to_list x.$lid:p.lid$ >>
        | List(p,_,_) -> <:expr< x.$lid:p.lid$ >>
        )
        if_ports
    in
    let rec mk_tl l =
        match l with 
        | [] -> <:expr< [] >>
        | h :: t -> <:expr< [ $h$ :: $mk_tl t$ ] >>
    in
    let tl = mk_tl tl in
    if if_ports = [] then <:module_expr< HardCaml.Interface.Empty >>
    else
        <:module_expr< 
            struct
                type $lid:"t"$ 'a = { $Ast.tySem_of_list l$ }; 
                value $lid:"t"$ = { $Ast.rbSem_of_list t$ };
                value $lid:"map"$ f x = { $Ast.rbSem_of_list m$ };
                value $lid:"map2"$ f x0 x1 = { $Ast.rbSem_of_list m2$ };
                value $lid:"to_list"$ x = List.concat $tl$;
            end
        >>

let _module_type _loc if_ports = 
    let l = List.map 
        (function
            | Signal(p,b) -> <:ctyp< $lid:p.lid$ : 'a >>
            | Module(p,m) -> <:ctyp< $lid:p.lid$ : $id:mpath _loc m "t"$ 'a>>
            | Array(p,_,_) -> <:ctyp< $lid:p.lid$ : array 'a >>
            | List(p,_,_) -> <:ctyp< $lid:p.lid$ : list 'a >>
        ) 
        if_ports 
    in
    if if_ports = [] then <:module_type< HardCaml.Interface.Empty >>
    else
        <:module_type<
            sig
                type t 'a = { $Ast.tySem_of_list l$ };
                value t : t (string*int);
                value map : ('a -> 'b) -> t 'a -> t 'b;
                value map2 : ('a -> 'b -> 'c) -> t 'a -> t 'b -> t 'c;
                value to_list : t 'a -> list 'a;
            end
        >> 

let bit_expr _loc = function None -> <:expr<0>> | Some(x) -> x

let () = 
    EXTEND Gram
        GLOBAL: module_expr module_type expr;

        hc_ident: [ [ x = UIDENT -> x | x = LIDENT -> x ] ];

        hc_bits: [ [ "["; x = expr; "]" -> x ] ];

        hc_vector: [ [ "{|"; x = expr; "|}" -> 0,x 
                     | "{"; x = expr; "}" -> 1,x ] ];
            
        hc_signal:
        [ [ "("; x = LIDENT; ":"; m = LIST1 [x = UIDENT -> x] SEP "."; ")" -> 
                Module(mk_ident x, m)
          | x = hc_ident; a = OPT hc_vector; b = OPT hc_bits -> begin
                  match a with
                  | None -> Signal(mk_ident x, bit_expr _loc b)
                  | Some(0,a) -> Array(mk_ident x, a, bit_expr _loc b)
                  | Some(_,a) -> List(mk_ident x, a, bit_expr _loc b)
          end
        ] ];

        hc_vector_simple: [ [ "{|"; "|}" -> 0 
                            | "{"; "}" -> 1 ] ];

        hc_signal_simple:
        [ [ "("; x = LIDENT; ":"; m = LIST1 [x = UIDENT -> x] SEP "."; ")" -> 
                Module(mk_ident x, m)
          | x = hc_ident; a = OPT hc_vector_simple -> begin
                  match a with
                  | None -> Signal(mk_ident x, <:expr<0>>)
                  | Some(0) -> Array(mk_ident x, <:expr<0>>, <:expr<0>>)
                  | Some(_) -> List(mk_ident x, <:expr<0>>, <:expr<0>>)
          end
        ] ];

        module_expr: AFTER "top" 
        [ [ "interface"; if_ports = LIST0 [x = hc_signal -> x]; "end" ->
            _module_expr _loc if_ports
        ] ];

        module_type: AFTER "sig"
        [ [ "interface"; if_ports = LIST0 [x = hc_signal_simple -> x]; "end" ->
            _module_type _loc if_ports
        ] ];

        expr: LEVEL "."
        [ [ e0 = SELF; "."; "["; e1 = SELF; ":"; e2 = SELF; "]" ->
            <:expr<select $e0$ $e1$ $e2$>> 
        ] ];

    END

