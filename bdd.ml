(*******************************************************************)
(*      This is part of SAFA, it is distributed under the          *)
(*  terms of the GNU Lesser General Public License version 3       *)
(*           (see file LICENSE for more details)                   *)
(*                                                                 *)
(*  Copyright 2014: Damien Pous. (CNRS, LIP - ENS Lyon, UMR 5668)  *)
(*******************************************************************)

open Common
open Hashcons

(** BDD nodes are hashconsed to ensure unicity, whence the following two-levels type *)
type ('a,'k) node = ('a,'k) descr hash_consed
and ('a,'k) descr = V of 'a | N of 'k * ('a,'k) node * ('a,'k) node
let head x = x.node

type ('a,'k) mem = {
  m: ('a,'k) descr Hashcons.t;
  h: 'a -> int;
  e: 'a -> 'a -> bool;
}

let init ?(size=1001) h e = {m=Hashcons.create size;h;e}

(** specific hashconsing function *)
let hashcons m =
  let hash = function V x -> m.h x | N(a,l,r) -> Hashtbl.hash (a,l.hkey,r.hkey) in
  let equal x y = match x,y with
    | V x, V y -> m.e x y
    | N(a,l,r), N(a',l',r') -> a=a' && l==l' && r==r'
    | _ -> false
  in hashcons hash equal m.m

let constant mem v = hashcons mem (V v)

let node mem b x y = if x==y then x else hashcons mem (N(b,x,y))

let tag x = x.tag

let hash x = x.hkey

let cmp x y = compare x.tag y.tag

(* utilities for memoised recursive function over hashconsed values *)

let cleaners = ref []
let on_clean f = cleaners := f :: !cleaners
let reset_caches () = List.iter (fun f -> f()) !cleaners

let memo_rec1 f =
  let t = ref Hmap.empty in
  on_clean (fun () -> t := Hmap.empty);
  let rec aux x =
    try Hmap.find x !t 
    with Not_found -> 
      let y = f aux x in 
      t := Hmap.add x y !t; y
  in aux

let memo_rec2 f =
  let t = ref Hmap.empty in
  on_clean (fun () -> t := Hmap.empty);
  let rec aux x y =
    try let tx = Hmap.find x !t in 
	try Hmap.find y !tx
	with Not_found ->
	  let z = f aux x y in
	  tx := Hmap.add y z !tx; z
    with Not_found -> 
      let z = f aux x y in 
      let tx = ref Hmap.empty in
      t := Hmap.add x tx !t; 
      tx := Hmap.add y z !tx;
      z
  in aux

let unary mem f =
  memo_rec1 (fun app x ->
    match head x with
      | V v -> constant mem (f v)
      | N(b,l,r) -> node mem b (app l) (app r)
  )

let binary mem f =
  memo_rec2 (fun app x y -> 
    match head x, head y with
      | V v, V w -> constant mem (f v w)
      | V _, N(b,l,r) -> node mem b (app x l) (app x r)
      | N(b,l,r), V _ -> node mem b (app l y) (app r y)
      | N(b,l,r), N(b',l',r') -> 
	match compare b b' with
	  | 0 -> node mem b  (app l l') (app r r')
	  | 1 -> node mem b' (app x l') (app x r')
	  | _ -> node mem b  (app l y ) (app r y )
  )

let times mem z = 
  memo_rec2 (fun times f x ->
    match head f, head x with
      | V true, _ -> x
      | V false, _ -> z
      | N(b,l,r), V _ -> node mem b (times l x) (times r x)
      | N(b,l,r), N(b',l',r') ->
	match compare b b' with
	  | 0 -> node mem b  (times l l') (times r r')
	  | 1 -> node mem b' (times f l') (times f r')
	  | _ -> node mem b  (times l x ) (times r x )
  )

let hide mem a f =
  memo_rec1 (fun hide x ->
    match head x with
      | V v -> x
      | N(b,l,r) -> if a=b then f l r else node mem b (hide l) (hide r)
  )

let partial_apply mem f =
  memo_rec1 (fun pa x ->
    match head x with
      | V v -> x
      | N(b,l,r) ->
        match f b with
	  | Some false -> pa l
	  | Some true -> pa r
	  | None -> node mem b (pa l) (pa r)
  )

type ('a,'k) unifier =
    (('k * bool) list -> 'a -> 'a -> unit) ->
    ('a,'k) node -> ('a,'k) node -> unit


module Hset2 = struct
  let empty() = 
    let t = ref Hmap.empty in
    on_clean (fun () -> t := Hmap.empty);
    t
  let mem t x y = 
    try Hset.mem y !(Hmap.find x !t)
    with Not_found -> false
  let add t x y =
    try let tx = Hmap.find x !t in 
	if Hset.mem y !tx then false
	else (tx := Hset.add y !tx; true)
    with Not_found -> 
      let tx = ref (Hset.singleton y) in
      t := Hmap.add x tx !t; 
      true
end

let unify_calls = Stats.counter "unify calls"
let unify_naive trace =
  let m = Hset2.empty() in
  fun f -> 
    let rec app at x y =
      let app b c x y = app ((b,c)::at) x y in
      if x!=y && Hset2.add m x y then (
	Stats.incr unify_calls;
	if trace then Trace.line x.tag y.tag;
	match head x, head y with
	  | V v, V w -> f at v w
	  | V _, N(b,l,r) -> app b false x l; app b true x r
	  | N(b,l,r), V _ -> app b false l y; app b true r y
	  | N(b,l,r), N(c,l',r') -> 
	    match compare b c with
	      | 0 -> app b false l l'; app b true r r'
	      | 1 -> app c false x l'; app c true x r'
	      | _ -> app b false l y ; app b true r y 
      )
    in fun x y -> if x!=y && not (Hset2.mem m x y) then app [] x y

let unify_dsf trace =
  let m = ref Hmap.empty in
  on_clean (fun () -> m := Hmap.empty);
  let link x y = m := Hmap.add x y !m in
  let get x = try Some (Hmap.find x !m) with Not_found -> None in
  let rec repr x = 
    match get x with
      | None -> x
      | Some y -> match get y with
	  | None -> y
	  | Some z -> link x z; repr z
  in 
  let link x y = 
    link x y;
    if trace then Trace.line x.tag y.tag
  in
  let rec unify at f x y =
    let unify b c x y = unify ((b,c)::at) f x y in
    let x = repr x in
    let y = repr y in
    if x!=y then (
      Stats.incr unify_calls;
      match head x, head y with
	| V a, V b -> link x y; f at a b
	| N(b,l,r), V _ -> link x y; unify b false l y; unify b true r y
	| V _, N(b,l,r) -> link y x; unify b false x l; unify b true x r
	| N(b,l,r), N(c,l',r') -> 
	  match compare b c with
	    | 0 -> link x y; unify b false l l'; unify b true r r'
	    | 1 -> link y x; unify c false x l'; unify c true x r'
	    | _ -> link x y; unify b false l y ; unify b true r y )
  in fun f x y -> 
    let x = repr x in
    let y = repr y in
    if x!=y then unify [] f x y

type key = char

type formula = (bool,key) node

let m = init Hashtbl.hash (=)
  
let bot = constant m false
let top = constant m true
let var b = node m b bot top
let rav b = node m b top bot

let neg = unary m not
let iff = binary m (=)
let xor = binary m (<>)

(* let dsj = binary (||) *)
let dsj = memo_rec2 (fun dsj x y ->
  if x==y then x else
  match head x, head y with
    | V true, _ | _, V false -> x
    | _, V true | V false, _ -> y
    | N(b,l,r), N(b',l',r') ->
      match compare b b' with
	| 0 -> node m b  (dsj l l') (dsj r r')
	| 1 -> node m b' (dsj x l') (dsj x r')
	| _ -> node m b  (dsj l y ) (dsj r y )
)

(* let cnj = binary (&&) *)
let cnj = memo_rec2 (fun cnj x y ->
  if x==y then x else
  match head x, head y with
    | V false, _ | _, V true -> x
    | _, V false | V true, _ -> y
    | N(b,l,r), N(b',l',r') ->
      match compare b b' with
	| 0 -> node m b  (cnj l l') (cnj r r')
	| 1 -> node m b' (cnj x l') (cnj x r')
	| _ -> node m b  (cnj l y ) (cnj r y )
)

let rec witness n =
  match head n with
    | V _ -> []
    | N(a,{node=V false},r) -> (a,true)::witness r
    | N(a,l,_) -> (a,false)::witness l

let rec print_formula i f x = match head x with
  | V false -> Format.fprintf f "0"
  | V true -> Format.fprintf f "1"
  | N(a,{node=V false},{node=V true}) -> Format.fprintf f "%c" a
  | N(a,{node=V true},{node=V false}) -> Format.fprintf f "!%c" a
  | N(a,{node=V true},r) -> Format.fprintf f (paren i 0 "!%c+%a") a (print_formula 0) r
  | N(a,l,{node=V true}) -> Format.fprintf f (paren i 0 "%c+%a") a (print_formula 0) l
  | N(a,{node=V false},r) -> Format.fprintf f (paren i 1 "%c%a") a (print_formula 1) r
  | N(a,l,{node=V false}) -> Format.fprintf f (paren i 1 "!%c%a") a (print_formula 1) l    
  | N(a,l,r) -> Format.fprintf f (paren i 0 "%c%a+!%c%a") a (print_formula 1) r a (print_formula 1) l

let rec print_formula i f x = match head x with
  | V false -> Format.fprintf f "0"
  | V true -> Format.fprintf f "1"
  | N(a,{node=V false},{node=V true}) -> Format.fprintf f "b%i" (int_of_char a)
  | N(a,{node=V true},{node=V false}) -> Format.fprintf f "!b%i" (int_of_char a)
  | N(a,{node=V true},r) -> Format.fprintf f (paren i 0 "!b%i+%a") (int_of_char a) (print_formula 0) r
  | N(a,l,{node=V true}) -> Format.fprintf f (paren i 0 "b%i+%a") (int_of_char a) (print_formula 0) l
  | N(a,{node=V false},r) -> Format.fprintf f (paren i 1 "b%i%a") (int_of_char a) (print_formula 1) r
  | N(a,l,{node=V false}) -> Format.fprintf f (paren i 1 "!b%i%a") (int_of_char a) (print_formula 1) l    
  | N(a,l,r) -> Format.fprintf f (paren i 0 "b%i%a+!b%i%a") (int_of_char a) (print_formula 1) r (int_of_char a) (print_formula 1) l

let walk g =
  let t = ref Hmap.empty in
  let rec walk x =
    try Hmap.find x !t
    with Not_found -> t := Hmap.add x () !t; g walk x
  in walk

let iter g =
  let t = ref Hmap.empty in
  let rec iter at x =
    let iter b c x = iter ((b,c)::at) x in
    try Hmap.find x !t
    with Not_found ->
      t := Hmap.add x () !t;
      match head x with
      | V v -> g at v
      | N(b,l,r) -> iter b false l; iter b true r
  in iter []

let unary m f = unary m f
let binary m f = binary m f

