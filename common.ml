(*******************************************************************)
(*      This is part of SAFA, it is distributed under the          *)
(*  terms of the GNU Lesser General Public License version 3       *)
(*           (see file LICENSE for more details)                   *)
(*                                                                 *)
(*  Copyright 2014: Damien Pous. (CNRS, LIP - ENS Lyon, UMR 5668)  *)
(*******************************************************************)

type 'a formatter = Format.formatter -> 'a -> unit

let get t x = try Some (Hashtbl.find t x) with Not_found -> None

module type QUEUE = sig
  type 'a t
  val empty: unit -> 'a t
  val singleton: 'a -> 'a t
  val push: 'a t -> 'a -> unit
  val pop: 'a t -> 'a option
  val fold : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
end

module IntSet = Sets.NarithInlined
type int_set = IntSet.t

type 'a hval = 'a Hashcons.hash_consed
type 'a hset = 'a Hset.t
module Set = struct
  type 'a t = 'a list
  let filter = List.filter
  let exists = List.exists
  let for_all = List.for_all
  let fold = List.fold_right
  let iter = List.iter
  let cardinal = List.length
  let empty = []
  let is_empty x = x = []
  let singleton x = [x]
  let rec union h k = 
    match h,k with
    | [],l | l,[] -> l
    | x::h', y::k' ->
       match compare x y with
       | 0 -> x::union h' k'
       | 1 -> y::union h k'
       | _ -> x::union h' k
  let rec inter h k = 
    match h,k with
    | [],_ | _,[] -> []
    | x::h', y::k' ->
       match compare x y with
       | 0 -> x::inter h' k'
       | 1 -> inter h k'
       | _ -> inter h' k
  let rec diff h k = 
    match h,k with
    | [],_ -> []
    | l,[] -> l
    | x::h', y::k' ->
       match compare x y with
       | 0 -> diff h' k'
       | 1 -> diff h k'
       | _ -> x::diff h' k
  let rec subset h k = 
    match h,k with
    | [],_ -> true
    | _,[] -> false
    | x::h', y::k' ->
       match compare x y with
       | 0 -> subset h' k'
       | 1 -> subset h k'
       | _ -> false
  let rec rem x = function
    | [] -> []
    | y::q as l -> match compare x y with
	| 1 -> y::rem x q
	| 0 -> q
	| _ -> l
  let rec mem x = function
    | [] -> false 
    | y::q -> match compare x y with
	| 1 -> mem x q
	| 0 -> true
	| _ -> false
  let add x = union [x]
  let map f = List.fold_left (fun x i -> add (f i) x) []
  let to_list h = h
  let of_list h = List.fold_right add h empty
  let print ?(sep=",") g f x = 
    Format.pp_print_list ~pp_sep:(fun f () -> Format.pp_print_string f sep) g f x
end
type 'a set = 'a Set.t

type ('v,'k) gstring_ = ('v * ('k*bool) list) list
type ('v,'k) gstring = ('k*bool) list * ('v,'k) gstring_
let print_gstring pp_v pp_k f (alpha,l) =
  let rec atom = function
    | [] -> ()
    | (a,true)::q -> atom q; Format.fprintf f "%a" pp_k a
    | (a,false)::q -> atom q; Format.fprintf f "!%a" pp_k a
  in
  let rec list = function
    | [] -> ()
    | (p,alpha)::q -> list q; atom alpha; Format.fprintf f "%a" pp_v p
  in if l=[] && alpha=[] then Format.fprintf f "1" else (list l; atom alpha)

let paren h k x = if h>k then "("^^x^^")" else x

module Span = struct
  type ('v,'s) t = ('v*'s) list * 's
  let empty z = [],z
  let single a x z = [a,x],z
  let merge f =
    let rec merge h k = match h,k with
      | [],l | l,[] -> l
      | (a,x as ax)::h', (b,y as ay)::k' -> 
	match compare a b with
	  | 0 -> (a, f x y)::merge h' k'
	  | 1 -> ax::merge h' k
	  | _ -> ay::merge h k'
    in fun (h,z) (k,z') -> assert(z==z'); merge h k,z
  let map f (h,z) = List.map (fun (a,x) -> (a, f x)) h, f z
  let iter f (h,z) = List.iter (fun (a,x) -> f a x) h
  let iter2 f (h,z) (k,z') =
    assert(z==z');
    let rec iter2 h k = match h,k with
      | [],[] -> ()
      | [],(a,y)::k -> f a z y; iter2 [] k
      | (a,x)::h,[] -> f a x z; iter2 h []
      | (a,x)::h', (b,y)::k' -> 
	match compare a b with
	  | 0 -> f a x y; iter2 h' k'
	  | 1 -> f a x z; iter2 h' k
	  | _ -> f b z y; iter2 h  k'
    in iter2 h k
  let get (h,z) a =
    let rec get = function
      | [] -> z
      | (b,v)::h -> match compare a b with
		 | 0 -> v
		 | 1 -> z
		 | _ -> get h
    in get h
end
type ('a,'s) span = ('a,'s) Span.t

let time f x =
  let t0 = Sys.time() in
  let y = f x in
  Sys.time()-.t0, y

let memo_rec ?(n=1003) f =
  let t = Hashtbl.create n in
  let rec aux x =
    try Hashtbl.find t x 
    with Not_found -> 
      let y = f aux x in 
      Hashtbl.add t x y; y
  in aux

let memo_rec1 f =
  let t = ref Hmap.empty in
  let rec aux x =
    try Hmap.find x !t 
    with Not_found -> 
      let y = f aux x in 
      t := Hmap.add x y !t; y
  in aux

let memo_rec2 f =
  let t = ref Hmap.empty in
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

exception UnexpectedChar of string

let next_line lexbuf = Lexing.(
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }
)

let unexpected_char lexbuf =
  raise (UnexpectedChar (Lexing.lexeme lexbuf))

let pp_lexing_pos f lexbuf = Lexing.(
  let pos = lexbuf.lex_curr_p in
  if pos.pos_lnum>1 then
    Format.fprintf f "line %d, character %d" (* pos.pos_fname *)
      pos.pos_lnum (pos.pos_cnum - pos.pos_bol)
  else
    Format.fprintf f "character %d" (* pos.pos_fname *)
      (pos.pos_cnum - pos.pos_bol))

let parse  ?(msg="") p t lexbuf =
  try p t lexbuf
  with
    | UnexpectedChar m ->
      failwith (Format.asprintf "%sunexpected symbol at %a: `%s'" msg pp_lexing_pos lexbuf m)
    | _ ->
      failwith (Format.asprintf "%ssyntax error near %a" msg pp_lexing_pos lexbuf)
      
