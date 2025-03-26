(*******************************************************************)
(*      This is part of SAFA, it is distributed under the          *)
(*  terms of the GNU Lesser General Public License version 3       *)
(*           (see file LICENSE for more details)                   *)
(*                                                                 *)
(*  Copyright 2014: Damien Pous. (CNRS, LIP - ENS Lyon, UMR 5668)  *)
(*******************************************************************)

open Common

module R1: sig
  (* representing a set of rewriting rules to perform efficient congruence tests.
     this module provides only the "one-pass" rewriting process *)
  type t
  val empty: t
  val add: int_set -> int_set -> t -> t	(* adding a rewriting rule to the candidate *)
  val pass: t -> int_set -> t * int_set	(* a single parallel rewriting pass *)
end = struct
  (* candidates as binary trees, allowing to cut some branches during
     rewriting *)
  type t = L of int_set | N of (int_set*t*t)
  let empty = L IntSet.empty
  let rec xpass skipped t z = match t with
    | L x -> skipped, IntSet.union x z
    | N(x,tx,fx) -> 
      if IntSet.subseteq x z then 
	let skipped,z = xpass skipped tx z in 
	xpass skipped fx z
      else
	let skipped,z = xpass skipped fx z in 
	N(x,tx,skipped),z
  let pass = xpass empty
  let add x x' =
    let rec add' = function
      (* optimisable *)
      | L y -> L (IntSet.union y x') 
      | N(y,t,f) -> N(y,t,add' f)
    in
    let rec add = function
      | L y as t -> 
	if IntSet.subseteq x y then L (IntSet.union y x') 
	else N(x,L (IntSet.union y x'),t)
      | N(y,t,f) -> match IntSet.set_compare x y with
	  | `Eq -> N(y,add' t,f)
	  | `Lt -> N(x,N(y,add' t,L x'),f)
	  | `Gt -> N(y,add t,f)
	  | `N  -> N(y,t,add f)
    in add
end

module R: sig
  (* more functions on rules, derived from the one-pass rewriting process *)
  type t
  val empty: t
  val add: int_set -> int_set -> t -> t
  val norm: t -> int_set -> int_set
  val pnorm: t -> int_set -> int_set -> (t*int_set) option
  val norm': ('a -> int_set -> 'a*int_set) -> t -> 'a -> int_set -> int_set
  val pnorm': ('a -> int_set -> 'a*int_set) -> t -> 'a -> int_set -> int_set -> (t*int_set) option
end = struct
  include R1

  (* get the normal form of [x] *)
  let rec norm rules x =
    let rules,x' = pass rules x in
    if IntSet.equal x x' then x else norm rules x'

  (* get the normal form of [y], unless [x] is subsumed by this normal
     form. In the first case, also return the subset of rules that
     were not applied *)
  let pnorm rules x y = 
    let rec pnorm rules y =
      if IntSet.subseteq x y then None else
	let rules,y' = pass rules y in
	if IntSet.equal y y' then Some (rules,y') else pnorm rules y'
    in pnorm rules y

  (* get the normal form of [x] w.r.t a relation and a todo list *)
  let norm' f =
    let rec norm' rules todo x =
      let rules,x' = pass rules x in
      let todo,x'' = f todo x' in 
      if IntSet.equal x x'' then x else norm' rules todo x''
    in norm'

  (* get the normal form of [y] w.r.t a relation, unless [x] is
     subsumed by the normal form of [y] w.r.t a relation and a todo
     list. In the first case, the normal form is only w.r.t the
     relation, and we also return the subset of (relation) rules that
     were not applied. *)
  let pnorm' f rules todo x y =
    let rec pnorm' rules todo y =
      if IntSet.subseteq x y then true else
	let todo,y' = f todo y in 
	let rules,y'' = pass rules y' in
	if IntSet.equal y y'' then false else
	  pnorm' rules todo y''
    in
    match pnorm rules x y with
      | None -> None
      | Some(rules,y') as r -> if pnorm' rules todo y' then None else r
end

module Make(Q: QUEUE) = struct

  let step todo z = List.fold_left (fun (todo,z) (x,y as xy) ->
    if IntSet.subseteq x z then todo, IntSet.union y z else
      if IntSet.subseteq y z then todo, IntSet.union x z else
	xy::todo,z
  ) ([],z) todo

  let empty () = 
    let r = ref R.empty in
    fun x y todo -> 
      let r' = !r in 
      let todo = Q.fold (fun acc (_,x,y) -> (x,y)::acc) [] todo in
      match R.pnorm' step r' todo x y, R.pnorm' step r' todo y x with
	| None, None -> true
	| Some(ry,y'), None -> 
	  r := R.add y (R.norm ry (IntSet.union x y')) r'; false
	| None, Some(rx,x') -> 
	  r := R.add x (R.norm rx (IntSet.union x' y)) r'; false
	| Some(ry,y'), Some(_,x') -> 
	  let z = R.norm ry (IntSet.union x' y') in
	  r := R.add x z (R.add y z r'); false
end

