(*******************************************************************)
(*      This is part of SAFA, it is distributed under the          *)
(*  terms of the GNU Lesser General Public License version 3       *)
(*           (see file LICENSE for more details)                   *)
(*                                                                 *)
(*  Copyright 2014: Damien Pous. (CNRS, LIP - ENS Lyon, UMR 5668)  *)
(*******************************************************************)

open Common
open Automata

let remove a =
  let n = a.SENFA.size in
  let e = Hashtbl.create (n*(n-1)) in
  let changed = ref false in
  let add i b j =
    if i<>j && b!=Bdd.bot then
      try let c = Hashtbl.find e (i,j) in 
	  let bc = Bdd.dsj b !c in
	  if !c != bc then (changed := true; c:=bc)
      with Not_found -> changed := true; Hashtbl.add e (i,j) (ref b)
  in
  List.iter (fun (i,b,j) -> add i b j) a.SENFA.e;
  while !changed do
    changed := false;
    Hashtbl.iter (fun (i,j) b ->
      Hashtbl.iter (fun (j',k) c ->
	if i<>k && j=j' then add i (Bdd.cnj !b !c) k
      ) e
    ) e
  done;
  let eps i j = 
    if i=j then Bdd.top else 
      try !(Hashtbl.find e (i,j))
      with Not_found -> Bdd.bot
  in
  let t = Hashtbl.create n in
  let add i a j =
    let aj = Span.single a (IntSet.singleton j) IntSet.empty in
    try let x = Hashtbl.find t i in
	Hashtbl.replace t i (Span.merge IntSet.union aj x)
    with Not_found -> Hashtbl.add t i aj
  in
  List.iter (fun (i,a,j) -> add i a j) a.SENFA.t;
  let m = Bdd.init Hashtbl.hash (=) in
  let z = (Span.empty (Bdd.constant m IntSet.empty)) in
  let t = Array.init n (fun i -> 
    Hashtbl.fold (fun j jk -> 
      Span.merge (Bdd.binary m IntSet.union) 
	(Span.map (Bdd.binary m (fun b x -> if b then x else IntSet.empty) (eps i j)) 
	   (Span.map (Bdd.constant m) jk))
    ) t z)
  in
  let o = Array.init n (fun i -> Set.fold (fun j -> Bdd.dsj (eps i j)) a.SENFA.o Bdd.bot)
  in SNFA.{ 
    m; 
    t=Array.get t; 
    o=Array.get o;
    o0=Bdd.bot;
    o2=Bdd.dsj;
    state_info=Format.pp_print_int
  }

