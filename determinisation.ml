(*******************************************************************)
(*      This is part of SAFA, it is distributed under the          *)
(*  terms of the GNU Lesser General Public License version 3       *)
(*           (see file LICENSE for more details)                   *)
(*                                                                 *)
(*  Copyright 2014: Damien Pous. (CNRS, LIP - ENS Lyon, UMR 5668)  *)
(*******************************************************************)

open Common
open Automata

(* polymorphic, generic, determinisation *)
let generic a =
  let m = a.SNFA.m in
  let z = Span.empty (Bdd.constant m Set.empty) in
  let t x = 
    Set.fold (fun i acc -> Span.merge (Bdd.binary m Set.union) (a.SNFA.t i) acc) x z in
  let o x = Set.fold (fun i acc -> a.SNFA.o2 (a.SNFA.o i) acc) x a.SNFA.o0 in
  SDFA.{ m; t; o;
         norm=generic_norm;
	 output_check=generic_output_check o;         
	 state_info=Set.print a.SNFA.state_info }

(* hset-based determinisation *)
let hashed a =
  let m = a.SNFA.m in
  let t = a.SNFA.t in
  let o = a.SNFA.o in
  let z = Span.empty (Bdd.constant m Hset.empty) in
  let t x =
    Hset.fold (fun i acc -> Span.merge (Bdd.binary m Hset.union) (t i) acc) x z in
  let o x = Hset.fold (fun i acc -> a.SNFA.o2 (o i) acc) x a.SNFA.o0 in
  SDFA.{ m; t; o;
         norm=generic_norm;
	 output_check=generic_output_check o;
	 state_info=Hset.print a.SNFA.state_info }

(* specialised, more efficient determinisation *)
let optimised a =
  let m = a.SNFA.m in
  let t = a.SNFA.t in
  let o = a.SNFA.o in
  let z = Span.empty (Bdd.constant m IntSet.empty) in
  let t x =
    IntSet.fold (fun i acc -> Span.merge (Bdd.binary m IntSet.union) (t i) acc) x z in
  let o x = IntSet.fold (fun i acc -> a.SNFA.o2 (o i) acc) x a.SNFA.o0 in
  SDFA.{ m; t; o;
         norm=generic_norm;
	 output_check=generic_output_check o;
	 state_info=IntSet.print' a.SNFA.state_info }

(* same but with outpur check assuming that the empty set is only equal to itself *)
let strict_optimised a =
  let a = optimised a in 
  let output_check x y =
    IntSet.is_empty x = IntSet.is_empty y &&
      a.SDFA.o x == a.SDFA.o y
  in
  SDFA.{ a with output_check }
