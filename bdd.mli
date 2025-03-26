(*******************************************************************)
(*      This is part of SAFA, it is distributed under the          *)
(*  terms of the GNU Lesser General Public License version 3       *)
(*           (see file LICENSE for more details)                   *)
(*                                                                 *)
(*  Copyright 2014: Damien Pous. (CNRS, LIP - ENS Lyon, UMR 5668)  *)
(*******************************************************************)

(** Binary Decision Diagrams (BDDs) *)

(** * high-level functions *)

open Common

(** Multi-Terminal BDD nodes with leaves in ['a] and decision nodes labelled with ['k] (keys) *)
type ('a,'k) node = ('a,'k) descr Hashcons.hash_consed
and ('a,'k) descr = private V of 'a | N of 'k * ('a,'k) node * ('a,'k) node

(** memories for BDDs nodes (required to ensure unicity) *)
type ('a,'k) mem

(** empty memory, with given hash and equality functions for leaves *)
val init: ?size:int -> ('a -> int) -> ('a -> 'a -> bool) -> ('a,'k) mem

(** constant BDD (leaf) *)
val constant: ('a,'k) mem -> 'a -> ('a,'k) node

(** applying a unary function to a BDD *)
val unary: ('b,'k) mem -> ('a -> 'b) -> ('a,'k) node -> ('b,'k) node

(** applying a binary function to two BDDs (the standard "apply" function) *)
val binary: ('c,'k) mem -> ('a -> 'b -> 'c) -> ('a,'k) node -> ('b,'k) node -> ('c,'k) node

(** hide a given key, by using the given function to resolve choices *)
val hide: ('a,'k) mem -> 'k -> (('a,'k) node -> ('a,'k) node -> ('a,'k) node) -> ('a,'k) node -> ('a,'k) node

(** walking recursively through a BDD, ensuring that a given node is visited at most once *)
val walk: ((('a,'k) node -> unit) -> ('a,'k) node -> unit) -> ('a,'k) node -> unit

(** iter leaves (with justifying path from the root )*)
val iter: (('k * bool) list -> 'a -> unit) -> ('a,'k) node -> unit


(** partially apply a BDD, by using the given function to resolve nodes *)
val partial_apply: ('a,'k) mem -> ('k -> bool option) -> ('a,'k) node -> ('a,'k) node

										 
(** BDD unifying functions *)
type ('a,'k) unifier =
  (('k * bool) list -> 'a -> 'a -> unit) -> ('a,'k) node -> ('a,'k) node -> unit

(** naive unifier where a set of pairs of nodes is stored. 
    The Boolean argument indicates whether to trace operations or not *)
val unify_naive: bool -> ('a,'k) unifier

(** union-find based unifier where a forest of pointers is stored.
    The Boolean argument indicates whether to trace operations or not *)
val unify_dsf: bool -> ('a,'k) unifier

(** * Boolean formulas *)

(** type of variables *)
type key = char

(** symbolic Boolean formulas *)
type formula = (bool,key) node

(** the various connectives  *)
val neg: formula -> formula
val dsj: formula -> formula -> formula
val cnj: formula -> formula -> formula
val xor: formula -> formula -> formula
val iff: formula -> formula -> formula

(** the two constants *)
val bot: formula
val top: formula

(** literals: a single variable, or the negation of a variable *)
val var: key -> formula
val rav: key -> formula

(** witness assignation for a satisfiable formula *)
val witness: (bool,'k) node -> ('k*bool) list

(** [times m z f n] multiplies the BDD [n] by the formula [f],
    using [z] where [f] is false *)
val times: ('a,'k) mem -> ('a,'k) node -> (bool,'k) node -> ('a,'k) node -> ('a,'k) node

(** pretty-printing a symbolic formula (the first argument specifies
    the current level, for parentheses) *)
val print_formula: int -> formula formatter



(** * low-level functions *)
  
(** explicit description of a given node *)
val head: ('a,'k) node -> ('a,'k) descr
    
(** identifier of a BDD node (unique w.r.t. a given memory) *)
val tag: ('a,'k) node -> int

(** hash of a BDD node *)
val hash: ('a,'k) node -> int

(** low-level node creation function, variable ordering is not enforced *)
val node: ('a,'k) mem -> 'k -> ('a,'k) node -> ('a,'k) node -> ('a,'k) node

(** reset all memoisation caches *)
val reset_caches: unit -> unit
