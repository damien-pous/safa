(*******************************************************************)
(*      This is part of SAFA, it is distributed under the          *)
(*  terms of the GNU Lesser General Public License version 3       *)
(*           (see file LICENSE for more details)                   *)
(*                                                                 *)
(*  Copyright 2014: Damien Pous. (CNRS, LIP - ENS Lyon, UMR 5668)  *)
(*******************************************************************)

(** Sets of integers  *)

(** Abstract signature for finite sets of natural numbers *)
module type T = sig
  type t
  val empty: t
  val union: t -> t -> t
  val inter: t -> t -> t
  val singleton: int -> t
  val mem: int -> t -> bool
  val equal: t -> t -> bool
  val compare: t -> t -> int
  val full: int -> t
  val hash: t -> int
  val fold: (int -> 'a -> 'a) -> t -> 'a -> 'a
  val shift: int -> t -> t
  val size: t -> int
  val rem: int -> t -> t
  val add: int -> t -> t
  val is_empty: t -> bool
  val intersect: t -> t -> bool
  val diff: t -> t -> t
  val subseteq: t -> t -> bool
  val set_compare: t -> t -> [`Lt|`Eq|`Gt|`N]
  val map: (int -> int) -> t -> t
  val iter: (int -> unit) -> t -> unit
  val filter: (int -> bool) -> t -> t
  val cardinal: t -> int

  val forall: t -> (int -> bool) -> bool
  val exists: t -> (int -> bool) -> bool

  val to_list: t -> int list
  val of_list: int list -> t

  val print: Format.formatter -> t -> unit
  val print': (Format.formatter -> int -> unit) -> Format.formatter -> t -> unit

  (* [random n p] returns a set whose elements are stricly 
     lesser than [n], and appear with a probability [p]  *)
  val random: int -> float -> t

  module Map: Hashtbl.S with type key = t
end

(** sets as ordered lists *)
module OList: T

(** sets as integers (so that elements have to be really small) *)
module Small: T

(* sets as large integers *)
(* module Zarith: T *)
(* module ZarithInlined: T *)

(** sets as large integers, without Z *)
module NarithInlined: T

(** sets as binary balanced trees (i.e., OCaml stdlib) *)
module AVL: T

(** duplicator, to check the consistency of one implementation w.r.t. another one  *)
module Dup(M1: T)(M2: T): T
