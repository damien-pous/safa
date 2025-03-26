(*******************************************************************)
(*      This is part of SAFA, it is distributed under the          *)
(*  terms of the GNU Lesser General Public License version 3       *)
(*           (see file LICENSE for more details)                   *)
(*                                                                 *)
(*  Copyright 2014: Damien Pous. (CNRS, LIP - ENS Lyon, UMR 5668)  *)
(*******************************************************************)

(** Common definitions and utilities *)

(** short-hand for user-defined formatter  *)
type 'a formatter = Format.formatter -> 'a -> unit
    
(** wrapper around [Hashtbl.find] *)
val get : ('a, 'b) Hashtbl.t -> 'a -> 'b option

(** signature for queues *)
module type QUEUE = sig
  type 'a t
  val empty : unit -> 'a t
  val singleton : 'a -> 'a t
  val push : 'a t -> 'a -> unit
  val pop : 'a t -> 'a option
  val fold : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
end

(** efficient implementation of sets of integers *)
module IntSet: Sets.T
type int_set = IntSet.t

(** hash-consed values, ans sets of such values *)
type 'a hval = 'a Hashcons.hash_consed
type 'a hset = 'a Hset.t

(** (not so efficient) implementation of polymorphic sets *)
module Set: sig
  type 'a t
  val empty : 'a t
  val singleton : 'a -> 'a t
  val union : 'a t -> 'a t -> 'a t
  val inter : 'a t -> 'a t -> 'a t
  val diff : 'a t -> 'a t -> 'a t
  val mem : 'a -> 'a t -> bool
  val is_empty : 'a t -> bool
  val subset : 'a t -> 'a t -> bool
  val rem : 'a -> 'a t -> 'a t
  val add : 'a -> 'a t -> 'a t
  val map : ('a -> 'b) -> 'a t -> 'b t
  val iter : ('a -> unit) -> 'a t -> unit
  val fold : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val filter : ('a -> bool) -> 'a t -> 'a t
  val exists : ('a -> bool) -> 'a t -> bool
  val for_all : ('a -> bool) -> 'a t -> bool
  val to_list : 'a t -> 'a list
  val of_list : 'a list -> 'a t
  val cardinal : 'a t -> int
  val print: ?sep:string -> 'a formatter -> 'a t formatter
  end
type 'a set = 'a Set.t

(** unterminated guarded strings (for counter-examples) *)
type ('v,'k) gstring_ = ('v * ('k * bool) list) list
(** guarded strings *)
type ('v,'k) gstring = ('k * bool) list * ('v,'k) gstring_

(** pretty-printing for guarded strings *)
val print_gstring: 'v formatter -> 'k formatter -> ('v,'k) gstring formatter

(** utility for formatting parenthesised expressions *)
val paren: int -> int -> ('b, 'c, 'd, 'e, 'f, 'g) format6 -> ('b, 'c, 'd, 'e, 'f, 'g) format6

(** spans: sparse polymorphic maps indexed by ['v] 
    these maps are used to represent explicit part of the
    transitions of guarded string automata *)
module Span: sig
  type ('v,'a) t
  (** [empty z] is the constant to [z] map *)
  val empty : 'a -> ('v,'a) t
  (** [single p x z] is map whose value is [x] on [p] and [z] everywhere else *)
  val single : 'v -> 'a -> 'a -> ('v,'a) t
  (** [merge f m n] applies [f] pointwise to [m] and [n], 
      assume that [m] and [z] share the same default value [z], 
      and that [f z z = z] *)
  val merge : ('a -> 'a -> 'a) -> ('v,'a) t -> ('v,'a) t -> ('v,'a) t
  (** pointwise application of a function  *)
  val map : ('a -> 'b) -> ('v,'a) t -> ('v,'b) t
  (** simple iterator *)
  val iter : ('v -> 'a ->  unit) -> ('v,'a) t  -> unit
  (** [iter2 f m n] applies [f] to all non-trivial pairs obtained by zipping [m] and [n] *)
  val iter2 : ('v -> 'a -> 'a -> unit) -> ('v,'a) t -> ('v,'a) t -> unit
  (** [get m p] returns the value of the map [m] on [p] *)
  val get : ('v,'a) t -> 'v -> 'a
end
type ('v,'s) span = ('v,'s) Span.t

(** simple timing function *)
val time: ('a -> 'b) -> 'a -> float * 'b

(** memoisation utilities *)
val memo_rec: ?n: int -> (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
val memo_rec1: (('a hval -> 'b) -> 'a hval -> 'b) -> 'a hval -> 'b
val memo_rec2: (('a hval -> 'b hval -> 'c) -> 'a hval -> 'b hval -> 'c) -> 'a hval -> 'b hval -> 'c

(** Utilities for lexing/parsing *)
val next_line: Lexing.lexbuf -> unit
val unexpected_char: Lexing.lexbuf -> 'a
val parse: ?msg: string -> ('a -> Lexing.lexbuf -> 'b) -> 'a -> Lexing.lexbuf -> 'b
