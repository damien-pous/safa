(*******************************************************************)
(*      This is part of SAFA, it is distributed under the          *)
(*  terms of the GNU Lesser General Public License version 3       *)
(*           (see file LICENSE for more details)                   *)
(*                                                                 *)
(*  Copyright 2014: Damien Pous. (CNRS, LIP - ENS Lyon, UMR 5668)  *)
(*******************************************************************)

(** Simple module to record statistics, by profiling *)

type t

(** create a new counter with the given name *)
val counter: string -> t

(** increment a counter *)
val incr: ?n:int -> t -> unit

(** getting the value of a counter *)
val get: string -> int

(** count the calls to the given function, under the given name *)
val count_calls: string -> ('a -> 'b) -> 'a -> 'b

(** print the status of all counters *)
val print: Format.formatter -> unit

(** reset all counters *)
val reset: unit -> unit
