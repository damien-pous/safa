(*******************************************************************)
(*      This is part of SAFA, it is distributed under the          *)
(*  terms of the GNU Lesser General Public License version 3       *)
(*           (see file LICENSE for more details)                   *)
(*                                                                 *)
(*  Copyright 2014: Damien Pous. (CNRS, LIP - ENS Lyon, UMR 5668)  *)
(*******************************************************************)

(** Symbolic determinisation *)

open Common
open Bdd
open Automata

(** simple, generic determinisation, using [Common.Set] sets *)
val generic: ('s, 's set, 'v, 'k, 'o) snfa -> ('s set, 'v, 'k, 'o) sdfa

(** intermediate one, working on [Common.hset] instead *)
val hashed: ('s hval, 's hset, 'v, 'k, 'o) snfa -> ('s hset, 'v, 'k, 'o) sdfa

(** more efficient determinisation, using [IntSet] instead *)
val optimised: (int, int_set, 'v, 'k, 'o) snfa -> (int_set, 'v, 'k, 'o) sdfa

(** idem, with output check assuming that the empty set is only equal to itself *)
val strict_optimised: (int, int_set, 'v, 'k, 'o) snfa -> (int_set, 'v, 'k, 'o) sdfa
