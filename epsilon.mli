(*******************************************************************)
(*      This is part of SAFA, it is distributed under the          *)
(*  terms of the GNU Lesser General Public License version 3       *)
(*           (see file LICENSE for more details)                   *)
(*                                                                 *)
(*  Copyright 2014: Damien Pous. (CNRS, LIP - ENS Lyon, UMR 5668)  *)
(*******************************************************************)

(** Symbolic epsilon removal *)

open Automata
open Common
open Bdd

(** convert a symbolic NFA with epsilon transitions into a symbolic NFA *)
val remove: 'v senfa -> (int,int_set,'v, key,formula) snfa

