(*******************************************************************)
(*      This is part of SAFA, it is distributed under the          *)
(*  terms of the GNU Lesser General Public License version 3       *)
(*           (see file LICENSE for more details)                   *)
(*                                                                 *)
(*  Copyright 2014: Damien Pous. (CNRS, LIP - ENS Lyon, UMR 5668)  *)
(*******************************************************************)

open Common

(** Various implementations of queues *)

(**
- FIFO for breadth-first search ([BFS])
- LIFO for breadth-first search ([DFS])
- random for random search ([RFS])
*)

module BFS: QUEUE
module DFS: QUEUE
module RFS: QUEUE
