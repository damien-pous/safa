(*******************************************************************)
(*      This is part of SAFA, it is distributed under the          *)
(*  terms of the GNU Lesser General Public License version 3       *)
(*           (see file LICENSE for more details)                   *)
(*                                                                 *)
(*  Copyright 2014: Damien Pous. (CNRS, LIP - ENS Lyon, UMR 5668)  *)
(*******************************************************************)

(** congruence closure algorithm, for the `up to congruence' variant
    of the algorithms *)

open Common
module Make(Q: QUEUE): sig
  val empty: unit -> int_set -> int_set -> ('a*int_set*int_set) Q.t -> bool
end
