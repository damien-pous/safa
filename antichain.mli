(*******************************************************************)
(*      This is part of SAFA, it is distributed under the          *)
(*  terms of the GNU Lesser General Public License version 3       *)
(*           (see file LICENSE for more details)                   *)
(*                                                                 *)
(*  Copyright 2016: Damien Pous. (CNRS, LIP - ENS Lyon, UMR 5668)  *)
(*******************************************************************)

(** Antichains *)

(** type of (pointed antichains): antichains with a pointer in the
  middle separating what has been done from what remains to be done *)

type 'a t

(** create an empty antichain, with the given comparison function (should be a partial order) *)
val create: ('a -> 'a -> [`Ge|`Lt|`Un]) -> 'a t

(** push a value into the antichain:
  - does nothing is this value is greater or equal than an existing value
  - insert the value in the "remains to be done" zone otherwise, removing all 
    strictly smaller values from the antichain
 *)
val insert: 'a t -> 'a -> unit

(** select a value from the "remains to be done" zone and move it to the "done" zone  *)
val get: 'a t -> 'a option
