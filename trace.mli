(*******************************************************************)
(*      This is part of SAFA, it is distributed under the          *)
(*  terms of the GNU Lesser General Public License version 3       *)
(*           (see file LICENSE for more details)                   *)
(*                                                                 *)
(*  Copyright 2014: Damien Pous. (CNRS, LIP - ENS Lyon, UMR 5668)  *)
(*******************************************************************)

open Common

val clear: unit -> unit

val node: int -> string -> unit
val node_l: int -> int -> unit
val node_r: int -> int -> unit
val leaf: int -> string -> string -> unit
val leaf_t: int -> string -> int -> unit
val line: int -> int -> unit
val ce: int -> int -> unit
val ok: int -> int -> unit
val skip: int -> int -> unit
val entry: int -> unit

val render: hk:bool -> quote:string -> string


val save: unit -> unit
val restore: unit -> unit
