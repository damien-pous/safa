(*******************************************************************)
(*      This is part of SAFA, it is distributed under the          *)
(*  terms of the GNU Lesser General Public License version 3       *)
(*           (see file LICENSE for more details)                   *)
(*                                                                 *)
(*  Copyright 2016: Damien Pous. (CNRS, LIP - ENS Lyon, UMR 5668)  *)
(*******************************************************************)

type 'a cell = E | C of 'a * 'a mlist
 and 'a mlist = 'a cell ref

type 'a t = {
    cmp: 'a -> 'a -> [`Ge|`Lt|`Un];
    seen: 'a mlist;
    mutable todo: 'a mlist;
  }

let create cmp =
  let seen = ref E in 
  {cmp; seen; todo=seen}
    
let get ac =
  match !(ac.todo) with
  | E -> None
  | C(x,q) -> ac.todo<-q; Some x

let insert ac x =
  let rec prune l x n =
    match !n with
    | E -> let c = C(x,ref E) in List.iter (fun m -> m:=c) l
    | C(y,m) as ym ->
       match ac.cmp x y with
       | `Ge -> assert false  (* cmp is not a preorder *)
       | `Lt -> prune (m::l) x m
       | `Un -> List.iter (fun m -> m:=ym) l; prune [m] x m
  in
  let rec ac_insert x n =
    match !n with
    | E -> n := C(x,ref E)
    | C(y,m) ->
       match ac.cmp x y with
       | `Ge -> ()
       | `Lt -> prune [n;m] x m
       | `Un -> ac_insert x m
  in
  ac_insert x ac.seen
