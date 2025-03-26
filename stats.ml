(*******************************************************************)
(*      This is part of SAFA, it is distributed under the          *)
(*  terms of the GNU Lesser General Public License version 3       *)
(*           (see file LICENSE for more details)                   *)
(*                                                                 *)
(*  Copyright 2014: Damien Pous. (CNRS, LIP - ENS Lyon, UMR 5668)  *)
(*******************************************************************)

type t = int ref

let t = Hashtbl.create 10

let counter s = 
  try Hashtbl.find t s
  with Not_found -> let r = ref 0 in Hashtbl.add t s r; r
  
let count_calls s f = 
  let r = counter s in
  fun x -> incr r; f x

let incr ?(n=1) r = r := !r+n

let get s = try !(Hashtbl.find t s) with Not_found -> 0

let print f =
  let l = Hashtbl.fold (fun s i l -> (s,i)::l) t [] in
  let l = List.sort (fun x y -> compare (fst x) (fst y)) l in
  List.iter (fun (s,i) -> Format.fprintf f "// %s: %i\n" s !i) l

let reset () =
  Hashtbl.iter (fun _ r -> r:=0) t
