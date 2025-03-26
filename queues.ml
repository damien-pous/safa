(*******************************************************************)
(*      This is part of SAFA, it is distributed under the          *)
(*  terms of the GNU Lesser General Public License version 3       *)
(*           (see file LICENSE for more details)                   *)
(*                                                                 *)
(*  Copyright 2014: Damien Pous. (CNRS, LIP - ENS Lyon, UMR 5668)  *)
(*******************************************************************)

(* FIFO queues give breadth-first traversal *)
module BFS = struct
  type 'a t = 'a Queue.t 
  let empty = Queue.create
  let singleton x = let q = Queue.create() in Queue.push x q; q
  let push q x = Queue.push x q
  let pop q = try Some (Queue.pop q) with Queue.Empty -> None
  let fold = Queue.fold
end

(* LIFO queues give depth-first traversal *)
module DFS = struct
  type 'a t = 'a list ref
  let empty () = ref []
  let singleton x = ref [x]
  let push r x = r := x :: !r
  let pop r = match !r with
    | [] -> None
    | x::q -> r := q; Some x
  let fold f x r = List.fold_left f x !r
end

(* Random queues for random traversal *)
module RFS = struct
  include DFS
  let pop r = 
    let rec get acc = function
      | _,[] -> None
      | 0,x::q -> r := List.rev_append acc q; Some x
      | n,x::q -> get (x::acc) (n-1,q)
    in 
    if !r = [] then None 
    else get [] (Random.int (List.length !r),!r)
end
