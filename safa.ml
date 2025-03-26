(*******************************************************************)
(*      This is part of SAFA, it is distributed under the          *)
(*  terms of the GNU Lesser General Public License version 3       *)
(*           (see file LICENSE for more details)                   *)
(*                                                                 *)
(*  Copyright 2014: Damien Pous. (CNRS, LIP - ENS Lyon, UMR 5668)  *)
(*******************************************************************)

open Common
open Automata
open SDFA

let iterations = Stats.counter "iterations" 

module Make(Q: QUEUE) = struct
  let equiv ?(tracer=fun _ _ _ -> ()) unify a x y =
    let todo = Q.empty () in
    let rec loop () = 
      match Q.pop todo with
	| None -> None
	| Some (w,x,y) ->
	  Stats.incr iterations;
          if a.output_check x y then (
	    tracer `OK x y;
	    Span.iter2 (fun i -> 
	      unify (fun at x y -> 
		Q.push todo ((i,at)::w,x,y))) 
	      (a.t x) (a.t y);
  	    loop ()
	  ) else ( 
	    tracer `CE x y;
	    Some (x, y, w)
	  )
    in 
    unify (fun _ x y -> Q.push todo ([],x,y)) 
          (Bdd.constant a.m (a.norm x)) (Bdd.constant a.m (a.norm y));
    loop ()

  module Congruence = Congruence.Make(Q)
  let equiv_c ?(tracer=fun _ _ _ -> ()) unify a	x y =
    let todo = Q.empty () in
    let congruence = Congruence.empty() in
    let rec loop () = 
      match Q.pop todo with
	| None -> None
	| Some (w,x,y) ->
	  Stats.incr iterations;
          if a.output_check x y then 
	    if congruence x y todo then (
	      tracer `Skip x y; 
	      loop()
	    ) else ( 
	      tracer `OK x y; 
	      Span.iter2 (fun i -> 
		unify (fun at x y -> 
		  Q.push todo ((i,at)::w,x,y))) 
		(a.t x) (a.t y);
  	      loop ()
	    ) else (
	    tracer `CE x y;
	    Some (x, y, w)
	  )
    in 
    unify (fun _ x y -> Q.push todo ([],x,y))
          (Bdd.constant a.m (a.norm x)) (Bdd.constant a.m (a.norm y));
    loop ()

end


let constant_ac a cmp ok x =
  let cmp (_,x) (_,y) = cmp x y in
  let ac = Antichain.create cmp in 
  let rec loop () =
    match Antichain.get ac with
    | None -> None
    | Some(w,x) ->
       Stats.incr iterations;
       if ok (a.o x) then (
	 Span.iter (fun i -> 
	     Bdd.iter (fun at x ->
                 Antichain.insert ac ((i,at)::w,x)
           )) (a.t x);
         loop()
       ) else Some (x, w)
  in
  Antichain.insert ac ([],x); loop ()

