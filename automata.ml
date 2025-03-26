(*******************************************************************)
(*      This is part of SAFA, it is distributed under the          *)
(*  terms of the GNU Lesser General Public License version 3       *)
(*           (see file LICENSE for more details)                   *)
(*                                                                 *)
(*  Copyright 2014: Damien Pous. (CNRS, LIP - ENS Lyon, UMR 5668)  *)
(*******************************************************************)

open Common

module SDFA = struct
  type ('s, 'v, 'k, 'o) t = {
    m: ('s,'k) Bdd.mem;
    t: 's -> ('v, ('s,'k) Bdd.node) span;
    o: 's -> 'o;
    norm: 's -> 's;
    output_check: 's -> 's -> bool;
    state_info: 's formatter;
  }
  let trace ?(labels=true) ?exclude pp_v pp_k pp_o a =
    let si i x = if labels then Format.asprintf "%a" a.state_info x else string_of_int i in 
    let vi a = Format.asprintf "%a" pp_v a in
    let ki b = Format.asprintf "%a" pp_k b in
    let oi o = Format.asprintf "%a" pp_o o in
    let exclude = match exclude with Some y -> Some (Bdd.constant a.m y) | _ -> None in
    let keep x = match exclude with Some y when x==y -> false | _ -> true in
    let trace = Bdd.walk (fun trace x ->
      let i = Bdd.tag x in
      match Bdd.head x with
	| Bdd.V x ->
	  Trace.leaf i (si i x) (oi (a.o x));
	  Span.iter (fun a x' -> 
	    if keep x' then (
	      trace x';
	      Trace.leaf_t i (vi a) (Bdd.tag x'))
	  ) (a.t x);
	| Bdd.N(b,l,r) ->
    	  Trace.node i (ki b);
	  if keep l then (Trace.node_l i (Bdd.tag l); trace l);
	  if keep r then (Trace.node_r i (Bdd.tag r); trace r);
    ) in
    fun x -> 
      let x = Bdd.constant a.m (a.norm x) in 
      Trace.entry (Bdd.tag x); trace x
  let size a l =
    let s,n = ref 0,ref 0 in
    let count = Bdd.walk (fun count x ->
      match Bdd.head x with
	| Bdd.V x -> incr s; Span.iter (fun _ -> count) (a.t x);
	| Bdd.N(_,l,r) -> incr n; count l; count r
    ) in
    List.iter (fun x -> count (Bdd.constant a.m (a.norm x))) l;
    !s,!n
  let generic_norm x = x
  let generic_output_check o x y = o x == o y
end
type ('s,'v,'k,'o) sdfa = ('s,'v,'k,'o) SDFA.t

module SNFA = struct
  type ('s,'set,'v,'k,'o) t = {
    m: ('set,'k) Bdd.mem;
    t: 's -> ('v, ('set,'k) Bdd.node) span;
    o: 's -> 'o;
    o0: 'o;
    o2: 'o -> 'o -> 'o;
    state_info: 's formatter;
  }
  let int_map f x = Hset.fold (fun i -> IntSet.add (f i)) x IntSet.empty
  let reindex a =
    let m = Bdd.init Hashtbl.hash (=) in
    let z = Span.empty (Bdd.constant m IntSet.empty) in
    let back = ref Hmap.empty in
    let r = ref 0 in    
    let g = ref [||] in
    let rec f x = 
      try Hmap.find x !back 
      with Not_found ->
	let i = !r in
	let t = !g in
	let l = Array.length t in
	let o = a.o x in
	back := Hmap.add x i !back;
	if i = l then (
	  g := Array.make (2*l+1) (o,z,x);
	  Array.blit t 0 !g 0 i
	); 
	incr r;
	!g.(i) <- (o, Span.map (Bdd.unary m f') (a.t x), x);
	i
    and f' x = int_map f x in
    let fst (x,_,_) = x in
    let snd (_,x,_) = x in
    let thd (_,_,x) = x in
    let t i = snd !g.(i) in
    let o i = fst !g.(i) in
    let state_info f i = a.state_info f (thd !g.(i)) in
    { m; t; o; state_info; o0 = a.o0; o2 = a.o2 }, f'
  let size a x =
    let s,n = ref x,ref 0 in
    let count = Bdd.walk (fun count x ->
      match Bdd.head x with
	| Bdd.V x -> s:=IntSet.union x !s; IntSet.iter (fun x -> Span.iter (fun _ -> count) (a.t x)) x;
	| Bdd.N(_,l,r) -> incr n; count l; count r
    ) in
    count (Bdd.constant a.m x);
    IntSet.cardinal !s,!n
end
type ('s,'t,'v,'k,'o) snfa = ('s,'t,'v,'k,'o) SNFA.t

module SENFA = struct
  type 'v t = {
    e : (int * Bdd.formula * int) list;
    t : (int * 'v * int) list;
    o : int set;
    size: int;
  }
  end
type 'v senfa = 'v SENFA.t
