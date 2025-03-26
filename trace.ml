(*******************************************************************)
(*      This is part of SAFA, it is distributed under the          *)
(*  terms of the GNU Lesser General Public License version 3       *)
(*           (see file LICENSE for more details)                   *)
(*                                                                 *)
(*  Copyright 2014: Damien Pous. (CNRS, LIP - ENS Lyon, UMR 5668)  *)
(*******************************************************************)

open Common

let r = ref []
let add x = r := x :: !r

let node x b = add (`N(x,b))
let node_l x y = add (`Nl(x,y))
let node_r x y = add (`Nr(x,y))
let leaf x s o = add (`F(x,s,o))
let leaf_t x a t = add (`Ft(x,a,t))
let line x y = add (`Ln(x,y))
let ce x y = add (`CE(x,y))
let ok x y = add (`OK(x,y))
let skip x y = add (`Skip(x,y))
let entry x = add (`E x)

let clear () = r := []; ignore (Format.flush_str_formatter ())

let pp fmt = Format.fprintf Format.str_formatter fmt

let rec filter = function
  | [] -> Set.empty
  | (`CE xy | `OK xy | `Skip xy)::q -> Set.add xy (filter q)
  | _::q -> filter q

let render ~hk ~quote = 
  let filter = filter !r in
  let r = List.filter (function `Ln xy when Set.mem xy filter -> false | _ -> true) !r in
  
  ignore (Format.flush_str_formatter ());
  pp "digraph{node[style=invisible,root=true];";
  List.iter (function `E(x) -> pp "e%i;" x | _ -> ()) r;
  pp "node[style=filled,root=false,shape=box,fillcolor=orange];";
  List.iter (function `F(x,s,"0") -> pp "%i[label=%s%s%s,tooltip=%so=0%s];" x quote s quote quote quote | _ -> ()) r;
  pp "node[fillcolor=yellow];";
  List.iter (function `F(x,s,"1") -> pp "%i[label=%s%s%s,tooltip=%so=1%s];" x quote s quote quote quote | _ -> ()) r;
  pp "node[fillcolor=brown];";
  List.iter (function `F(x,s,o) when o<>"0" && o<>"1" -> pp "%i[label=%s%s%s,tooltip=%so=%s%s];" x quote s quote quote o quote | _ -> ()) r;
  pp "node[fillcolor=lightgray,shape=circle];";
  List.iter (function `N(x,b) -> pp "%i[label=%s%s%s];" x quote b quote | _ -> ()) r;
  pp "node[fillcolor=grey,shape=box,label=%ssink%s];" quote quote;
  List.iter (function `E(x) -> pp "e%i->%i;" x x
                    | `Nr(x,y) -> pp "%i->%i;" x y 
		    | `Ft(x,a,y) -> pp "%i->%i[label=%s%s%s];" x y quote a quote
		    | _ -> ()) r;
  pp "edge[style=dotted];";
  List.iter (function `Nl(x,y) -> pp "%i->%i;" x y | _ -> ()) r;
  pp "edge[style=solid,constraint=false,color=violet%s];" (if hk then "" else ",arrowhead=none");
  List.iter (function `Ln(x,y) -> pp "%i->%i;" x y | _ -> ()) r;
  pp "edge[color=blue];";
  List.iter (function `OK(x,y) -> pp "%i->%i;" x y | _ -> ()) r;
  pp "edge[style=dashed];";
  List.iter (function `Skip(x,y) -> pp "%i->%i;" x y | _ -> ()) r;
  List.iter (function `CE(x,y) -> pp "%i->%i[style=solid,label=%s≠%s,color=red,arrowhead=none];" x y quote quote | _ -> ()) r;
  pp "}";
  Format.flush_str_formatter ()

let r' = ref []
let save () = r' := !r
let restore () = r := !r'

