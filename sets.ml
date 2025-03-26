(*******************************************************************)
(*      This is part of SAFA, it is distributed under the          *)
(*  terms of the GNU Lesser General Public License version 3       *)
(*           (see file LICENSE for more details)                   *)
(*                                                                 *)
(*  Copyright 2014: Damien Pous. (CNRS, LIP - ENS Lyon, UMR 5668)  *)
(*******************************************************************)

(** several implementations for finite sets of integers *)

module type PRE = sig
  type t
  val empty: t
  val union: t -> t -> t
  val inter: t -> t -> t
  val singleton: int -> t
  val mem: int -> t -> bool
  val equal: t -> t -> bool
  val compare: t -> t -> int
  val full: int -> t
  val hash: t -> int
  val fold: (int -> 'a -> 'a) -> t -> 'a -> 'a
  val shift: int -> t -> t
  val size: t -> int
  val rem: int -> t -> t
end

module type T = sig
  include PRE
  val add: int -> t -> t
  val is_empty: t -> bool
  val intersect: t -> t -> bool
  val diff: t -> t -> t
  val subseteq: t -> t -> bool
  val set_compare: t -> t -> [`Lt|`Eq|`Gt|`N]
  val map: (int -> int) -> t -> t
  val iter: (int -> unit) -> t -> unit
  val filter: (int -> bool) -> t -> t
  val cardinal: t -> int
  val forall: t -> (int -> bool) -> bool
  val exists: t -> (int -> bool) -> bool
  val to_list: t -> int list
  val of_list: int list -> t
  val print: Format.formatter -> t -> unit
  val print': (Format.formatter -> int -> unit) -> Format.formatter -> t -> unit
  val random: int -> float -> t
  module Map: Hashtbl.S with type key = t
end

module Extend(M: PRE): T = struct
  include M
  let is_empty = equal empty
  let add x = union (singleton x)
  let subseteq x y = equal (union x y) y
  let intersect x y = not (is_empty (inter x y))
  let diff x y = fold rem y x
  let set_compare x y =
    if equal x y then `Eq else
      let xy=union x y in
      if equal xy y then `Lt
      else if equal xy x then `Gt
      else `N
  let of_list = List.fold_left (fun x i -> add i x) empty 
  let cardinal x = fold (fun _ n -> 1+n) x 0
  let to_list x = fold (fun i q -> i::q) x []
  let filter f x = fold (fun i y -> if f i then add i y else y) x empty
  let iter f x = fold (fun i () -> f i) x ()
  let map f x = fold (fun i -> add (f i)) x empty
  let rec xprint g f = function
    | [] -> Format.pp_print_char f '}'
    | [x] ->  Format.fprintf f "%a}" g x
    | x::q -> Format.fprintf f "%a,%a" g x (xprint g) q
  let print f x = Format.fprintf f "{%a" (xprint Format.pp_print_int) (to_list x)
  let print' g f x = Format.fprintf f "{%a" (xprint g) (to_list x)
  let random n p =
    let rec aux acc = function
      | 0 -> acc
      | n ->
	if Random.float 1. < p then 
	  aux (add (n-1) acc) (n-1)
	else aux acc (n-1)
    in aux empty n
  let forall x f =
    try fold (fun i _ -> f i || raise Not_found) x true
    with Not_found -> false
  let exists x f =
    try fold (fun i _ -> f i && raise Not_found) x false
    with Not_found -> true
  module Map = Hashtbl.Make(M)
end

module OList = Extend(struct 
  type t = int list
  let empty = []
  let rec union h k = match h,k with
    | [],l | l,[] -> l
    | x::h', y::k' -> 
      match compare x y with
	| 1 -> y::union h  k'
	| 0 -> x::union h' k'
	| _ -> x::union h' k
  let rec inter h k = match h,k with
    | [],_ | _,[] -> []
    | x::h', y::k' -> 
      match compare x y with
	| 1 ->    inter h  k'
	| 0 -> x::inter h' k'
	| _ ->    inter h' k
  let singleton x = [x]
  let equal = (=)
  let compare = compare
  let rec mem x = function
    | [] -> false
    | y::q -> match compare x y with
	| 1 -> mem x q
	| 0 -> true 
	| _ -> false
  let fold = List.fold_right
  let shift n = List.map ((+) n)
  let hash = Hashtbl.hash
  let rem x h = failwith "todo: OList.rem"
  let rec full acc = function
    | 0 -> acc
    | n -> full (n-1::acc) (n-1)
  let full = full []
  let size = List.length
end)

module Small = Extend(struct 
  type t = int
  let ws = Sys.word_size-1
  let empty = 0
  let union = (lor)
  let inter = (land)
  let singleton i = assert(i<ws); 1 lsl i
  let equal = (=)
  let compare = compare
  let mem i x = inter (singleton i) x <> empty
  let fold f x acc =
    let r = ref acc in
    for i = ws - 1 downto 0 do
      if mem i x then 
	r := f i !r
    done;
    !r
  let rem x h = failwith "todo: Small.rem"
  let hash = Hashtbl.hash
  let full n = singleton n - 1
  let size x = fold (+) x 0
  let shift n m = m lsl n               (* unsafe... *)
end)

(*
module Zarith = Extend(struct 
  type t = Z.t
  let empty = Z.zero
  let union = Z.logor
  let inter = Z.logand
  let singleton = Z.shift_left Z.one
  let equal = Z.equal
  let compare = Z.compare
  let mem i x = not (Z.equal (Z.logand (Z.shift_left Z.one i) x) Z.zero)
  let shift n m = Z.shift_left m n
  let ws = Sys.word_size
  (* let fold f x acc = *)
  (*   let r = ref acc in *)
  (*   let m = ref Z.one in *)
  (*   for i = 0 to Z.size x * ws - 1 do *)
  (*     if not (Z.equal Z.zero (Z.logand x !m)) then  *)
  (*       r := f i !r; *)
  (*     m := Z.shift_left !m 1; *)
  (*   done; *)
  (*   !r *)
  let fold f x acc =
    let r = ref acc in
    for i = Z.size x * ws - 1 downto 0 do
      if mem i x then
        r := f i !r
    done;
    !r
  let shift n m = Z.shift_left m n
  let hash = Z.hash
  let full n = Z.pred (singleton n) 
  let rem x = inter (Z.lognot (singleton x))
  let size = Z.popcount
end)


module ZarithInlined = struct 
  type t = Z.t
  let empty = Z.zero
  let union = Z.logor
  let inter = Z.logand
  let singleton = Z.shift_left Z.one
  let equal = Z.equal
  let compare = Z.compare
  let mem i x = not (Z.equal (Z.logand (Z.shift_left Z.one i) x) Z.zero)
  let shift n m = Z.shift_left m n
  let ws = Sys.word_size
  let fold f x acc =
    let r = ref acc in
    for i = Z.size x * ws - 1 downto 0 do
      if mem i x then
        r := f i !r
    done;
    !r
  let iter f x =
    for i = Z.size x * ws - 1 downto 0 do
      if mem i x then f i
    done
  let hash = Z.hash
  let full n = Z.pred (singleton n) 
  let diff x y = Z.logand x (Z.lognot y)
  let rem x = Z.logand (Z.lognot (singleton x))
  let size = Z.popcount

  let is_empty = equal empty
  let add x = union (singleton x)
  let subseteq x y = equal (union x y) y
  let intersect x y = not (equal empty (inter x y))
  let set_compare x y = 
  if equal x y then `Eq else
      let xy=union x y in
      if equal xy y then `Le
      else if equal xy x then `Gt
      else `N
  let of_list = List.fold_left (fun x i -> add i x) empty 
  let to_list x = fold (fun i q -> i::q) x []
  let map f x = fold (fun i -> add (f i)) x empty
  let filter f x = fold (fun i y -> if f i then add i y else y) x empty
  let rec xprint f = function
    | [] -> Format.pp_print_char f '}'
    | [x] ->  Format.fprintf f "%i}" x
    | x::q -> Format.fprintf f "%i,%a" x xprint q
  let print f x = Format.fprintf f "{%a" xprint (to_list x)
  let random n p =
    let rec aux acc = function
      | 0 -> acc
      | n ->
	if Random.float 1. < p then 
	  aux (add (n-1) acc) (n-1)
	else aux acc (n-1)
    in aux empty n
  let forall x f =
    try fold (fun i _ -> f i || raise Not_found) x true
    with Not_found -> false
  let exists x f =
    try fold (fun i _ -> f i && raise Not_found) x false
    with Not_found -> true
  module Map = Hashtbl.Make(Z)
end
*)

module NarithInlined = struct 
  type t = int list
(*
0  -> [1]
1  -> [2]
2  -> [4]
...
n  -> [1 lsl n]
...
62 -> [2^62]
63 -> [0; 1]
...
*)
  let empty = []
  let (<::) x q = if x=0 && q=[] then [] else x::q
  let rec union x y = match x,y with 
    | [],z | z,[] -> z
    | a::x, b::y -> a lor b :: union x y
  let rec inter x y = match x,y with 
    | [],_ | _,[] -> []
    | a::x, b::y -> a land b <:: inter x y
  let rec pad acc = function 0 -> acc | n -> pad (0::acc) (n-1)
  let rec get n x = match n,x with 0,a::_ -> Some a | _,[] -> None | n,_::x -> get (n-1) x
  let width = Sys.word_size - 1 
  let singleton i = pad [1 lsl (i mod width)] (i/width)  
  let equal = (=)
  let compare = compare
  let mem i x = match get (i/width) x with
    | None -> false
    | Some a -> (a lsr (i mod width)) land 1 <> 0 
  let shift n x = 
    if x=[] then [] else
    let m = n mod width in
    if m=0 then pad x (n/width) else
    let wm = width-m in 
    let rec xshift b = function
      | [] -> if b=0 then [] else [b]
      | a::x -> b lor (a lsl m) :: xshift (a lsr wm) x
    in
    pad (xshift 0 x) (n/width)
  (* let shift n x =  *)
  (*   pad x (1+n/width) *)
  let fold f x acc =
    let rec aux i acc = function
      | [] -> acc
      | a::x -> aux' i width acc x a
    and aux' i w acc x = function
      | 0 -> aux (i+w) acc x
      | a -> aux' (i+1) (w-1) (if a land 1 <> 0 then f i acc else acc) x (a lsr 1)
    in aux 0 acc x
  let iter f x = fold (fun i () -> f i) x ()
  let hash = Hashtbl.hash
  let full n = 
    let rec xfull acc = function 0 -> acc | n -> xfull (-1::acc) (n-1) in
    let m = n mod width in
    if m=0 then xfull [] (n/width) else
    xfull [1 lsl m - 1] (n/width)
  let rec diff x y = match x,y with 
    | [],_ -> []
    | _,[] -> x
    | a::x, b::y -> a land (lnot b) <:: diff x y
  let size x = fold (fun _ i -> i+1) x 0

  let is_empty = function [] -> true | _ -> false
  let add x = union (singleton x)       (* optimisable *)
  let rec subseteq x y = match x,y with
    | [],_ -> true
    | _,[] -> false
    | a::x, b::y -> (a lor b = b) && subseteq x y
  let rec intersect x y = match x,y with
    | [],_ | _,[] -> false
    | a::x, b::y -> (a land b != 0) || intersect x y
  let rec set_compare x y = match x,y with
    | [],[] -> `Eq
    | [],_ -> `Lt
    | _,[] -> `Gt
    | a::x, b::y -> 
      if a=b then set_compare x y else 
        let c = a lor b in 
        if (c = b) then if subseteq x y then `Lt else `N
        else if (c = a) && subseteq y x then `Gt else `N
  let cardinal x = fold (fun _ n -> 1+n) x 0
  let of_list = List.fold_left (fun x i -> add i x) empty 
  let to_list x = List.rev (fold (fun i q -> i::q) x [])
  let map f x = fold (fun i -> add (f i)) x empty
  let filter f x = fold (fun i x -> if f i then add i x else x) x empty (* optimisable *)
  let rem i = filter (fun j -> i<>j)    (* highly optimisable *)
  let rec xprint g f = function
    | [] -> Format.pp_print_char f '}'
    | [x] ->  Format.fprintf f "%a}" g x
    | x::q -> Format.fprintf f "%a,%a" g x (xprint g) q
  let print f x = Format.fprintf f "{%a" (xprint Format.pp_print_int) (to_list x)
  let print' g f x = Format.fprintf f "{%a" (xprint g) (to_list x)
  let random n p =                      (* optimisable *)
    let rec aux acc = function
      | 0 -> acc
      | n ->
	if Random.float 1. < p then 
	  aux (add (n-1) acc) (n-1)
	else aux acc (n-1)
    in aux empty n
  let forall x f =
    try fold (fun i _ -> f i || raise Not_found) x true
    with Not_found -> false
  let exists x f =
    try fold (fun i _ -> f i && raise Not_found) x false
    with Not_found -> true
  module Map = Hashtbl.Make(struct 
    type t = int list let equal = (=) let compare = compare let hash = hash 
  end)
end


module AVL = Extend(struct
  include Set.Make(struct type t = int let compare = compare end)
  let rem = remove
  let hash = Hashtbl.hash
  let rec full acc = function
    | 0 -> acc
    | n -> full (add (n-1) acc) (n-1)
  let full = full empty
  let size x = fold (fun _ n -> n + 1) x 0
  let shift n x = fold (fun i -> add (i+n)) x empty
end)



module Dup(M1: T)(M2: T): T = struct 
  type t = M1.t*M2.t
  let ret ?(msg="") x1 x2 = 
    let x2' = M2.to_list x2 in
    if x2'=[] && not (x2=M2.empty) then failwith ("DUP: invalid empty set - "^msg)
    else if M1.to_list x1 <> x2' then failwith ("DUP: set difference - "^msg)
    else (x1,x2)
  let ret' ?(msg="") x1 x2 = if x1 = x2 then x1 else failwith ("DUP: difference - "^msg)
  let empty = ret M1.empty M2.empty
  let union (x1,x2) (y1,y2) =
    assert (M1.to_list x1 = M2.to_list x2);
    assert (M1.to_list y1 = M2.to_list y2);
    try ret ~msg:"union" (M1.union x1 y1) (M2.union x2 y2)
    with Failure _ ->
      Format.eprintf "union %a %a\n = %a\n<> %a\n%!"
        M2.print x2 M2.print y2 M1.print (M1.union x1 y1) M2.print (M2.union x2 y2);
      failwith "BING"
  let inter (x1,x2) (y1,y2) = ret (M1.inter x1 y1) (M2.inter x2 y2)
  let singleton x = ret (M1.singleton x) (M2.singleton x)
  let equal (x1,x2) (y1,y2) = ret' (M1.equal x1 y1) (M2.equal x2 y2)
  let compare _ _ = failwith "DISABLED: DUP.compare"
  let hash _ = failwith "DISABLED: DUP.hash"
  let mem x (x1,x2) = ret' (M1.mem x x1) (M2.mem x x2)
  let fold f (x1,x2) acc = ret' (M1.fold f x1 acc) (M2.fold f x2 acc)
  let shift n (x1,x2) = 
    let sx2 = M2.shift n x2 in
    if M2.to_list sx2 = [] && sx2<>M2.empty then
      Format.eprintf "shift %i %a = %a <> []\n%!" n M2.print x2 M2.print sx2;
    ret (M1.shift n x1) (M2.shift n x2)
  let rem i (x1,x2) = ret (M1.rem i x1) (M2.rem i x2)
  let full n = ret (M1.full n) (M2.full n)
  let size (x1,x2) = ret' (M1.size x1) (M2.size x2)

  let is_empty (x1,x2) = ret' (M1.is_empty x1) (M2.is_empty x2)
  let add i (x1,x2) = ret (M1.add i x1) (M2.add i x2)
  let subseteq (x1,x2) (y1,y2) = ret' (M1.subseteq x1 y1) (M2.subseteq x2 y2)
  let intersect (x1,x2) (y1,y2) = ret' (M1.intersect x1 y1) (M2.intersect x2 y2)
  let diff (x1,x2) (y1,y2) = ret (M1.diff x1 y1) (M2.diff x2 y2)
  let set_compare (x1,x2) (y1,y2) = ret' (M1.set_compare x1 y1) (M2.set_compare x2 y2)
  let cardinal (x1,x2) = ret' (M1.cardinal x1) (M2.cardinal x2)
  let of_list l = ret (M1.of_list l) (M2.of_list l)
  let to_list (x1,x2) = ret' (M1.to_list x1) (M2.to_list x2)
  let filter f (x1,x2) = ret (M1.filter f x1) (M2.filter f x2)
  let map f (x1,x2) = ret (M1.map f x1) (M2.map f x2)
  let iter f (x1,_) = M1.iter f x1
  let print f (x1,_) = M1.print f x1
  let print' g f (x1,_) = M1.print' g f x1
  let random n p = failwith "TODO: Sets.Dup.random"
  let forall (x1,x2) f = ret' (M1.forall x1 f) (M2.forall x2 f)
  let exists (x1,x2) f = ret' (M1.exists x1 f) (M2.exists x2 f)
  module Map = Hashtbl.Make(struct type t = M1.t*M2.t let equal=equal let compare=compare let hash=hash end)
end
