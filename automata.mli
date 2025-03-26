open Common
open Bdd

(** Types for the various kind of manipulated automata *)

(** symbolic deterministic automata *)
module SDFA: sig
  type ('s, 'v, 'k, 'o) t = {
    m: ('s,'k) mem;
    t: 's -> ('v, ('s,'k) node) span;
    o: 's -> 'o;
    norm: 's -> 's;
    output_check: 's -> 's -> bool;
    state_info: 's formatter;
  }
  (** print an automaton using the module [Trace], possibly excluding
      the specified node; if [labels] false, then label states with
      identifiers rather than with their content *)
  val trace: ?labels: bool -> ?exclude:'s -> 'v formatter -> 'k formatter -> 'o formatter ->
    ('s,'v,'k,'o) t -> ('s -> unit)
  (** number of states and Bdd internal nodes reachable from the given list of states *)
  val size: ('s,_,_,_) t -> 's list -> int * int
  (** generic normalisation function: identity *)
  val generic_norm: 's -> 's
  (** generic output check: outputs are computed and compared physically *)
  val generic_output_check: ('s -> 'o) -> 's -> 's -> bool
end
type ('s,'v,'k,'o) sdfa = ('s,'v,'k,'o) SDFA.t

(** symbolic non-deterministic automata *)
module SNFA:  sig
  type ('s, 'set, 'v, 'k, 'o) t = {
    m: ('set,'k) mem;
    t: 's -> ('v, ('set, 'k) node) span;
    o: 's -> 'o;
    o0: 'o;
    o2: 'o -> 'o -> 'o;
    state_info: 's formatter;
  }
  (** reindex and memoise an NFA *)
  val reindex: ('s hval,'s hset,'v,'k,'o) t -> (int,int_set,'v,'k,'o) t * ('s hset -> int_set)
  (** number of states and Bdd internal nodes reachable from the given set of states *)
  val size: (int,int_set,_,_,_) t -> int_set -> int * int
end
type ('s,'t,'v,'k,'o) snfa = ('s,'t,'v,'k,'o) SNFA.t

(** non-deterministic automata with epsilon transitions *)
module SENFA: sig
  type 'v t = {
    e: (int * formula * int) list;
    t: (int * 'v * int) list;
    o: int set;
    size: int;
  }
end
type 'v senfa = 'v SENFA.t
