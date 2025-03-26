(*******************************************************************)
(*      This is part of SAFA, it is distributed under the          *)
(*  terms of the GNU Lesser General Public License version 3       *)
(*           (see file LICENSE for more details)                   *)
(*                                                                 *)
(*  Copyright 2014: Damien Pous. (CNRS, LIP - ENS Lyon, UMR 5668)  *)
(*******************************************************************)

(** Symbolic algorithms for language equivalence *)

open Common
open Automata

(** Symbolic equivalence check, using queues [Q]

    Candidates for [Q] include:
    - fifo queues for breadth-first exploration ([Queues.BFS])
    - lifo queues for depth-first exploration ([Queues.DFS])
    - random queues for random exploration ([Queues.RFS])

    By changing the BDD unifier, we get either the naive or the Hopcroft
    and Karp like algorithm.

    The first argument allows one to trace the execution of the
    algorithm, a priori using the module [Trace]. (Cf. [Symbolic_kat]
    for a concrete usage.)

    Return [None] if the states were language-equivalent, or a path
    leading to two states whose output value differ. (Note that the path
    is reversed.) *)
module Make(Q: QUEUE): sig
  val equiv: 
    ?tracer:([`OK|`CE|`Skip] -> 's -> 's -> unit) ->
    ('s,'k) Bdd.unifier ->
    ('s,'v,'k,'o) sdfa ->
    's -> 's -> ('s*'s*('v,'k) gstring_) option

  (** same algorithms, further enhanced using up to congruence
      the transition function of the automaton should be an homomorphism 
      for the sup-semilattice on [int_set] given by union and empty set
      the output function of the automaton should also be an homomorphism 
      for some notion of sup-semilattice on ['o]
   *)
  val equiv_c: 
    ?tracer:([`OK|`CE|`Skip] -> int_set -> int_set -> unit) ->
    (int_set,'k) Bdd.unifier ->
    (int_set,'v,'k,'o) sdfa ->
    int_set -> int_set -> (int_set*int_set*('v,'k) gstring_) option
end


(** symbolic antichain algorithm, for checking universality w.r.t. a given predicate on outputs
    - the comparison function should correspond to a partial order 
    - the predicate on outputs should be upward closed:
       [cmp x y = `Ge && pred (o y)] should entail [pred (o x)]
    - the transition function of the automaton should be monotone w.r.t. the partial order
*)
val constant_ac: 
  ('s,'v,'k,'o) sdfa ->
  ('s -> 's -> [`Ge|`Lt|`Un]) ->
  ('o -> bool) -> 's -> ('s*('v,'k) gstring_) option
