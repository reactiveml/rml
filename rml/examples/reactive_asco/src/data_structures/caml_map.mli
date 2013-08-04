(***************************************************************)
(*                        Reactive Asco                        *)
(*             http://reactiveml.org/reactive_asco             *)
(*                                                             *)
(*                                                             *)
(*  Authors: Guillaume Baudart (guillaume.baudart@ens.fr)      *)
(*           Louis Mandel (louis.mandel@lri.fr)                *)
(*                                                             *)
(***************************************************************)

(** The [Map] module of OCaml without functors. *)

type ('key, 'a) t
val empty: ('key, 'a) t
val is_empty: ('key, 'a) t -> bool
val mem:  'key -> ('key, 'a) t -> bool
val add: 'key -> 'a -> ('key, 'a) t -> ('key, 'a) t
val singleton: 'key -> 'a -> ('key, 'a) t
val remove: 'key -> ('key, 'a) t -> ('key, 'a) t
val merge: ('key -> 'a option -> 'b option -> 'c option) ->
  ('key, 'a) t -> ('key, 'b) t -> ('key, 'c) t
val compare: ('a -> 'a -> int) -> ('key, 'a) t -> ('key, 'a) t -> int
val equal: ('a -> 'a -> bool) -> ('key, 'a) t -> ('key, 'a) t -> bool
val iter: ('key -> 'a -> unit) -> ('key, 'a) t -> unit
val fold: ('key -> 'a -> 'b -> 'b) -> ('key, 'a) t -> 'b -> 'b
val for_all: ('key -> 'a -> bool) -> ('key, 'a) t -> bool
val exists: ('key -> 'a -> bool) -> ('key, 'a) t -> bool
val filter: ('key -> 'a -> bool) -> ('key, 'a) t -> ('key, 'a) t
val partition:
    ('key -> 'a -> bool) -> ('key, 'a) t -> ('key, 'a) t * ('key, 'a) t
val cardinal: ('key, 'a) t -> int
val bindings: ('key, 'a) t -> ('key * 'a) list
val min_binding: ('key, 'a) t -> ('key * 'a)
val max_binding: ('key, 'a) t -> ('key * 'a)
val choose: ('key, 'a) t -> ('key * 'a)
val split: 'key -> ('key, 'a) t -> ('key, 'a) t * 'a option * ('key, 'a) t
val find: 'key -> ('key, 'a) t -> 'a
val map: ('a -> 'b) -> ('key, 'a) t -> ('key, 'b) t
val mapi: ('key -> 'a -> 'b) -> ('key, 'a) t -> ('key, 'b) t

(** [nearest x m] searchs the entry [(v, d)] in [m] such that
    [d] is the bigest value which satifies [d <= x]. The map [m]
    must be such that if [(v1, d1)] and [(v2, d2)] are in [m] then
    [v1 <= v2] if and only if [d1 <= d2].
*)
val nearest: 'a -> ('key, 'a) t -> 'key * 'a
