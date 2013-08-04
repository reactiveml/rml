(***************************************************************)
(*                        Reactive Asco                        *)
(*             http://reactiveml.org/reactive_asco             *)
(*                                                             *)
(*                                                             *)
(*  Authors: Guillaume Baudart (guillaume.baudart@ens.fr)      *)
(*           Louis Mandel (louis.mandel@lri.fr)                *)
(*                                                             *)
(***************************************************************)

(** Implementation of priority queues as in the OCaml manual. *)

type ('prio, 'a) queue =
  | Empty
  | Node of 'prio * 'a * ('prio, 'a) queue * ('prio, 'a) queue

let empty = Empty

let is_empty = function Empty -> true | _ -> false

let rec push queue (delay, elt) =
  match queue with
  | Empty -> Node(delay, elt, Empty, Empty)
  | Node(d, e, left, right) ->
      if delay <= d
      then Node(delay, elt, push right (d, e), left)
      else Node(d, e, push right (delay, elt), left)

exception Queue_is_empty

let rec remove_top = function
  | Empty -> raise Queue_is_empty
  | Node(delay, elt, left, Empty) -> left
  | Node(delay, elt, Empty, right) -> right
  | Node(delay, elt, (Node(ldelay, lelt, _, _) as left),
         (Node(rdelay, relt, _, _) as right)) ->
           if ldelay <= rdelay
           then Node(ldelay, lelt, remove_top left, right)
           else Node(rdelay, relt, left, remove_top right)

let pop = function
  | Empty -> None
  | Node(delay, elt, _, _) as queue -> Some(delay, elt, remove_top queue)
