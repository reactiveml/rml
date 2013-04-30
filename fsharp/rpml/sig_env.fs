module Event

open Types

type clock = int ref
type clock_index = int
type ('a, 'b) t =
    { mutable clock : clock;
      kind : signal_kind;
      mutable status: int;
      mutable reset : bool;
      mutable value: 'b;
      mutable pre_status: int;
      mutable last: 'b;
      mutable _default: 'b;
      combine: ('a -> 'b -> 'b); }

let absent = -2

let create ck kind _default combine =
  { clock = ck;
    kind = kind;
    status = absent;
    reset = false;
    value = _default;
    pre_status = absent;
    last = _default;
    _default = _default;
    combine = combine; }

(* -------------------------- Access functions -------------------------- *)
let _default n = n._default
let status n =
  n.status = !(n.clock)

let value n = n.value

let pre_status n =
  if n.status = !(n.clock)
  then n.pre_status = !(n.clock) - 1
  else n.status = !(n.clock) - 1

let last n =
  if n.reset
  then n._default
  else (
    if n.status = !(n.clock)
    then n.last
    else n.value
  )

let pre_value n =
  if n.status = !(n.clock)
  then
    if n.pre_status = !(n.clock) - 1
    then n.last
    else n._default
  else
    if n.status = !(n.clock) - 1
    then n.value
    else n._default

let one n =
  match n.value with
  | x :: _ -> x
  | _ -> raise Types.RML

let emit n v =
  if n.status <> !(n.clock)
  then
    (n.pre_status <- n.status;
     n.last <- if n.reset then n._default else n.value;
     n.status <- !(n.clock);
     n.reset <- false;
     n.value <- if n.kind = Memory then n.combine v n.last else n.combine v n._default)
  else
    n.value <- n.combine v n.value

let set_value n v =
  if n.status <> !(n.clock)
  then
    (n.pre_status <- n.status;
     n.last <- if n.reset then n._default else n.value;
     n.status <- !(n.clock);
     n.reset <- false;
     n.value <- v)
  else
    n.value <- v

let copy n new_n =
  n.status <- new_n.status;
  n.value <- new_n.value;
  n.pre_status <- new_n.pre_status;
  n.last <- new_n.last

let reset n =
  n.reset <- true

let init_clock () =
  ref 0

let set_clock n ck =
  n.clock <- ck

let next ck =
  incr ck

let get ck = !ck
let set ck v = ck := v

let equal ck1 ck2 =
  ck1 = ck2

(*
let print_clock_index ff c =
  Format.fprintf ff "%d" c
*)