
type 'a behaviour =
  | O of 'a
  | P of 'a behaviour list
  | S of 'a behaviour * 'a behaviour
  | N

type 'a stack = { s_name : string;
                  mutable s_active : bool;
                  mutable s_failed : bool;
                  mutable s_stack : ('a * 'a behaviour) list ;
                  mutable s_next_b : 'a behaviour list;
                  mutable s_ttl : int }

exception Test_success
exception Test_failed of string

let error_on_unexpected = true
let unexpected_error_code = 3
let error_on_unfired = true
let unfired_error_code = 10

let print_debug = false
let print_status = true

let debug st s =
  if print_debug then
    (print_string (st.s_name^":"^s); print_newline ())

let debug_int st s n =
 if print_debug then
    (print_string (st.s_name^":"^s); print_int n; print_newline ())

let status st s =
 if print_status then
    (print_string (st.s_name^":"^s); print_newline ())

let status_int st s n =
 if print_status then
    (print_string (st.s_name^":"^s); print_int n; print_newline ())


let mk_stack s b n =
  { s_name = s; s_active = true; s_failed = false; s_stack = []; s_next_b = b; s_ttl = n + 1 }

let check_empty s = match s.s_stack with
| [] -> debug s "List empty; Step OK"
| (x, _)::_ ->
    if error_on_unfired then
      (status_int s "*********** ERROR: result not fired: " x; s.s_failed <- true)

let add_to_stack s b =
  let rec _add_to_stack b k = match b with
  | O r -> debug_int s "Add one to stack" r; s.s_stack <- (r, k) :: s.s_stack
  | N -> ()
  | P bl -> List.iter (fun b -> _add_to_stack b k) bl
  | S (b1, b2) ->
      match k with
      | N -> _add_to_stack b1 b2
      | _ -> _add_to_stack b1 (S(b2,k))
  in
  _add_to_stack b N

let step_stack s =
  check_empty s;
  if s.s_failed then
    (status s "Test failed"; exit 2);
  s.s_ttl <- s.s_ttl - 1;
  if s.s_ttl = 0 then (
    status s "Test OK";
    s.s_active <- false;
    false
  ) else
    (match s.s_next_b with
      | [] -> true
      | b::bl ->
        s.s_next_b <- bl;
        add_to_stack s b;
        true)

let assoc_rm x l =
  let newl, res =
    List.fold_left
      (fun (newl, res) (y, v) -> if x = y then newl, (v::res) else ((y,v)::newl), res)
      ([], []) l
  in
  if res = [] then
    raise Not_found
  else
    newl, res

let act s r =
  if s.s_active then (
    try
      debug_int s "Raising " r;
      let new_stack, bl = assoc_rm r s.s_stack in
      s.s_stack <- new_stack;
      List.iter (add_to_stack s) bl
    with
      | Not_found ->
        if error_on_unexpected then
          (status_int s "*********** ERROR: Unexpected result" r; s.s_failed <- true)
  )


module type T = sig
  val new_test : string -> int behaviour list -> (int -> unit)
  val next_step : unit -> unit
  val end_test : unit -> unit
end

module Test = struct
  let checkers = ref ([]: 'a stack list)

  let new_test name b =
    let n =  List.length b in
    let s = mk_stack name b n in
    checkers := s :: !checkers;
    ignore (step_stack s);
    act s

  let next_step () =
    checkers := List.filter step_stack !checkers;
    if !checkers = [] then (
      if print_status then
        Format.eprintf "All tests OK@.";
      exit 0
    )

  let end_test () =
    List.iter check_empty !checkers
end
