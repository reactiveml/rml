
type 'a behaviour =
  | O of 'a
  | P of 'a behaviour list
  | S of 'a behaviour * 'a behaviour
  | N

type 'a stack = { s_name : string;
                  mutable s_stack : ('a * 'a behaviour) list ;
                  mutable s_next_b : 'a behaviour list;
                  mutable s_ttl : int }

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

let checkers = ref ([]: 'a stack list)

let mk_stack s b n =
  { s_name = s; s_stack = []; s_next_b = b; s_ttl = n + 1 }

let check_empty s = match s.s_stack with
| [] -> debug s "List empty; Step OK"
| (x, _)::_ ->
    if error_on_unfired then
      (status_int s "*********** ERROR: result not fired: " x; exit unexpected_error_code)

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
  s.s_ttl <- s.s_ttl - 1;
  if s.s_ttl = 0 then (
    status s "Test OK";
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
  try
    debug_int s "Raising " r;
    let new_stack, bl = assoc_rm r s.s_stack in
    s.s_stack <- new_stack;
    List.iter (add_to_stack s) bl
  with
  | Not_found ->
      if error_on_unexpected then
        (status_int s "*********** ERROR: Unexpected result" r; exit unexpected_error_code)

let mk_checker name b =
  let n =  List.length b in
  let s = mk_stack name b n in
  checkers := s :: !checkers;
  ignore (step_stack s);
  act s, n

let step () =
  checkers := List.filter step_stack !checkers;
  if !checkers = [] then (
    if print_status then
      Format.printf "All tests OK@.";
    exit 0
  )

let end_program () =
  List.iter check_empty !checkers
