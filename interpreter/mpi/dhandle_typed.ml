module type S = sig
  type 'a handle
  type 'a cache
  type key

  val mk_cache : ('a -> 'a) -> 'a cache

  val init : 'a cache -> key -> 'a -> 'a handle
  val get : 'a cache -> 'a handle -> 'a

  val get_stored : 'a handle -> 'a
  val get_local : 'a cache -> 'a handle -> 'a
end


(** TODO: rendre key visible de l'exterieur ?? *)
module Make (T : Map.OrderedType) = struct

  module MyWeak = Weak_map.Make (struct
    type t = T.t
    let compare = T.compare
  end)

  type 'a handle =
      { d_key : T.t;
        mutable d_token : Local_token.token;
        mutable d_value : 'a; }
  type 'a cache = ('a -> 'a) * ('a MyWeak.t) ref
  type key = T.t

  let mk_cache local_value = local_value, ref MyWeak.empty

  let is_valid dr =
    Local_token.is_valid dr.d_token

  let set_valid dr =
    dr.d_token <- Local_token.get_token ()

  let find_local (local_value, memo) dr =
    try
      (MyWeak.find dr.d_key !memo)
    with
      | Not_found -> (* allocate local value *)
          let r = local_value dr.d_value in
            dr.d_value <- r;
            set_valid dr;
            memo := MyWeak.add dr.d_key r !memo;
            dr.d_value

  let get memo dr =
    if is_valid dr then
      dr.d_value
    else
      find_local memo dr

  let get_stored dr = dr.d_value
  let get_local memo dr =
    find_local memo dr

  let init (_, memo) key v =
    let dr = { d_key = key;
               d_token = Local_token.get_token ();
               d_value = v; }
    in
    memo := MyWeak.add key v !memo;
    dr
end

