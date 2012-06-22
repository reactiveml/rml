
module type DHANDLE_TYPE = sig
  type key
  type ('a, 'b) value

  val compare : key -> key -> int
end

module type S = sig
  type ('a, 'b) handle
  type ('a, 'b) value
  type key

  type local_fun = { c_local_value : 'a 'b. key -> ('a, 'b) value -> ('a, 'b) value; }
  type cache
  val mk_cache : local_fun -> cache

  val init : cache -> key -> ('a, 'b) value -> ('a, 'b) handle
  val get : cache -> ('a, 'b) handle -> ('a, 'b) value
  val set_valid : ('a, 'b) handle -> unit
end

(** TODO: rendre key visible de l'exterieur ?? *)
module Make (T : DHANDLE_TYPE) = struct

  module MyWeak = Weak_map.Make (struct
    type t = T.key
    let compare = T.compare
  end)

  type ('a, 'b) handle =
      { d_key : T.key;
        mutable d_token : Local_token.token;
        mutable d_value : ('a, 'b) T.value; }
  type ('a, 'b) value = ('a, 'b) T.value
  type key = T.key

  type local_fun = { c_local_value : 'a 'b. key -> ('a, 'b) value -> ('a, 'b) value; }
  type cache = local_fun * (Obj.t MyWeak.t) ref

  let mk_cache m = m, ref MyWeak.empty

  let is_valid dr =
    Local_token.is_valid dr.d_token

  let set_valid dr =
    dr.d_token <- Local_token.get_token ()

  let find_local ({ c_local_value = local_value }, memo) (dr: ('a, 'b) handle) =
    try
      (Obj.obj (MyWeak.find dr.d_key !memo) : ('a, 'b) value)
    with
      | Not_found -> (* allocate local value *)
         (* let module L = (val m : LOCAL_VALUE) in *)
          let r = local_value dr.d_key dr.d_value in
            dr.d_value <- r;
            set_valid dr;
            memo := MyWeak.add dr.d_key (Obj.repr r) !memo;
            dr.d_value

  let get cache dr =
    if Local_token.is_valid dr.d_token then
      dr.d_value
    else
      find_local cache dr

  let init (_, memo) key v =
    let dr = { d_key = key;
               d_token = Local_token.get_token ();
               d_value = v; }
    in
    memo := MyWeak.add key (Obj.repr v) !memo;
    dr
end

