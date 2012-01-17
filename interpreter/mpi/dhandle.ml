
module type DHANDLE_TYPE = sig
  type key
  type ('a, 'b) value

  val compare : key -> key -> int
end

module type S = sig
  type ('a, 'b) handle
  type ('a, 'b) value
  type key

  module type LOCAL_VALUE = sig
    val local_value : key -> ('a, 'b) value -> ('a, 'b) value
  end

  type cache
  val mk_cache : (module LOCAL_VALUE) -> cache

  val init : cache -> key -> ('a, 'b) value -> ('a, 'b) handle
  val get : cache -> ('a, 'b) handle -> ('a, 'b) value
  val set_valid : ('a, 'b) handle -> unit
end

(** TODO: rendre key visible de l'exterieur ?? *)
module Make (T : DHANDLE_TYPE) (C : Communication.S) = struct

  module MyWeak = Weak_map.Make (struct
    type t = T.key
    let compare = T.compare
  end)

  type ('a, 'b) handle =
      { d_key : T.key;
        mutable d_token : Mpi.local_token;
        mutable d_value : ('a, 'b) T.value; }
  type ('a, 'b) value = ('a, 'b) T.value
  type key = T.key

  module type LOCAL_VALUE = sig
    val local_value : key -> ('a, 'b) value -> ('a, 'b) value
  end

  type cache = (module LOCAL_VALUE) * (Obj.t MyWeak.t) ref

  let mk_cache m = m, ref MyWeak.empty

  let is_valid dr =
    Mpi.is_valid_token dr.d_token

  let set_valid dr =
    dr.d_token <- Mpi.get_valid_token ()

  let find_local (m, memo) (dr: ('a, 'b) handle) =
    try
      (Obj.obj (MyWeak.find dr.d_key !memo) : ('a, 'b) value)
    with
      | Not_found -> (* allocate local value *)
          let module L = (val m : LOCAL_VALUE) in
          let r = L.local_value dr.d_key dr.d_value in
            dr.d_value <- r;
            set_valid dr;
            memo := MyWeak.add dr.d_key (Obj.repr r) !memo;
            dr.d_value

  let get cache dr =
    if is_valid dr then
      dr.d_value
    else
      find_local cache dr

  let init (_, memo) key v =
    let dr = { d_key = key;
               d_token = Mpi.get_valid_token ();
               d_value = v; }
    in
    memo := MyWeak.add key (Obj.repr v) !memo;
    dr
end

