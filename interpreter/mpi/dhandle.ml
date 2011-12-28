
module type DHANDLE_TYPE = sig
  type key
  type ('a, 'b) value

  val compare : key -> key -> int
end

module type S = sig
  type ('a, 'b) handle
  type ('a, 'b) value
  type key

  type cache
  val mk_cache : (key -> ('a, 'b) value -> ('a, 'b) value) -> cache

  val init : cache -> key -> ('a, 'b) value -> ('a, 'b) handle
  val get : cache -> ('a, 'b) handle -> ('a, 'b) value
end

(** TODO: rendre key visible de l'exterieur ?? *)
module Make (T : DHANDLE_TYPE) (C : Communication.S) = struct

  module MyWeak = Weak_map.Make (struct
    type t = T.key
    let compare = T.compare
  end)

  type ('a, 'b) handle =
      { d_master : Mpi.rank;
        d_key : T.key;
        mutable d_valid_for : Mpi.rank;
        mutable d_value : ('a, 'b) T.value; }
  type ('a, 'b) value = ('a, 'b) T.value
  type key = T.key
  type cache = (key -> ('a, 'b) value -> ('a, 'b) value) * (Obj.t MyWeak.t) ref

  let mk_cache local_value = local_value, ref MyWeak.empty

  let find_local (local_value, memo) (dr: ('a, 'b) handle) =
    try
      (Obj.obj (MyWeak.find dr.d_key !memo) : ('a, 'b) value)
    with
      | Not_found -> (* allocate local value *)
          let r = local_value dr.dr_key dr.d_value in
            dr.d_value <- r;
            dr.d_valid_for <- Mpi.comm_rank Mpi.comm_world;
            memo := MyWeak.add dr.d_key (Obj.repr r) !memo;
            dr.d_value

  let get memo dr =
    if dr.d_valid_for = Mpi.comm_rank Mpi.comm_world then
      dr.d_value
    else
      find_local memo dr

  let init (_, memo) key v =
    let dr = { d_master = Mpi.comm_rank Mpi.comm_world;
               d_key = key;
               d_valid_for = Mpi.comm_rank Mpi.comm_world;
               d_value = v; }
    in
    MyWeak.add key dr !memo
end

