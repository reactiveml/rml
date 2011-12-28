

module type S = sig
  type 'a handle
  type key

  val mk_cache : ('a -> 'a) -> 'a cache

  val init : key -> 'a -> 'a handle
  val get : 'a cache -> 'a handle -> 'a
end

(** TODO: rendre key visible de l'exterieur ?? *)
module Make (T : Map.OrderedType) (C : Communication.S) = struct

  module MyWeak = Weak_map.Make (struct
    type t = T.key
    let compare = T.compare
  end)

  type 'a handle =
      { d_master : Mpi.rank;
        d_key : T.t;
        mutable d_valid_for : Mpi.rank;
        mutable d_value : 'a; }
  type 'a cache = ('a -> 'a) * ('a MyWeak.t) ref
  type key = T.t

  let mk_cache local_value = local_value, ref MyWeak.empty

  let find_local (local_value, memo) dr =
    try
      (MyWeak.find dr.d_key !memo)
    with
      | Not_found -> (* allocate local value *)
          let r = local_value dr.d_value in
            dr.d_value <- r;
            dr.d_valid_for <- Mpi.comm_rank Mpi.comm_world;
            memo := MyWeak.add dr.d_key r !memo;
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

