type msg = string
type msg_handler = msg -> unit

type lid = int (* local unique id *)
type gid =
    { g_rank : Mpi.rank; g_id : lid } (* global unique id *)

module Gid = struct
  type t = gid
  let compare = compare
end
module GidMap = Map.Make (Gid)
module GidSet = Set.Make (Gid)

type site = Mpi.rank

module Site = struct
  type t = site
  let compare = compare
end
module SiteMap = Map.Make (Site)
module SiteSet = Set.Make (Site)

let is_master () =
  Mpi.comm_rank Mpi.comm_world = 0

let available_site () =
  Mpi.comm_rank Mpi.comm_world

let counter = ref 0
let fresh () =
  incr counter;
  { g_rank = Mpi.comm_rank Mpi.comm_world;
    g_id = !counter }

let is_local gid =
  gid.g_rank = Mpi.comm_rank Mpi.comm_world

let to_msg d =
  Marshal.to_string d [Marshal.Closures]

let from_msg s =
  Marshal.from_string s 0

let send_owner gid tag d =
  Mpi.send (tag, to_msg d) gid.g_rank 0 Mpi.comm_world
let send site tag d =
  Mpi.send (tag, to_msg d) site 0 Mpi.comm_world

let receive () =
  let tag, (msg:msg) = Mpi.receive Mpi.any_source Mpi.any_tag Mpi.comm_world in
    tag, msg


let print_lid ff lid =
  Format.fprintf ff "%d" lid
let print_gid ff gid =
  Format.fprintf ff "%d.%d" gid.g_rank gid.g_id
