
module type TAG_TYPE = sig
  type 'gid t
  val print : (Format.formatter -> 'gid -> unit) -> Format.formatter -> 'gid t -> unit
end

module type S = sig
  type lid
  type gid

  type msg
  type 'gid tag
  type site

  val print_gid : Format.formatter -> gid -> unit
  val print_site : Format.formatter -> site -> unit

  module GidMap : (Map.S with type key = gid)
  module GidSet : (Set.S with type elt = gid)

  module SiteMap : (Map.S with type key = site)
  module SiteSet : (Set.S with type elt = site)

  val is_master : unit -> bool
  val available_site : unit -> site

  val fresh : unit -> gid
  val is_local : gid -> bool

  val to_msg : 'a -> msg
  val from_msg : msg -> 'a

  val send : site -> gid tag -> 'a -> unit
  val send_owner : gid -> gid tag -> 'a -> unit
  val receive : unit -> gid tag * msg
end

open Runtime_options


let msg_tag = 0

module Make (P : TAG_TYPE) = struct
  type msg = string
  type msg_handler = msg -> unit

  type lid = int (* local unique id *)
  type gid =
      { g_rank : Mpi.rank; g_id : lid } (* global unique id *)
  type 'gid tag = 'gid P.t

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


  let print_here ff () =
    Format.fprintf ff "%d" (Mpi.communicator_rank ())

  let print_gid ff gid =
    Format.fprintf ff "%d.%d" gid.g_rank gid.g_id

  let print_site ff (site:site) =
    Format.fprintf ff "%d" site

  let print_tag = P.print print_gid

  let local_site () =
    Mpi.communicator_rank ()

  let site_of_gid { g_rank = r } = r

  let is_master () =
    local_site () = !Runtime_options.min_rank

  let master_site () = (!Runtime_options.min_rank : Mpi.rank)

  let nth_site n = (n + 1 + !Runtime_options.min_rank : Mpi.rank)

  let number_of_sites () =
    Mpi.communicator_size - (1 + !Runtime_options.min_rank)

  type seed = int ref

  let mk_seed () = ref 0
  let fresh counter =
    incr counter;
    { g_rank = local_site ();
      g_id = !counter }

  let is_local gid =
    gid.g_rank = local_site ()

  let to_msg d =
    Marshal.to_string d [Marshal.Closures]

  let from_msg s =
    Marshal.from_string s 0

  let send_owner gid tag d =
    print_debug "%a: Send '%a' to gid '%a' at site '%d'@."
      print_here ()  print_tag tag  print_gid gid  gid.g_rank;
    Mpi.send (tag, to_msg d) gid.g_rank msg_tag

  let send site tag d =
    print_debug "%a: Send '%a' to site '%a'@."
      print_here ()  print_tag tag  print_site site;
    Mpi.send (tag, to_msg d) site msg_tag

  let broadcast tag d =
    for i = 0 to number_of_sites () - 1 do
      let s = nth_site i in
      if s <> local_site () then
        send s tag d
    done

  let broadcast_set s tag d =
    SiteSet.iter (fun site -> send site tag d) s

  let receive () =
    let tag, (msg:msg) = Mpi.receive Mpi.any_source msg_tag in
      print_debug "%a: Received '%a'@."
        print_here ()  print_tag tag;
      tag, msg
end

module Test = struct
  let tag_counter = ref msg_tag

  let fresh_channel () =
    incr tag_counter;
    let tag = !tag_counter in
    let send d =
      Mpi.send_int d 0 tag in
    let receive () =
      Mpi.receive_int Mpi.any_source tag
    in
    send, receive
end

