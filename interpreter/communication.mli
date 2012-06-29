
module type TAG_TYPE = sig
  type 'gid t

  val dummy : 'gid t
  val flush_after : 'gid t -> bool
  val print : (Format.formatter -> 'gid -> unit) -> Format.formatter -> 'gid t -> unit
end

module type S = sig
  type lid
  type gid

  type seed

  type msg
  type 'gid tag
  type site

  val dummy_tag : 'gid tag

  val print_here : Format.formatter -> unit -> unit
  val print_gid : Format.formatter -> gid -> unit
  val print_site : Format.formatter -> site -> unit
  val print_tag : Format.formatter -> gid tag -> unit

  module GidMap : (Map.S with type key = gid)
  module GidSet : (Set.S with type elt = gid)

  module SiteMap : (Map.S with type key = site)
  module SiteSet : (Set.S with type elt = site)

  val is_master : unit -> bool
  val local_site : unit -> site
  val site_of_gid : gid -> site

  val master_site : unit -> site
  val all_sites : unit -> SiteSet.t

  val mk_seed : unit -> seed
  val fresh : seed -> gid
  val relocate_gid : gid -> site -> gid
  val is_local : gid -> bool

  val to_msg : 'a -> msg
  val from_msg : msg -> 'a

  val send : site -> gid tag -> 'a -> unit
    (* broadcast to all the sites in the set *)
  val broadcast_set : SiteSet.t -> gid tag -> 'a -> unit
    (* broadcast to everybody except master site and the current site *)
  val broadcast : gid tag -> 'a -> unit
  val send_owner : gid -> gid tag -> 'a -> unit
  val receive : unit -> gid tag * msg

  val flush : unit -> unit
end

module type T = sig
  val fresh_channel : unit -> (int -> unit) * (unit -> int) * (int -> int)
end
