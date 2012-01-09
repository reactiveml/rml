
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
  (* number of slave sites (master site not included) *)
  val number_of_sites : unit -> int
  (* returns the nth slave site (n < number_of_sites) *)
  val nth_site : int -> site
  val master_site : unit -> site

  val fresh : unit -> gid
  val is_local : gid -> bool

  val to_msg : 'a -> msg
  val from_msg : msg -> 'a

  val send : site -> gid tag -> 'a -> unit
  val send_owner : gid -> gid tag -> 'a -> unit
  val receive : unit -> gid tag * msg
end

module type T = sig
  val fresh_channel : unit -> (int -> unit) * (unit -> int)
end
