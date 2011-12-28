
module type S = sig
  type msg
  type site

  type lid
  type gid

  module GidMap : (Map.S with type key = gid)
  module GidSet : (Set.S with type elt = gid)

  module SiteMap : (Map.S with type key = site)
  module SiteSet : (Set.S with type elt = site)

  val available_site : unit -> site

  val fresh : unit -> gid
  val is_local : gid -> bool

  val to_msg : 'a -> msg
  val from_msg : msg -> 'a

  val send : site -> 'tag -> 'a -> unit
  val send_owner : gid -> 'tag -> 'a -> unit
  val receive : unit -> 'tag * msg
end
