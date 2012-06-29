
open Runtime_options


let msg_tag = 0

module Make (P : Communication.TAG_TYPE) = struct
  type msg = string

  type lid = int (* local unique id *)
  type gid =
      { g_rank : Mpi.rank; g_id : lid } (* global unique id *)
  type 'gid tag = 'gid P.t

  let dummy_tag = P.dummy

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

  let all_sites () =
   let s = ref SiteSet.empty in
   for i = !Runtime_options.min_rank + 1 to Mpi.communicator_size - 1 do
     s:= SiteSet.add (i:Mpi.rank) !s
   done;
   !s

  type seed = int ref

  let mk_seed () = ref 0
  let fresh counter =
    incr counter;
    { g_rank = local_site ();
      g_id = !counter }

  let relocate_gid gid site =
    { gid with g_rank = site }

  let is_local gid =
    gid.g_rank = local_site ()

  let to_msg d =
    Marshal.to_string d [Marshal.Closures]

  let from_msg s =
    Marshal.from_string s 0


  type send_buffer = ((gid tag * msg) list) SiteMap.t
  type recv_buffer = (gid tag * msg) list

  let get_send_buffer () =
    (Local_ref.get 1:send_buffer ref)
  let get_recv_buffer () =
    (Local_ref.get 2:recv_buffer ref)

  let _ =
    Local_ref.init 1 (ref SiteMap.empty);
    Local_ref.init 2 (ref [])

  let do_send site (tag:gid tag) (msg:msg) =
    let buffer = get_send_buffer () in
    let msgs =
      if SiteMap.mem site !buffer then
        SiteMap.find site !buffer
      else
        []
    in
    let msgs = (tag, msg)::msgs in
    if P.flush_after tag then (
      (* really send the values *)
      Mpi.send (List.rev msgs) site msg_tag;
      buffer:= SiteMap.remove site !buffer
    ) else
      buffer := SiteMap.add site msgs !buffer

  let flush () =
    print_debug "Flushing all messages@.";
    let buffer = get_send_buffer () in
    SiteMap.iter (fun site msgs -> print_debug "Flusing %d messages@." (List.length msgs); Mpi.send (List.rev msgs) site msg_tag) !buffer;
    buffer := SiteMap.empty

  let send_owner gid tag d =
    print_debug "%a: Send '%a' to gid '%a' at site '%d'@."
      print_here ()  print_tag tag  print_gid gid  gid.g_rank;
    do_send gid.g_rank tag (to_msg d)

  let send site tag d =
    print_debug "%a: Send '%a' to site '%a'@."
      print_here ()  print_tag tag  print_site site;
    do_send site tag (to_msg d)

  let broadcast tag d =
    SiteSet.iter (fun s -> if s <> local_site () then send s tag d) (all_sites ())

  let broadcast_set s tag d =
    SiteSet.iter (fun site -> do_send site tag (to_msg d)) s

  let receive () =
    let buffer = get_recv_buffer () in
    let tag, msg, msgs =
      match !buffer with
        | [] ->
            let msgs:(gid tag*msg) list = Mpi.receive Mpi.any_source msg_tag in
            (match msgs with
              | [] -> print_debug "Received empty list@."; assert false
              | (tag, msg)::msgs -> tag, msg, msgs)
        | (tag, msg)::msgs -> tag, msg, msgs
    in
    print_debug "%a: Received '%a'@." print_here ()  print_tag tag;
    buffer := msgs;
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

