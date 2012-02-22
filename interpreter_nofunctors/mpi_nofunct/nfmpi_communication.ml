
open Runtime_options


let msg_tag = 0

(*module Make (P : Communication.TAG_TYPE) = struct *)
  type msg = string

  type lid = int (* local unique id *)
  type gid =
      { g_rank : Mpi.rank; g_id : lid } (* global unique id *)

      type 'gid tag =
          | Mfinalize
          | Mdummy
          (* scheduling *)
          | Mnew_cd
          | Mcd_created of 'gid
          | Mstep (* do one global step of the clock domain *)
          | Mstep_done of 'gid (* a clock domain has finished its step *)
          | Mdone of 'gid (* global step done *)
          | Mpauseclock of 'gid
          | Mbefore_eoi (* Before the end of instant of clock domain *)
          | Meoi (* End of instant of clock domain *)
          | Meoi_control of 'gid
          | Mnext_instant (* Go to next instant *)
          | Mreq_has_next of 'gid
          | Mhas_next of 'gid
              (* Whether there is processes to execute in the next local step *)
          | Mnew_remote of 'gid
              (* signals *)
          | Memit of 'gid (* Emit a value *)
          | Mvalue of 'gid (* Send the value of the signal to child cd *)
          | Msignal of 'gid (* Send the whole signal *)
          | Mreq_signal of 'gid (* Request the value of the signal *)
          | Mcreate_signal
          | Msignal_created of 'gid

      let flush_after tag = match tag with
         | Mfinalize | Mdummy | Mhas_next _ | Mvalue _ | Mnew_cd | Mcd_created _
         | Mreq_signal _ | Msignal_created _ | Mcreate_signal -> true
         | _ -> false

      open Format
      let print print_gid ff m = match m with
        | Mdummy -> fprintf ff "Mdummy"
        | Mfinalize -> fprintf ff "Mfinalize"
        | Mnew_cd -> fprintf ff "Mnew_cd"
        | Mcd_created gid -> fprintf ff "Mcd_created %a" print_gid gid
        | Mstep -> fprintf ff "Mstep"
        | Mstep_done gid -> fprintf ff "Mstep_done %a" print_gid gid
        | Mdone gid -> fprintf ff "Mdone %a" print_gid gid
        | Mpauseclock gid -> fprintf ff "Mpauseclock %a" print_gid gid
        | Meoi -> fprintf ff "Mbefore_eoi"
        | Mbefore_eoi -> fprintf ff "Meoi"
        | Meoi_control gid -> fprintf ff "Meoi_control %a" print_gid gid
        | Mnext_instant -> fprintf ff "Mnext_instant"
        | Mreq_has_next gid -> fprintf ff "Mreq_has_next %a" print_gid gid
        | Mhas_next gid -> fprintf ff "Mhas_next %a" print_gid gid
        | Mnew_remote gid -> fprintf ff "Mnew_remote %a" print_gid gid
            (* Whether there is processes to execute in the next local step *)
        (* signals *)
        | Memit gid -> fprintf ff "Memit %a" print_gid gid
        | Mvalue gid -> fprintf ff "Mvalue %a" print_gid gid
        | Msignal gid -> fprintf ff "Msignal %a" print_gid gid
        | Mreq_signal gid -> fprintf ff "Mreq_signal %a" print_gid gid
        | Mcreate_signal -> fprintf ff "Mcreate_signal"
        | Msignal_created gid -> fprintf ff "Msignal_created %a" print_gid gid

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

  let print_tag = print print_gid

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

  let is_local gid =
    gid.g_rank = local_site ()

  let to_msg d =
    Marshal.to_string d [Marshal.Closures]

  let from_msg s =
    Marshal.from_string s 0

  let send_owner gid tag d =
    let s = to_msg d in
    print_debug "%a: Send '%a' to gid '%a' at site '%d', with %d bytes@."
      print_here ()  print_tag tag  print_gid gid  gid.g_rank  (Marshal.total_size s 0);
    Mpi.send (tag, s) gid.g_rank msg_tag

  let send site tag d =
    let s = to_msg d in
    print_debug "%a: Send '%a' to site '%a', with %d bytes@."
      print_here ()  print_tag tag  print_site site  (Marshal.total_size s 0);
    Mpi.send (tag, s) site msg_tag

  let broadcast tag d =
    SiteSet.iter (fun s -> if s <> local_site () then send s tag d) (all_sites ())

  let broadcast_set s tag d =
    SiteSet.iter (fun site -> send site tag d) s

  let receive () =
    let tag, (msg:msg) = Mpi.receive Mpi.any_source msg_tag in
      print_debug "%a: Received '%a'@."
        print_here ()  print_tag tag;
      tag, msg

  let flush () = ()
(*end *)

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

