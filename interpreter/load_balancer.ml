
module type S = sig
  type site
  type kind = Lany | Lleaf

  class virtual load_balancer :
  object
    method virtual new_child : (int -> int * int) option -> site * kind * load_balancer
  end

  val mk_top_balancer : unit -> load_balancer
end

module Make (C : Communication.S) = struct
  type site = C.site
  type kind = Lany | Lleaf

  class virtual load_balancer  =
  object
    method virtual new_child : (int -> int * int) option -> C.site * kind * load_balancer
  end

  class local_balancer site =
  object(self)
    inherit load_balancer

    val site = site
    method new_child _ =
      site, Lleaf, ({< >} :> load_balancer)
  end

  module IntMap = Map.Make(struct
    type t = int
    let compare = compare
  end)

  class virtual generic_balancer here sites =
  object(self)
    inherit load_balancer

    val nb_sites = C.SiteSet.cardinal !sites
    val unused_sites = sites
    val mutable child_sites = IntMap.empty

    (* Tries to use site number i to execute a child. If it is not available, use
       another unused site. Returns the used site and updates the state of the object*)
    method add_child_site i =
      let rec aux i =
        if i > nb_sites then
          aux (i mod (nb_sites + 1))
        else if i = 0 then
          here
        else if IntMap.mem i child_sites then
          IntMap.find i child_sites
        else
          let s =
            if C.SiteSet.is_empty !unused_sites then
              aux ((i+1) mod (nb_sites + 1))
            else
              C.SiteSet.choose !unused_sites
          in
          child_sites <- IntMap.add i s child_sites;
          unused_sites := C.SiteSet.remove s !unused_sites;
          s
      in
      aux i

    method take_grand_child_sites n =
      let rec take n s acc = match n with
        | 0 -> s, acc
        | _ ->
            let elt = C.SiteSet.choose s in
            take (n-1) (C.SiteSet.remove elt s) (C.SiteSet.add elt acc)
      in
      let n = min (C.SiteSet.cardinal !unused_sites) (max 0 n) in
      let unused, used = take n !unused_sites C.SiteSet.empty in
      unused_sites := unused;
      used

    method follow_annotation f =
      let i, nb_req_sites = f nb_sites in
      let site = self#add_child_site i in
      let k = if nb_req_sites = 0 then Lleaf else Lany in
      let sites = ref (self#take_grand_child_sites nb_req_sites) in
      (*Format.eprintf "Creating clock domain at %a with idx %d at site %a with %d req_sites and %d provided@."
        C.print_site here  i  C.print_site site  nb_req_sites (C.SiteSet.cardinal !sites);*)
      site, k, sites
  end

  class local_user_balancer here sites =
  object(self)
    inherit generic_balancer here sites

    method new_child f =
      match f with
        | Some f ->
            let site, k, sites = self#follow_annotation f in
            site, k, (new local_user_balancer site sites :> load_balancer)
        | _ ->
            let k = if C.SiteSet.is_empty !unused_sites then Lleaf else Lany in
            here, k, (new local_user_balancer here unused_sites :> load_balancer)
  end


  class robin_balancer ignore_annotations start_index here sites =
  object(self)
    inherit generic_balancer here sites

    val mutable current_index = start_index

    method new_child f =
      match f, ignore_annotations with
        | Some f, false ->
            let site, k, sites = self#follow_annotation f in
            site, k, (new robin_balancer ignore_annotations start_index site sites :> load_balancer)
        | _ ->
            let s, k, bal =
              if current_index = 0 then
                let k = if C.SiteSet.is_empty !sites then Lleaf else Lany in
                here, k, new robin_balancer ignore_annotations start_index here unused_sites
              else
                let site = self#add_child_site current_index in
                site, Lleaf, new robin_balancer ignore_annotations start_index site (ref C.SiteSet.empty)
            in
            current_index <- (current_index + 1) mod (nb_sites + 1);
            s, k, (bal :> load_balancer)
  end

  let mk_top_balancer () =
    let all_sites = ref (C.all_sites ()) in
    let master = C.master_site () in
    match !Runtime_options.load_balancing_policy with
      | Runtime_options.Plocal -> (new local_balancer master:> load_balancer)
      | Runtime_options.Puser_local -> (new local_user_balancer master all_sites :> load_balancer)
      | Runtime_options.Pround_robin -> (new robin_balancer true 0 master all_sites :> load_balancer)
      | Runtime_options.Puser_robin -> (new robin_balancer false 0 master all_sites :> load_balancer)
      | Runtime_options.Premote -> (new robin_balancer true 1 master all_sites :> load_balancer)
end
