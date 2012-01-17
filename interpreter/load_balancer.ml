
module type S = sig
  type site

  class virtual load_balancer :
  object
    method virtual new_child : unit -> site * load_balancer
  end

  val mk_top_balancer : unit -> load_balancer
end

type policy = Plocal | Pround_robin | Pall_remote
let load_balancing_policy = ref Pround_robin

let set_load_balancing_policy s =
  let p =
    match s with
      | "local" -> Plocal
      | "robin" -> Pround_robin
      | "all_remote" -> Pall_remote
      | _ -> raise (Arg.Bad ("Invalid load balancing policy"))
  in
  load_balancing_policy := p

module Make (C : Communication.S) = struct
  type site = C.site

  class virtual load_balancer  =
  object
    method virtual new_child : unit -> C.site * load_balancer
  end

  class local_balancer site =
  object(self)
    inherit load_balancer

    val site = site
    method new_child () =
      site, ({< >} :> load_balancer)
  end

  class round_robin_balancer here sites =
  object(self)
    inherit load_balancer

    val s_sites = sites
    val s_here = here
    val mutable s_next_site = Array.length sites
    method new_child () =
      let new_site =
        if s_next_site = Array.length s_sites then (
          s_next_site <- -1;
          s_here
        ) else
          s_sites.(s_next_site)
      in
      s_next_site <- s_next_site + 1;
      let new_balancer = {< s_here = new_site;
                            s_sites = Array.copy s_sites;
                            s_next_site = Array.length s_sites >} in
      new_site, (new_balancer :> load_balancer)
  end

  class all_remote_balancer sites =
  object(self)
    inherit load_balancer

    val s_sites = sites
    val mutable s_next_site = 0
    method new_child () =
      let new_site = s_sites.(s_next_site) in
      s_next_site <- s_next_site + 1;
      if s_next_site = Array.length s_sites then
        s_next_site <- 0;
      let new_balancer = {< s_sites = Array.copy s_sites;
                            s_next_site = 0 >} in
      new_site, (new_balancer :> load_balancer)
  end


  let mk_top_balancer () =
    match !load_balancing_policy with
      | Plocal -> new local_balancer (C.local_site ())
      | Pround_robin ->
          let a = Array.init (C.number_of_sites ()) (fun i -> C.nth_site i) in
          new round_robin_balancer (C.master_site ()) a
      | Pall_remote ->
          if C.number_of_sites () = 0 then (
            new local_balancer (C.local_site ())
          ) else (
            let a = Array.init (C.number_of_sites ()) (fun i -> C.nth_site i) in
            new all_remote_balancer a
          )
end
