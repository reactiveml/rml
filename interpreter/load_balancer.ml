
module type S = sig
  type site
  type state

  val mk_top_balancer : unit -> state
  (* [new_child s] returns the site for the new child and its state. *)
  val new_child : state -> site * state
end

module Local (C : Communication.S) = struct
  type site = C.site
  type state = C.site

  let mk_top_balancer () =
    C.master_site ()

  let new_child s = s, s
end

module RoundRobin (C : Communication.S) = struct
  type site = C .site
  type state = {
    s_sites : site array;
    mutable s_next_site : int;
    s_here : site;
  }

  let mk_state here sites = {
    s_sites = sites;
    s_next_site = 0;
    s_here = here;
  }

  let mk_top_balancer () =
    let a = Array.init (C.number_of_sites ()) (fun i -> C.nth_site i) in
    mk_state (C.master_site ()) a

  let new_child s =
    let new_state = mk_state (s.s_sites.(s.s_next_site)) (Array.copy s.s_sites) in
    s.s_next_site <- (s.s_next_site + 1) mod (Array.length s.s_sites);
    new_state.s_here, new_state
end
