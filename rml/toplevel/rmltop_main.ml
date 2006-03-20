(**********************************************************************)
(*                        ReactiveML                                  *)
(*                                                                    *)
(* Auteur : Louis Mandel                                              *) 
(* Date de creation : 25/10/2005                                      *)
(* Fichier : rmltop_directives.ml                                     *)
(*                                                                    *)
(**********************************************************************)

let exec_machine_controler () =
  let _ = Sys.signal Sys.sigalrm (Sys.Signal_handle (fun x -> ())) in
  let debut = ref 0.0 in
  let fin = ref 0.0 in
  let sleep = ref 0.0 in
  let react = 
    Rmltop_implantation.Machine_controler_machine.rml_make 
      Rmltop_machine_controler.main 
  in
  while true do
    let _ = debut := Sys.time() in
    let _ = react () in
    let _ = 
      fin := Sys.time();
      sleep := !Rmltop_global.sampling -. (!fin -. !debut);
      if !sleep > 0.001 then (
	ignore (Unix.setitimer 
		  Unix.ITIMER_REAL 
		  {Unix.it_interval = 0.0; Unix.it_value = !sleep});
	Unix.pause())  
      else ();
    in ()
  done


let start () =
  Thread.create exec_machine_controler ()

let machine = start ()
