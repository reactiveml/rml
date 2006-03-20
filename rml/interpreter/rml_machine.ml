(**********************************************************************)
(*                        ReactiveML                                  *)
(*                                                                    *)
(* Auteur : Louis Mandel                                              *) 
(* Date de creation : 13/09/2005                                      *)
(* Fichier : rml_machine.ml                                           *)
(*                                                                    *)
(**********************************************************************)


module type Interpretor_type =
  sig
    type 'a process 
    val rml_make: 'a process -> (unit -> 'a option)
  end

module M =
  functor (Interpretor: Interpretor_type) ->
  struct

    let rml_exec p =
      let react = Interpretor.rml_make p in
      let rec exec () =
	match react () with
	| None -> exec()
	| Some v -> v
      in exec ()
    
    let rml_exec_n p n =
      let react = Interpretor.rml_make p in
      let rec exec n =
	if n > 0 then
	  match react () with
	  | None -> exec (n-1)
	  | v -> v
	else
	  None
      in exec n
 
    let rml_exec_sampling p min =
      let _ = Sys.signal Sys.sigalrm (Sys.Signal_handle (fun x -> ())) in
      let debut = ref 0.0 in
      let fin = ref 0.0 in
      let diff = ref 0.0 in
      let react = Interpretor.rml_make p in
      let rec exec () =
	let _ = debut := Sys.time() in
	let v = react () in
	let _ = 
	  fin := Sys.time();
	  diff := min -. (!fin -. !debut);
	  if !diff > 0.001 then (
	    ignore (Unix.setitimer 
		      Unix.ITIMER_REAL 
		      {Unix.it_interval = 0.0; Unix.it_value = !diff});
	    Unix.pause())  
	  else ();
	in
	match v with
	| None -> exec ()
	| Some v -> v
      in exec ()

   
    let rml_exec_n_sampling p n min =
      let _ = Sys.signal Sys.sigalrm (Sys.Signal_handle (fun x -> ())) in
      let debut = ref 0.0 in
      let fin = ref 0.0 in
      let diff = ref 0.0 in
      let instant = ref 0 in
      let react = Interpretor.rml_make p in
      let rec exec n =
	if n > 0 then
	  let _ =
	    print_string ("************ Instant "^
			  (string_of_int !instant)^
			  " ************");
	    print_newline();
	    debut := Sys.time();
	    incr instant
	  in
	  let _ = debut := Sys.time() in
	  let v = react () in
	  let _ = 
	    fin := Sys.time();
	    diff := min -. (!fin -. !debut);
	    if !diff > 0.001 then (
	      ignore (Unix.setitimer 
			Unix.ITIMER_REAL 
			{Unix.it_interval = 0.0; Unix.it_value = !diff});
	      Unix.pause())  
	    else 
	      (print_string "Instant ";
	       print_int !instant;
	       print_string " : depassement = ";
	       print_float (-. !diff);
	       print_newline());
	  in
	  match v with
	  | None -> exec (n-1)
	  | v -> v
	else
	  None
      in exec n


  end
    
