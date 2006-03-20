(*-----------------------------------------------------------------------
** Copyright (C) - Verimag.
** This file may only be copied under the terms of the GNU Library General
** Public License
**-----------------------------------------------------------------------
**
** File: lucaml.ml
** Author: jahier@imag.fr
*)

(* XXX Should be set properly *)
let _ =
  Unix.putenv "VERSION" "";
  Unix.putenv "LURETTE_PATH" "";
  Unix.putenv "PIXMAP_DIR" "";
  Unix.putenv "PDF_VIEWER" "acroread";
  Unix.putenv "DOT" "dot";
  Unix.putenv "LUS2EC" "lus2ec";
  Unix.putenv "EC2C" "ec2c";
  Unix.putenv "SIM2CHRO" "sim2chro";
  Unix.putenv "HOST_TYPE" "";
  Unix.putenv "GNUPLOTRIF" "gnuplot-rif";
  Unix.putenv "AWK" "gawk";
  Unix.putenv "PLOT" "plot";
  Unix.putenv "SCADE2LUSTRE" "scade2lustre";
  Unix.putenv "SCADE_CG" "scade_cg";
  Unix.putenv "LUSTRE2C" "lustre2c";
  Unix.putenv "SCADE_INSTALL_DIR" "";
  Unix.putenv "SCADE_COMPIL_OPTION" " -noexp @ALL@ "



open Env_state

type state = Env_state.t

type value = F of float | I of int | B of bool

type var_name = string
type subst = var_name * value

type inputs = subst list
(* (var_name, value) Hashtbl.t (* XXX hashtbl ou list ??? *) *)
type outputs = subst list
type locals = subst list

type solution = outputs * locals

(** To indicate whether the point used to perform the step is
  drawn inside, at edges, or at vertices of the convex hull of
  solutions; the step mode is used iff at least one controllable
  variable is numeric.  
*)
type step_mode = StepInside | StepEdges | StepVertices


  

(* I defined mine because i need to know the seed that has been drawn by self_init. *)
let random_seed () =
  let () = Random.self_init () in
    Random.int max_int

let make ?(seed = (random_seed ())) ?(fair = false) ?(pp = "") 
  ?(verbose = false) files =

  let _ = Formula_to_bdd.clear_all () in
  let pp_opt = if pp = "" then None else Some pp in
    
  (* Initialisation of `Env_state.env_state' *)
  let automata_l = List.map (Parse_luc.parse_automata pp_opt) files in
  let state0 = Env_state.read_env_state false automata_l in
  let init_state_dyn =
    {
      memory = state0.d.memory;
      current_nodes = state0.d.current_nodes;
      input = state0.d.input;
      verbose = verbose
    }
  in
  let init_state = {
    d = init_state_dyn ;
    s = state0.s
  }
  in

  (* Initialisation of the random engine *)
    Random.init seed ;
    output_string stderr "#The random engine was initialized with the seed ";
    output_string stderr (string_of_int seed);
    output_string stderr  "\n";
    flush stderr ;

    (* selecting the draw mode *)
    if fair then Solver.set_fair_mode () else Solver.set_efficient_mode ();

    (* Initializing the solution number table *)
    !Solver.init_snt ();

    init_state
    

let step_mode_to_lucky_step_mode = function
    StepInside  -> Lucky.StepInside
  | StepEdges  -> Lucky.StepEdges
  | StepVertices  -> Lucky.StepVertices   



let value2num = function
    B(x) -> Value.B(x)
  | I(x) -> Value.N(Value.I(x))
  | F(x) -> Value.N(Value.F(x))

let (num2value : Value.t -> value) = 
  fun v -> 
    match v with
	Value.B(x) -> B(x)
      | Value.N(Value.I(x)) -> I(x)
      | Value.N(Value.F(x)) -> F(x)


let string_of_value = function
    B(b) -> if b then "t" else "f"
  | I(i) -> string_of_int i
  | F(f) -> string_of_float f


let (subst_convert : Var.subst -> subst) = 
  fun (vn,v) -> 
    (vn, num2value v)


let (convert_inputs : subst list -> Var.env_in) =
  fun l -> 
    let h = Hashtbl.create (List.length l) in
      List.iter (fun (vn, x) -> Hashtbl.add h vn (value2num x)) l;
      h

let (convert_inputs_back : Var.env_in -> subst list) =
  fun h -> 
    Hashtbl.fold (fun name value acc -> (subst_convert (name,value))::acc) h []

let step ?(mode = StepInside) state inputs =
  let (st, (outs, locs)) = 
    Lucky.env_step 
      (step_mode_to_lucky_step_mode mode) 
      (convert_inputs inputs) 
      state
  in
    (st, (List.map subst_convert outs, List.map subst_convert locs))

let step_se ?(mode = StepInside) state_ref inputs =
  let (nst,sol) = step ~mode:mode !state_ref inputs in
    state_ref := nst;
    sol

(***********************************************************************)

let (rif_read : in_channel -> state -> inputs * state) = 
  fun ic st -> 
  let (i,st') = Rif.read ic st in
    (convert_inputs_back i, st')


