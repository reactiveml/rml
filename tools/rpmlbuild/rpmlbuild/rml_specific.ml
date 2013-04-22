open Command;;
open Configuration
open Flags
open Pathname
open My_std
open Rule
open Tags.Operators
open Tools
open Rule.Common_commands
open Rml_version

let rec input_backslash_lines inc s =
  try
    let l = input_line inc in
    if String.get l (String.length l - 1) = '\\' then
      let s = s^(String.sub l 0 (String.length l - 1)) in
      input_backslash_lines inc s
    else
      s^l
  with
    | End_of_file -> s

let split_string s c =
  let rec aux s =
    try
      let id = String.index s c in
      let rest = String.sub s (id + 1) (String.length s - id - 1) in
      String.sub s 0 id :: aux rest
    with Not_found -> [s]
  in
  List.filter (fun s -> String.length s > 0) (aux s)

let remove_spaces s =
  List.hd (split_string s ' ')

let read_depends file =
  let inc = open_in (Pathname.to_string file) in
  try
    let l = input_backslash_lines inc "" in
    close_in inc;
    if l = "" then
      []
    else (
      let i = String.index l ':' in
      let names = split_string (String.sub l (i+1) (String.length l - i - 1)) ' ' in
      List.map (fun s -> [Pathname.basename
                             (Pathname.update_extension "rzi" (Pathname.mk s))]) names
    )
  with
    | _ -> []

let mk_includes dir =
  let add_include acc i =
    if i = Pathname.current_dir_name then acc else A"-I" :: A i :: acc
  in
  List.fold_left add_include [] (Pathname.include_dirs_of dir)

let mpi_backends = ["lco_mpi"; "lco_mpi_c"; "lco_mpi_new"; "lco_mpi_buffer"]
let uses_mpi main_file =
  let rml_file_tags = tags_of_pathname main_file in
  List.exists (fun t -> Tags.mem t rml_file_tags) mpi_backends

let link_rml_core tag ml_extension rml_extension env _build =
  let main_file = env "%.rml" in
  let byte_file = Pathname.update_extension ml_extension main_file in
  let rml_byte_file = env ("%.rml."^rml_extension) in
  tag_file byte_file ["use_rpmllib"];
  if uses_mpi main_file then (
    if not !Options.use_ocamlfind then
      tag_file byte_file ["thread"];
    tag_file byte_file ["use_mlmpi"; "use_rpmllib_mpi"; "with_mpicc"];
  );
  if not !Options.use_ocamlfind then
    tag_file byte_file ["use_unix"];
  tag_file byte_file (Tags.elements (tags_of_pathname rml_byte_file));
  tag_file main_file [tag];
  List.iter Outcome.ignore_good (_build [[byte_file]]);
  ln_s byte_file rml_byte_file
;;

let link_rml extension env _build =
  link_rml_core "simulation_file" extension extension env _build

let link_rml_test extension env _build =
  link_rml_core "test_file" extension ("t."^extension) env _build

let init () =

     Ocaml_utils.ocaml_lib ~extern:true ~native:true ~dir:mlmpilib_dir "mlmpi";
     Ocaml_utils.ocaml_lib ~extern:true ~native:true ~dir:rpmllib_dir "rpmllib";
     Ocaml_utils.ocaml_lib ~extern:true ~native:true ~dir:rpmllib_dir "rpmllib_mpi";

      rule "rmldep: rml -> rmldepends"
        ~prod:"%.rmldepends"
        ~dep:"%.rml"
      begin fun env _build ->
        let file = env "%.rml" in
        let includes = mk_includes (Pathname.dirname file) in
        Cmd(S ([rmldep; A"-I"; A (Pathname.to_string Pathname.pwd)]
               @ includes @ [Px file; Sh ">"; Px(env "%.rmldepends")]))
      end;

      rule "rmldep: mli -> rmldepends"
        ~prod:"%.rmldepends"
        ~dep:"%.mli"
      begin fun env _build ->
        let file = env "%.mli" in
        let includes = mk_includes (Pathname.dirname file) in
        Cmd(S ([rmldep; A"-I"; A (Pathname.to_string Pathname.pwd)]
               @ includes @ [Px file; Sh ">"; Px(env "%.rmldepends")]))
      end;

     rule "rmldep: rmli -> rmldepends"
        ~prod:"%.rmldepends"
        ~dep:"%.rmli"
      begin fun env _build ->
        let file = env "%.rmli" in
        let includes = mk_includes (Pathname.dirname file) in
        Cmd(S ([rmldep; A"-I"; A (Pathname.to_string Pathname.pwd)]
               @ includes @ [Px file; Sh ">"; Px(env "%.rmldepends")]))
      end;

      rule "rml: mli -> rzi"
        ~prods:["%.rzi"]
        ~deps:["%.mli"; "%.rmldepends"]
      begin fun env _build ->
        let dep_file = env "%.rmldepends" in
        let depends = read_depends dep_file in
        if depends <> [] then
          List.iter Outcome.ignore_good (_build depends);

        let file = env "%.mli" in
        let includes = mk_includes (Pathname.dirname file) in
        Cmd(S ([rmlc] @ includes @ [P file]))
      end;

      rule "rml: rmli -> rzi"
        ~prods:["%.rzi"]
        ~deps:["%.rmli"; "%.rmldepends"]
      begin fun env _build ->
        let dep_file = env "%.rmldepends" in
        let depends = read_depends dep_file in
        if depends <> [] then
          List.iter Outcome.ignore_good (_build depends);

        let file = env "%.rmli" in
        let includes = mk_includes (Pathname.dirname file) in
        Cmd(S ([rmlc] @ [A "-c"] @ includes @ [P file]))
      end;

      rule "rml: rml -> ml, rzi"
        ~prods:["%.ml"; "%.rzi"]
        ~deps:["%.rml"; "%.rmldepends"]
      begin fun env _build ->
        let dep_file = env "%.rmldepends" in
        let depends = read_depends dep_file in
        if depends <> [] then
          List.iter Outcome.ignore_good (_build depends);

        let gen_file = env "%.ml" in
        tag_file gen_file ["use_rpmllib"];
        if uses_mpi (env "%.rml") then
          tag_file gen_file ["use_rpmllib_mpi"];

        let file = env "%.rml" in
        let includes = mk_includes (Pathname.dirname file) in
        Cmd(S ([rmlc]@includes@[T (tags_of_pathname file++"rml"++"compile"); P file]))
      end;

      rule "rml: rml -> byte"
        ~prod:"%.rml.byte"
        ~dep:"%.rml"
        (link_rml "byte");

      rule "rml: rml -> d.byte"
        ~prod:"%.rml.d.byte"
        ~dep:"%.rml"
        (link_rml "d.byte");

      rule "rml: rml -> native"
        ~prod:"%.rml.native"
        ~dep:"%.rml"
        (link_rml "native");

      rule "rml: rml -> p.native"
        ~prod:"%.rml.p.native"
        ~dep:"%.rml"
        (link_rml "p.native");

     rule "rml: rml -> t.byte"
        ~prod:"%.rml.t.byte"
        ~dep:"%.rml"
        (link_rml_test "byte");

      rule "rml: rmllib -> mllib"
        ~prod:"%.mllib"
        ~dep:"%.rmllib"
      begin fun env _build ->
        let file = env "%.rmllib" in
        let mllib_file = Pathname.update_extension "mllib" (Pathname.basename file) in
     (*   let cma_file = Pathname.update_extension "cma" file in
        tag_file cma_file ["use_rpmllib"; "use_unix"];
        let cma_file = Pathname.update_extension "d.cma" file in
        tag_file cma_file ["use_rpmllib"; "use_unix"];
        let cmxa_file = Pathname.update_extension "cmxa" file in
        tag_file cmxa_file ["use_rpmllib"; "use_unix"];
        let cmxa_file = Pathname.update_extension "p.cmxa" file in
        tag_file cmxa_file ["use_rpmllib"; "use_unix"]; *)
        cp file mllib_file
      end;
      if !Options.use_ocamlfind then
        flag ["with_mpicc"] (S [A "-thread"; A "-package"; A "threads"; A"-cc"; A mpicc])
      else
        flag ["with_mpicc"] (S [A"-cc"; A mpicc]);
      flag ["rml";"compile";"simulation_file"] (S [A "-s"; A "main"]);
      flag ["rml";"compile";"test_file"] (S [A "-t"; A "test"]);

      flag ["rml"; "compile"; "annot"] (A "-dtypes");
      flag ["rml"; "compile"; "lco"] (S ([A "-runtime"; A "Lco"]));
      flag ["rml"; "compile"; "lco_new"] (S ([A "-runtime"; A "Lco_new"]));
      flag ["rml"; "compile"; "lco_mpi"] (S [A "-runtime"; A "Lco_mpi"]);
      flag ["rml"; "compile"; "lco_mpi_buffer"] (S [A "-runtime"; A "Lco_mpi_buffer"]);
      flag ["rml"; "compile"; "lco_mpi_c"] (S [A "-runtime"; A "Lco_mpi_c"]);
      flag ["rml"; "compile"; "lco_mpi_new"] (S [A "-runtime"; A "Lco_mpi_new"]);
      flag ["rml"; "compile"; "no_clocking"] (A "-no-clocking");
      flag ["rml"; "compile"; "no_reactivity"] (A "-no-reactivity");
      flag ["rml"; "compile"; "row_clocking"] (A "-row-clocking")

;;


let rml_lib ?(byte=true) ?(native=true) ?dir ?tag_name libpath =
  let add_dir x =
    match dir with
    | Some dir -> S[A"-I"; P dir; x]
    | None -> x
  in
  let tag_name =
    match tag_name with
    | Some x -> x
    | None -> "use_" ^ Pathname.basename libpath
  in
  let flag_and_dep tags lib =
    flag tags (add_dir (A lib))
  in
  if byte then
    flag_and_dep ["ocaml"; tag_name; "link"; "byte"] (libpath^".cma");
  if native then
    flag_and_dep ["ocaml"; tag_name; "link"; "native"] (libpath^".cmxa");
  match dir with
  | None -> ()
  | Some dir ->
    flag ["rml"; "compile"; tag_name] (S[A"-I"; P dir]);
    flag ["ocaml"; "compile"; tag_name] (S[A"-I"; P dir])

