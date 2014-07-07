open Command;;
open Configuration
open Flags
open Pathname
open My_std
open Rule
open Tags.Operators
open Tools
open Rule.Common_commands

let rmlc = A"rmlc";;
let rmldep = A "rmldep";;
let stdlib_dir = ref ""

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
      split_string (String.sub l (i+1) (String.length l - i - 1)) ' '
    )
  with
    | _ -> []

let import_depends build dep_file file =
  let names = read_depends dep_file in
  let include_dirs = Pathname.include_dirs_of (Pathname.dirname file) in
  let files_alternatives =
    List.map begin fun module_name ->
      Ocaml_utils.expand_module include_dirs module_name ["rzi"]
    end names
  in
  ignore (build files_alternatives)

let read_rmlsim_lines inc =
  let rec read_rmlsim_line inc cmds =
    try
      let l = input_line inc in
      let i = String.index l ':' in
      let cmd = String.sub l 0 i in
      let value = remove_spaces (String.sub l (i+1) (String.length l - i - 1)) in
      read_rmlsim_line inc ((cmd, value) :: cmds)
    with
      | _ -> cmds
  in
  read_rmlsim_line inc []


let read_rmlsim file filename =
  let inc = open_in (Pathname.to_string file) in
  let cmds = read_rmlsim_lines inc in
  close_in inc;
  tag_file filename ["simulation_file"];
  let specs =
    if List.mem_assoc "sim" cmds then
      [A "-s"; A (List.assoc "sim" cmds)]
    else
      []
  in
  let specs =
    if List.mem_assoc "sampling" cmds then
      specs@[A "-sampling"; A (List.assoc "sampling" cmds)]
    else
      specs
  in
  let specs =
    if List.mem_assoc "n" cmds then
      specs@[A "-n"; A (List.assoc "n" cmds)]
    else
      specs
  in
  flag ["rml";"compile";"simulation_file"] (S specs)

let mk_includes dir =
  let add_include acc i =
    if i = Pathname.current_dir_name then acc else A"-I" :: A i :: acc
  in
  List.fold_left add_include [] (Pathname.include_dirs_of dir)

let find_rml_stdlib_dir () =
  let path = "rmlc.where" in
    if not (Pathname.exists path) then
      (let cmd = Cmd (S [rmlc; T (tags_of_pathname path++"rml"++"where");
                         A "-where"; Sh ">"; Px path]) in
         Command.execute cmd);
    let s = My_std.read_file (Pathname.to_string path) in
      stdlib_dir := String.sub s 0 (String.length s - 1)

let declare_rmllib () =
  if !stdlib_dir = "" then (
    find_rml_stdlib_dir ();
    Ocaml_utils.ocaml_lib ~extern:true ~native:true ~dir:!stdlib_dir "rmllib"
  )

let link_rmlsim extension env _build =
  let rmlsim_file = env "%.rmlsim" in
  let main_file = env "%.rml" in
  let () = read_rmlsim rmlsim_file main_file in
  let byte_file = Pathname.update_extension extension main_file in
  let rml_byte_file = env ("%.rml."^extension) in
  declare_rmllib ();
  tag_file byte_file ["use_rmllib"; "use_unix"];
  tag_file byte_file (Tags.elements (tags_of_pathname rml_byte_file));
  List.iter Outcome.ignore_good (_build [[byte_file]]);
  mv byte_file rml_byte_file

let link_rml extension env _build =
  let main_file = env "%.rml" in
  let byte_file = Pathname.update_extension extension main_file in
  let rml_byte_file = env ("%.rml."^extension) in
  declare_rmllib ();
  tag_file byte_file ["use_rmllib"; "use_unix"];
  tag_file byte_file (Tags.elements (tags_of_pathname rml_byte_file));
  List.iter Outcome.ignore_good (_build [[byte_file]]);
  mv byte_file rml_byte_file
;;

let init () =

      rule "rmldep: mli -> rmldepends"
        ~prod:"%.rmli.depends"
        ~dep:"%.mli"
      begin fun env _build ->
        let file = env "%.mli" in
        Cmd(S ([rmldep; A "-modules"; T (tags_of_pathname file++"rml"++"depend");
                  Px file; Sh ">"; Px(env "%.rmli.depends")]))
      end;

     rule "rmldep: rmli -> rmldepends"
        ~prod:"%.rmli.depends"
        ~dep:"%.rmli"
      begin fun env _build ->
        let file = env "%.rmli" in
        Cmd(S ([rmldep; A "-modules"; T (tags_of_pathname file++"rml"++"depend");
               Px file; Sh ">"; Px(env "%.rmli.depends")]))
      end;

      rule "rmldep: rml -> rmldepends"
        ~prod:"%.rml.depends"
        ~dep:"%.rml"
      begin fun env _build ->
        let file = env "%.rml" in
        Cmd(S ([rmldep; A "-modules"; T (tags_of_pathname file++"rml"++"depend");
                  Px file; Sh ">"; Px(env "%.rml.depends")]))
      end;

      rule "rml: mli -> rzi"
        ~prods:["%.rzi"]
        ~deps:["%.mli"; "%.rmli.depends"]
      begin fun env _build ->
        let file = env "%.mli" in
        import_depends _build (env "%.rmli.depends") file;
        let includes = mk_includes (Pathname.dirname file) in
        Cmd(S ([rmlc] @ includes @ [T (tags_of_pathname file++"rml"++"compile"); P file]))
      end;

      rule "rml: rmli -> rzi"
        ~prods:["%.rzi"]
        ~deps:["%.rmli"; "%.rmli.depends"]
      begin fun env _build ->
        let file = env "%.rmli" in
        import_depends _build (env "%.rmli.depends") file;
        let includes = mk_includes (Pathname.dirname file) in
        Cmd(S ([rmlc] @ [A "-c"] @ includes
               @ [T (tags_of_pathname file++"rml"++"compile"); P file]))
      end;

      rule "rml: rml & rzi -> ml"
        ~prod:"%.ml"
        ~deps:["%.rmli"; "%.rml"; "%.rml.depends"; "%.rzi"]
      begin fun env _build ->
        let file = env "%.rml" in
        import_depends _build (env "%.rml.depends") file;

        declare_rmllib () ;
        let gen_file = env "%.ml" in
        tag_file gen_file ["use_rmllib"];

        let includes = mk_includes (Pathname.dirname file) in
        Cmd(S ([rmlc]@includes@[T (tags_of_pathname file++"rml"++"compile"); P file]))
      end;

      rule "rml: rml -> ml, rzi"
        ~prods:["%.ml"; "%.rzi"]
        ~deps:["%.rml"; "%.rml.depends"]
      begin fun env _build ->
        let file = env "%.rml" in
        let gen_file = env "%.ml" in
        import_depends _build (env "%.rml.depends") file;

        declare_rmllib () ;
        tag_file gen_file ["use_rmllib"];

        let file = env "%.rml" in
        let includes = mk_includes (Pathname.dirname file) in
        Cmd(S ([rmlc]@includes@[T (tags_of_pathname file++"rml"++"compile"); P file]))
      end;

      rule "rml: rmlsim -> byte"
        ~prod:"%.rml.byte"
        ~dep:"%.rmlsim"
        (link_rmlsim "byte");

      rule "rml: rmlsim -> d.byte"
        ~prod:"%.rml.d.byte"
        ~dep:"%.rmlsim"
        (link_rmlsim "d.byte");

      rule "rml: rmlsim -> native"
        ~prod:"%.rml.native"
        ~dep:"%.rmlsim"
        (link_rmlsim "native");

      rule "rml: rmlsim -> p.native"
        ~prod:"%.rml.p.native"
        ~dep:"%.rmlsim"
        (link_rmlsim "p.native");


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

      rule "rml: rmllib -> mllib"
        ~prod:"%.mllib"
        ~dep:"%.rmllib"
      begin fun env _build ->
        declare_rmllib ();
        let file = env "%.rmllib" in
        let mllib_file = Pathname.update_extension "mllib" (Pathname.basename file) in
        let cma_file = Pathname.update_extension "cma" file in
        tag_file cma_file [(* "use_rmllib"; "use_unix" *)];
        let cma_file = Pathname.update_extension "d.cma" file in
        tag_file cma_file [(* "use_rmllib"; "use_unix" *)];
        let cmxa_file = Pathname.update_extension "cmxa" file in
        tag_file cmxa_file [(* "use_rmllib"; "use_unix" *)];
        let cmxa_file = Pathname.update_extension "p.cmxa" file in
        tag_file cmxa_file [(* "use_rmllib"; "use_unix" *)];
        cp file mllib_file
      end;

      flag ["rml"; "compile"; "annot"] (A "-dtypes");
      flag ["rml"; "compile"; "thread"] (A "-thread");

      flag ["rml"; "compile"; "lco_rewrite"] (S ([A "-runtime"; A "Lco_rewrite"]));
      flag ["rml"; "where"; "lco_rewrite"] (S ([A "-runtime"; A "Lco_rewrite"]));
      flag ["rml"; "compile"; "lco"] (S ([A "-runtime"; A "Lco"]));
      flag ["rml"; "where"; "lco"] (S ([A "-runtime"; A "Lco"]));
      flag ["rml"; "compile"; "lco_n"] (S ([A "-runtime"; A "Lco_n"]));
      flag ["rml"; "where"; "lco_n"] (S ([A "-runtime"; A "Lco_n"]));
      flag ["rml"; "compile"; "lco_class"] (S ([A "-runtime"; A "Lco_class"]));
      flag ["rml"; "where"; "lco_class"] (S ([A "-runtime"; A "Lco_class"]));
      flag ["rml"; "compile"; "lk"] (S ([A "-runtime"; A "Lk"]));
      flag ["rml"; "where"; "lk"] (S ([A "-runtime"; A "Lk"]));
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
