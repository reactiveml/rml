open Ocamlbuild_plugin;;
open Command;;

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
      let names = split_string (String.sub l (i+1) (String.length l - i - 1)) ' ' in
      List.map (fun s -> [Pathname.basename
                             (Pathname.update_extension "rzi" (Pathname.mk s))]) names
    )
  with
    | _ -> []

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


let read_rmlsim file =
  let inc = open_in (Pathname.to_string file) in
  let cmds = read_rmlsim_lines inc in
  close_in inc;
  let filename = Pathname.mk (List.assoc "file" cmds) in
  tag_file filename ["simulation_file"];
  let specs = [A "-s"; A (List.assoc "sim" cmds)] in
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
  flag ["rml";"compile";"simulation_file"] (S specs);
  filename

let mk_includes dir =
  let add_include acc i =
    if i = Pathname.current_dir_name then acc else A"-I" :: A i :: acc
  in
  List.fold_left add_include [] (Pathname.include_dirs_of dir)

let find_rml_stdlib_dir () =
  let path = !Options.build_dir / "rmlc.where" in
  if not (Pathname.exists path) then
    (let cmd = Cmd (S [rmlc; A "-where"; Sh ">"; Px path]) in
    Command.execute cmd);
  let s = read_file (Pathname.to_string path) in
  stdlib_dir := String.sub s 0 (String.length s - 1)
;;

let link_rml extension env _build =
  let rmlsim_file = env "%.rmlsim" in
  let main_file = read_rmlsim rmlsim_file in
  let byte_file = Pathname.update_extension extension main_file in
  let rml_byte_file = env ("%.rml."^extension) in
  tag_file byte_file ["use_rmllib"; "use_unix"];
  tag_file byte_file (Tags.elements (tags_of_pathname rml_byte_file));
  List.iter Outcome.ignore_good (_build [[byte_file]]);
  ln_s byte_file rml_byte_file
;;

let rmlbuild_after_rules () =
      find_rml_stdlib_dir ();
      ocaml_lib ~extern:true ~native:true ~dir:!stdlib_dir "rmllib";

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
        Cmd(S ([rmlc] @ includes @ [P file]))
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
        tag_file gen_file ["use_rmllib"];

        let file = env "%.rml" in
        let includes = mk_includes (Pathname.dirname file) in
        Cmd(S ([rmlc]@includes@[T (tags_of_pathname file++"rml"++"compile"); P file]))
      end;

      rule "rml: rmlsim -> byte"
        ~prod:"%.rml.byte"
        ~dep:"%.rmlsim"
        (link_rml "byte");;

      rule "rml: rmlsim -> d.byte"
        ~prod:"%.rml.d.byte"
        ~dep:"%.rmlsim"
        (link_rml "d.byte");;

      rule "rml: rmlsim -> native"
        ~prod:"%.rml.native"
        ~dep:"%.rmlsim"
        (link_rml "native");;

      rule "rml: rmlsim -> p.native"
        ~prod:"%.rml.native"
        ~dep:"%.rmlsim"
        (link_rml "p.native");;

      rule "rml: rmllib -> mllib"
        ~prod:"%.mllib"
        ~dep:"%.rmllib"
      begin fun env _build ->
        let file = env "%.rmllib" in
        let mllib_file = Pathname.update_extension "mllib" (Pathname.basename file) in
        let cma_file = Pathname.update_extension "cma" file in
        tag_file cma_file ["use_rmllib"; "use_unix"];
        let cma_file = Pathname.update_extension "d.cma" file in
        tag_file cma_file ["use_rmllib"; "use_unix"];
        let cmxa_file = Pathname.update_extension "cmxa" file in
        tag_file cmxa_file ["use_rmllib"; "use_unix"];
        let cmxa_file = Pathname.update_extension "p.cmxa" file in
        tag_file cmxa_file ["use_rmllib"; "use_unix"];
        cp file mllib_file
      end;

      flag ["rml"; "compile"; "annot"] (A "-dtypes");
      flag ["rml"; "compile"; "lco_rewrite"] (S ([A "-runtime"; A "Lco_rewrite"]));
      flag ["rml"; "compile"; "lco"] (S ([A "-runtime"; A "Lco"]));
      flag ["rml"; "compile"; "lco_ctrl_tree"] (S ([A "-runtime"; A "Lco_ctrl_tree"]));
      flag ["rml"; "compile"; "lco_ctrl_tree_n"] (S ([A "-runtime"; A "Lco_ctrl_tree_n"]));
      flag ["rml"; "compile"; "lk"] (S ([A "-runtime"; A "Lk"]))
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

