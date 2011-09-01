open Ocamlbuild_plugin;;
open Command;;

let source_dir = "/Users/ccpasteur/Documents/work/git/reparml"

let rmlc = A (source_dir^"/compiler/rpmlc.byte");;
let rmldep = A (source_dir^"/tools/rpmldep/rpmldep.byte");;
let rpmllib_dir = source_dir^"/interpreter/_build"
let stdlib_dir = source_dir^"/lib"
let stdlib_path = Pathname.mk stdlib_dir

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
      let names = List.map (fun s -> (Pathname.update_extension "rzi" (Pathname.mk s))) names in
      List.map (fun s -> [Pathname.basename s]) names
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

;;

let rmlbuild_after_rules () =
      rule "rmldep: rml -> rmldepends"
        ~prod:"%.rml.depends"
        ~dep:"%.rml"
      begin fun env _build ->
        let file = env "%.rml" in
        let includes = mk_includes (Pathname.dirname file) in
        Cmd(S ([rmldep; A"-I"; A (Pathname.to_string Pathname.pwd)]
               @ includes @ [Px file; Sh ">"; Px(env "%.rml.depends")]))
      end;

      rule "rmldep: mli -> rmldepends"
        ~prod:"%.rml.depends"
        ~dep:"%.mli"
      begin fun env _build ->
        let file = env "%.mli" in
        let includes = mk_includes (Pathname.dirname file) in
        Cmd(S ([rmldep; A"-I"; A (Pathname.to_string Pathname.pwd)]
               @ includes @ [Px file; Sh ">"; Px(env "%.rml.depends")]))
      end;

      rule "rml: mli -> rzi"
        ~prods:["%.rzi"]
        ~deps:["%.rml.depends"; "%.mli"]
      begin fun env _build ->
        let dep_file = env "%.rml.depends" in
        let depends = read_depends dep_file in
        if depends <> [] then
          List.iter Outcome.ignore_good (_build depends);

        let file = env "%.mli" in
        let includes = mk_includes (Pathname.dirname file) in
        Cmd(S ([rmlc] @ includes @ [A "-I"; A stdlib_dir; P file]))
      end;

      rule "rml: rml -> ml, rzi"
        ~prods:["%.ml"; "%.rzi"]
        ~deps:["%.rml.depends"; "%.rml"]
      begin fun env _build ->
        let dep_file = env "%.rml.depends" in
        let depends = read_depends dep_file in
        if depends <> [] then
          List.iter Outcome.ignore_good (_build depends);

        let gen_file = env "%.ml" in
        tag_file gen_file ["rpmllib"];

        let file = env "%.rml" in
        let includes = mk_includes (Pathname.dirname file) in
        Cmd(S ([rmlc; A "-I"; A stdlib_dir]
               @includes@[T (tags_of_pathname file++"rml"++"compile"); P file]))
      end;

      rule "rml: rmlsim -> byte"
        ~prod:"%.byte"
        ~dep:"%.rmlsim"
      begin fun env _build ->
        let rmlsim_file = env "%.rmlsim" in
        let main_file = read_rmlsim rmlsim_file in
        let byte_file = Pathname.update_extension "byte" main_file in
        tag_file byte_file ["rpmllib"; "use_unix"];
        List.iter Outcome.ignore_good (_build [[byte_file]]);
        ln_s byte_file (env "%.byte")
      end;

      ocaml_lib ~extern:true ~dir:rpmllib_dir ~tag_name:"rpmllib" "rpmllib";

      flag ["rml"; "compile"; "annot"] (A "-dtypes")
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

