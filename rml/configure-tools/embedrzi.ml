
let (//) = Filename.concat;;

let basedir = Array.get Sys.argv ((Array.length Sys.argv) -1) in
let stdlib = basedir // "stdlib" in
let buf = Buffer.create 2048 in
let () = Buffer.add_string buf "let known_modules = [\n" in
let stdlib_files = Array.to_list (Sys.readdir stdlib) in
let stdlib_files = List.map (fun f -> stdlib // f) stdlib_files in
let stdlib_files =
  let rmltop_alt_global = basedir // "toplevel-alt" // "rmltop_alt_global.rzi" in
  if Sys.file_exists rmltop_alt_global then
    rmltop_alt_global :: stdlib_files
  else
    stdlib_files
in
let () = List.iter
  (fun file ->
    if Filename.check_suffix file ".rzi" then
    let fh = open_in file in
    let md = (input_value fh : Def_modules.module0) in
    let modname = Filename.basename (String.uncapitalize (Filename.chop_suffix file ".rzi")) in
    Buffer.add_string buf
      (Printf.sprintf " \"%s\", \"%s\";\n"
        modname
        (String.escaped (Marshal.to_string md [])))
  )
  stdlib_files
in
let () = Buffer.add_string buf "]" in
let out = open_out_gen
  [Open_creat; Open_wronly; Open_trunc]
  0o640
  (basedir // "compiler" // "global" // "rzi.ml")
in
Printf.fprintf out "%s\n" (Buffer.contents buf)
