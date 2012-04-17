
let basedir = Array.get Sys.argv ((Array.length Sys.argv) -1) in
let out = open_out_gen
  [Open_creat; Open_wronly; Open_trunc]
  0o640
  (Filename.concat (Filename.concat (Sys.getcwd()) "global") "rzi.ml")
in
let () = Printf.fprintf out "let known_modules = [\n" in
let () = Array.iter
  (fun file ->
    if Filename.check_suffix file ".rzi" then
    let fh = open_in (Filename.concat basedir file) in
    let md = (input_value fh : Def_modules.module0) in
    let modname = String.uncapitalize (Filename.chop_suffix file ".rzi") in
    Printf.fprintf out " \"%s\", \"%s\";\n"
      modname
      (String.escaped (Marshal.to_string md []))
  )
  (Sys.readdir basedir)
in
Printf.fprintf out "]\n"
