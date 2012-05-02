
let (//) = Filename.concat;;

let basedir = Array.get Sys.argv ((Array.length Sys.argv) -1) in
let stdlib = basedir // "stdlib" in
let buf = Buffer.create 2048 in
let () = Buffer.add_string buf "let known_modules = [\n" in
let () = Array.iter
  (fun file ->
    if Filename.check_suffix file ".rzi" then
    let fh = open_in (stdlib // file) in
    let md = (input_value fh : Def_modules.module0) in
    let modname = String.uncapitalize (Filename.chop_suffix file ".rzi") in
    Buffer.add_string buf
      (Printf.sprintf " \"%s\", \"%s\";\n"
        modname
        (String.escaped (Marshal.to_string md [])))
  )
  (Sys.readdir stdlib)
in
let () = Buffer.add_string buf "]" in
let out = open_out_gen
  [Open_creat; Open_wronly; Open_trunc]
  0o640
  (basedir // "compiler" // "global" // "rzi.ml")
in
Printf.fprintf out "%s\n" (Buffer.contents buf)
