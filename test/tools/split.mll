(***************************************************************************)
(*                                Lucy-n                                   *)
(*                                                                         *)
(*                                                                         *)
(* Authors: Louis Mandel (louis.mandel@lri.fr)                             *)
(*          Florence Plateau (florence.plateau@lri.fr)                     *)
(*                                                                         *)
(* Creation date: September 2011                                           *)
(*                                                                         *)
(* Based on "Programmation de Programmation", Université Paris-Sud 11.     *)
(*                                                                         *)
(***************************************************************************)

{

  let ext = ref ".ls"

  (* let string_buf = Buffer.create 1024 *)

  (* let labeltab = Hashtbl.create 17  *)

  let postfix = ref "testgen"
  let filename = ref ""
  let label = ref ""

  let filenum = ref 0

  let flags = [Open_text; Open_excl; Open_creat]

  let new_label str =
    (* Hashtbl.add labeltab !label !filenum; *)
    label := str(* ; *)
    (* filenum := try Hashtbl.find labeltab str with Not_found -> 0 *)

  let newfile =
    let make_tag s = if s = "" then "" else "-" ^ s in
    fun () ->
      let postfix = make_tag !postfix in
      let label = make_tag !label in
      Format.sprintf "%s-%03i%s%s%s" !filename !filenum label postfix !ext

  let preamble = ref ""
  let write str =
    incr filenum;
    while Sys.file_exists (newfile ()) do incr filenum done;
    let chan = open_out (newfile ()) in
    output_string chan !preamble;
    output_string chan str;
    flush chan;
    close_out chan

}

let file = ([^'$'])+
let sep = '$'
let lab = "$$$"
let comment = "$$" [^'\n']* ( '\n' | eof )
let alpha= [ 'a'-'z' 'A'-'Z' ]
let ident= (alpha | '_')+


rule split = parse
    "${" { preamble_aux lexbuf }
  | "${$}" { preamble := ""; true }
  | lab (ident as newlab) ' '* '\n' { new_label newlab; true }
  | file { write (Lexing.lexeme lexbuf); true }
  | sep { true }
  | comment { true }
  | eof { false }
  | _ { failwith "????"}

and preamble_aux = parse
    "$}" { true }
  | file { preamble := Lexing.lexeme lexbuf; preamble_aux lexbuf }
  | _ { failwith "????"}

{
  let cwd = Sys.getcwd ()

  let process file =
    if not (Filename.check_suffix file ".split") then
      raise (Arg.Bad "no .split extension");
    filename := Filename.chop_suffix (Filename.basename file) ".split";
    Sys.chdir cwd;
    let chan = open_in file in
    Sys.chdir (Filename.dirname file);
    let buf = Lexing.from_channel chan in
    filenum := 0;
    preamble := "";
    label := "";
    while split buf do () done;
    close_in chan

  let _ =
    let spec =
      [ "-post", Arg.Set_string postfix,
        "change the postfix of the number of the files generated (default "^ !postfix ^")";
        "-ext", Arg.Set_string ext,
        "choose the extension of the files generated (default "^ !ext ^")"; ]
    in
    let usage =
      "Usage: "^Sys.argv.(0)^" file.split \n"^
      "File splitter for tests."
    in
    Arg.parse spec process usage;
    Sys.chdir cwd

}
