(*************************************************************************)
(*                              Reactive ML                              *)
(*                                                                       *)
(*  Fichier : ident.ml                                                   *)
(*  Date de creation : 23/04/2004                                        *)
(*  Auteur : Louis Mandel                                                *)
(*  Remarque : taken from OCaml                                          *)
(*************************************************************************)

(* $Id$ *) 

type kind = 
    Val_ML | Val_RML | Sig 
  | Constr | Label | Exn | Type | Internal

type t = { id: int; name: string; kind: kind; }

let compare i1 i2 = compare i1.id i2.id

let name i = i.name

let string_of_kind = function
  | Val_ML -> "val_ml"
  | Val_RML -> "val_rml"
  | Sig -> "sig"
  | Constr -> "cstr"
  | Label -> "lbl"
  | Exn -> "exn"
  | Type -> "ty"
  | Internal -> "loc"

let unique_name i = 
(*  i.name ^ *)"__" ^ (string_of_kind i.kind) ^ "_" ^ (string_of_int i.id)

(* generating names *)
class name_generator =
  object
    val mutable counter = 0
    method name =
      counter <- counter + 1;
      counter
    method reset =
      counter <- 0
    method init i =
      counter <- i
  end

let currentid = ref 0

let create gen s k =
  {id = gen#name;
   name = s;
   kind = k; }
      
let same i1 i2 = i1.id = i2.id 
