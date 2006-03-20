(**********************************************************************)
(*                        ReactiveML                                  *)
(*                                                                    *)
(* Auteur : Louis Mandel                                              *) 
(* Date de creation : 13/09/2005                                      *)
(* Fichier : implantation.ml                                          *)
(*                                                                    *)
(**********************************************************************)

module Lco_ctrl_tree_record = Lco_ctrl_tree.Rml_interpreter(Sig_env.Record)
module Lco_rewrite_record = Lco_rewrite.Rml_interpreter(Sig_env.Record)

module Lco_ctrl_tree_class = Lco_ctrl_tree.Rml_interpreter(Sig_env.Class)
