open Types
open Clocks
open Global

(* Informations associated with module names *)

type value_description = (value_type_description, value_clock_description) global
type constructor_description = (constructor_type_description,
                               constructor_clock_description) global
type label_description = (label_type_description,
                           label_clock_description) global
type type_description = (Types.type_description, clock_description) global

type module0 =
    { mod_name: string;                      (* name of the module *)
      mod_values: (string, value_description) Hashtbl.t;
                                             (* table of values *)
      mod_constrs: (string, constructor_description) Hashtbl.t;
                                             (* table of constructors *)
      mod_labels: (string, label_description) Hashtbl.t;
                                             (* table of labels *)
      mod_types: (string, type_description) Hashtbl.t;
                                             (* table of type constructors *)
    }
