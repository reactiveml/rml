type appareance = | App_none | App_text of string | App_Image of Misc.t
type atticobj =
       {
         larg: int; 
         haut: int; 
         appar: appareance; 
         col: Rml_Graphics.color; 
         pos: Rml_Graphics.pos
       }
type behavior =
       process{inout getatt: (unit , unit list) Rml_interpreter.event ;
       inout icoatt: (atticobj , atticobj list) Rml_interpreter.event ;
       inout a_click: (unit , unit list) Rml_interpreter.event ;
       inout getlist: (unit , unit list) Rml_interpreter.event ;
       inout listico: (icobj list , icobj list list) Rml_interpreter.event ;
       }
and icobj =
      {
        nocloneable:
          process{out outsig: (behavior , behavior list)
                              Rml_interpreter.event
          ; }; 
        cloneable:
          process{out outsig: (behavior , behavior list)
                              Rml_interpreter.event
          ; }; 
        activate: process{}; 
        att: atticobj
      }
val att_set_image :  atticobj -> Misc.t -> unit
val att_set_texte :  atticobj -> string -> unit
val pi :  float
val mytime :  process{out ds: (unit , 'a) Rml_interpreter.event ; }
val icobj : 
  process{inout getatt: ('a , 'b) Rml_interpreter.event ;
  inout icoatt: (atticobj , 'c) Rml_interpreter.event ;
  inout a_click: ('d , 'd list) Rml_interpreter.event ;
  inout getlist: ('e , 'f) Rml_interpreter.event ;
  inout listico: ('g , 'h) Rml_interpreter.event ; } ->
  bool ->
  process{inout getatt: ('a , 'b) Rml_interpreter.event ;
  inout icoatt: (atticobj , 'c) Rml_interpreter.event ;
  inout a_click: ('d , 'd list) Rml_interpreter.event ;
  inout getlist: ('e , 'f) Rml_interpreter.event ;
  inout listico: ('g , 'h) Rml_interpreter.event ; } ->
  Rml_Graphics.pos ->
  process{in step: ('i , 'j) Rml_interpreter.event ;
  in dograph: ('k , 'l) Rml_interpreter.event ;
  out graph: (Rml_Graphics.objet_graphic list , 'm) Rml_interpreter.event ;
  in activate: ('d , 'd list) Rml_interpreter.event ;
  in getclonebehav: ('n , 'o) Rml_interpreter.event ;
  out clonebehav: (process{inout getatt: ('a , 'b) Rml_interpreter.event ;
                   inout icoatt: (atticobj , 'c) Rml_interpreter.event ;
                   inout a_click: ('d , 'd list) Rml_interpreter.event ;
                   inout getlist: ('e , 'f) Rml_interpreter.event ;
                   inout listico: ('g , 'h) Rml_interpreter.event ; }
                  , 'p) Rml_interpreter.event
  ; in getnoclonebehav: ('q , 'r) Rml_interpreter.event ;
  out noclonebehav: (process{inout getatt: ('a , 'b) Rml_interpreter.event ;
                     inout icoatt: (atticobj , 'c) Rml_interpreter.event ;
                     inout a_click: ('d , 'd list) Rml_interpreter.event ;
                     inout getlist: ('e , 'f) Rml_interpreter.event ;
                     inout listico: ('g , 'h) Rml_interpreter.event ; }
                    , 's) Rml_interpreter.event
  ; inout getatt: ('a , 'b) Rml_interpreter.event ;
  out icoatt: (atticobj , 'c) Rml_interpreter.event ;
  out getlist: ('e , 'f) Rml_interpreter.event ;
  in listico: ('g , 'h) Rml_interpreter.event ; }
val get_getcloneableproc :  'a * 'b * 'c -> 'c
val get_getprocactiv :  'a * 'b * 'c -> 'b
val get_att :  'a * 'b * 'c -> 'a
val geticobj :  icobj list ref -> Rml_Graphics.pos -> icobj list
val geticobj_under :  icobj list -> atticobj -> icobj list
val selected_object : 
  icobj ->
  process{in dograph: ('a , 'b) Rml_interpreter.event ;
  out graph: (Rml_Graphics.objet_graphic list , 'c) Rml_interpreter.event ;
  in a_move: ('d , Rml_Graphics.pos list) Rml_interpreter.event ; }
val addicobj : 
  ((process{inout getatt: (unit , unit list) Rml_interpreter.event ;
    inout icoatt: (atticobj , atticobj list) Rml_interpreter.event ;
    inout a_click: (unit , unit list) Rml_interpreter.event ;
    inout getlist: (unit , unit list) Rml_interpreter.event ;
    inout listico: (icobj list , icobj list list) Rml_interpreter.event ; }
   * bool)
  *
  process{inout getatt: (unit , unit list) Rml_interpreter.event ;
  inout icoatt: (atticobj , atticobj list) Rml_interpreter.event ;
  inout a_click: (unit , unit list) Rml_interpreter.event ;
  inout getlist: (unit , unit list) Rml_interpreter.event ;
  inout listico: (icobj list , icobj list list) Rml_interpreter.event ; } *
  Rml_Graphics.pos) list ->
  process{inout att: (icobj , 'a) Rml_interpreter.event ;
  inout graph: (Rml_Graphics.objet_graphic list , 'b) Rml_interpreter.event ;
  inout step: ('c , 'd) Rml_interpreter.event ;
  inout dograph: ('e , 'f) Rml_interpreter.event ;
  inout getlist: (unit , unit list) Rml_interpreter.event ;
  inout listico: (icobj list , icobj list list) Rml_interpreter.event ; }
val gere_addicobj : 
  process{inout newicobj: ('a ,
                          ((process{inout getatt: (unit , unit list)
                                                  Rml_interpreter.event
                            ;
                            inout icoatt: (atticobj , atticobj list)
                                          Rml_interpreter.event
                            ;
                            inout a_click: (unit , unit list)
                                           Rml_interpreter.event
                            ;
                            inout getlist: (unit , unit list)
                                           Rml_interpreter.event
                            ;
                            inout listico: (icobj list , icobj list list)
                                           Rml_interpreter.event
                            ; }
                           * bool)
                          *
                          process{inout getatt: (unit , unit list)
                                                Rml_interpreter.event
                          ;
                          inout icoatt: (atticobj , atticobj list)
                                        Rml_interpreter.event
                          ;
                          inout a_click: (unit , unit list)
                                         Rml_interpreter.event
                          ;
                          inout getlist: (unit , unit list)
                                         Rml_interpreter.event
                          ;
                          inout listico: (icobj list , icobj list list)
                                         Rml_interpreter.event
                          ; } * Rml_Graphics.pos) list) Rml_interpreter.event
  ; inout att: (icobj , 'b) Rml_interpreter.event ;
  inout graph: (Rml_Graphics.objet_graphic list , 'c) Rml_interpreter.event ;
  inout step: ('d , 'e) Rml_interpreter.event ;
  inout dograph: ('f , 'g) Rml_interpreter.event ;
  inout getlist: (unit , unit list) Rml_interpreter.event ;
  inout listico: (icobj list , icobj list list) Rml_interpreter.event ; }
val workspace : 
  int ->
  int ->
  process{in newicobj: ('a ,
                       ((process{inout getatt: (unit , unit list)
                                               Rml_interpreter.event
                         ;
                         inout icoatt: (atticobj , atticobj list)
                                       Rml_interpreter.event
                         ;
                         inout a_click: (unit , unit list)
                                        Rml_interpreter.event
                         ;
                         inout getlist: (unit , unit list)
                                        Rml_interpreter.event
                         ;
                         inout listico: (icobj list , icobj list list)
                                        Rml_interpreter.event
                         ; }
                        * bool)
                       *
                       process{inout getatt: (unit , unit list)
                                             Rml_interpreter.event
                       ;
                       inout icoatt: (atticobj , atticobj list)
                                     Rml_interpreter.event
                       ;
                       inout a_click: (unit , unit list)
                                      Rml_interpreter.event
                       ;
                       inout getlist: (unit , unit list)
                                      Rml_interpreter.event
                       ;
                       inout listico: (icobj list , icobj list list)
                                      Rml_interpreter.event
                       ; } * Rml_Graphics.pos) list) Rml_interpreter.event
  ; }
