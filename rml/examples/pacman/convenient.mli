val nombre_point : int ref
val donne_nb_point : unit -> int
val blanc : Misc.t
val point : Misc.t
val grospoint : Misc.t
val horiz : Misc.t
val vertic : Misc.t
val hautdroite : Misc.t
val hautgauche : Misc.t
val basdroite : Misc.t
val basgauche : Misc.t
val monstreVert : Misc.t
val monstreRouge : Misc.t
val monstreBleu : Misc.t
val monstreViolet : Misc.t
val monstreChasse : Misc.t
val joueur : Misc.t
type brique =
    Point
  | GrosPoint
  | Horiz
  | Vertic
  | BarHoriz
  | BarVertic
  | HautGauche
  | HautDroite
  | BasGauche
  | BasDroite
  | Blanc
and direction = Dirhaut | Dirbas | Dirgauche | Dirdroite
and cmd = Dir of direction | Quit | No_cmd
val cmd_clavier : char -> cmd
val max_x : int
val max_y : int
val table : brique array array
val get_table : unit -> brique array array
val charge_table_from_file : string -> unit
val transition : int * int -> char -> int * int
val est_un_point : Moteur.pos -> bool
val est_un_gros_point : Moteur.pos -> bool
val supprime_point : Moteur.pos -> unit
val print_info : int * int -> unit
val get_brique : Moteur.pos -> Misc.t
val choix : 'a -> 'a -> 'a
val est_possible : Moteur.pos -> direction -> bool -> bool * Moteur.pos
val f : Moteur.pos -> Moteur.pos -> int
val calcul_suivant_monstre :
  Moteur.pos * Moteur.pos -> direction -> int -> bool -> direction * Moteur.pos
val affiche_monstre_inter : int * int -> int -> 'a -> unit
