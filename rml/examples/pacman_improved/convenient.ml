(*                                 *)
(* essai de definition d'un pacman *)
(*                                 *)
(* version Reactive-ML             *)

open Array
open Misc
open Moteur

let nombre_point = ref 0;;

let donne_nb_point () = (!nombre_point)

(* Les textures du jeu *)

let blanc      = misc_load "images/noir.bmp" [];;
let point      = misc_load "images/point.bmp" [];;
let grospoint  = misc_load "images/grospoint.bmp" [];;
let horiz      = misc_load "images/droit.bmp" [];;
let vertic     = misc_load "images/vert.bmp" [];;
let hautdroite = misc_load "images/haut_droit.bmp" [];;
let hautgauche = misc_load "images/haut_gauche.bmp" [];;
let basdroite  = misc_load "images/bas_droit.bmp" [];;
let basgauche  = misc_load "images/bas_gauche.bmp" [];;
let barhoriz   = misc_load "images/barriere_droit.bmp" [];;
let barvertic  = misc_load "images/barriere_vert.bmp" [];;

(* les images de monstres *)

let monstreVert   = misc_load "images/monstre_vert.bmp" [];;
let monstreRouge  = misc_load "images/monstre_red.bmp" [];;
let monstreBleu   = misc_load "images/monstre_bleu.bmp" [];;
let monstreViolet = misc_load "images/monstre_viol.bmp" [];;
let monstreChasse = misc_load "images/monstre_chasse.bmp" [];;

let joueur        = misc_load "images/joueur.bmp" [];;


(*                                                 *)
(* d'abord, on essaye de definir une table de jeux *)
(*                                                 *)

(* Initialisation du jeux       *)
(* pour utilisation des infos X *)

(* definition de types *)

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
  | Blanc;;

type direction =
    | Dirhaut
    | Dirbas
    | Dirgauche
    | Dirdroite

type cmd =
    | Dir of direction
    | Quit
    | No_cmd
(* Obtient la commande qui correspond a la touche clavier *)

let cmd_clavier = function
	    'u' -> Dir Dirhaut
	   |'j' -> Dir Dirbas
	   |'h' -> Dir Dirgauche
	   |'k' -> Dir Dirdroite
	   |'q' -> Quit
	   | _  -> No_cmd;;




(* les briques font 20x20 pixels             *)
(* une table 25x25 = 25*20 x 25*20 = 500x500 *)

let max_x = 25 and
    max_y = 25;;

(* table est notre table de jeux            *)
(* pour des soucis d'efficacité, c'est une  *)
(* variable globale.                        *)

let table = create_matrix max_x max_y Blanc;;

let get_table () = table;;
(* table.(i).(j) commence a 0.0 vers (max_x -1) et (max_y -1) *)

let charge_table_from_file name =
  let f_in = open_in name in
  for i = (max_x -1) downto 0 do
    for j = 0 to (max_y -1) do 
      match (input_char f_in) with
        '-' ->table.(i).(j) <- Horiz
      | '|' ->table.(i).(j) <- Vertic 
      | '_' ->table.(i).(j) <- BarHoriz
      | ':' ->table.(i).(j) <- BarVertic 
      | '.' ->table.(i).(j) <- Point;
              nombre_point := !nombre_point +1
      | 'o' ->table.(i).(j) <- GrosPoint;
              nombre_point := !nombre_point +1;
      | '1' ->table.(i).(j) <- HautGauche 
      | '2' ->table.(i).(j) <- HautDroite
      | '3' ->table.(i).(j) <- BasGauche
      | '4' ->table.(i).(j) <- BasDroite
      | ' ' ->table.(i).(j) <- Blanc
      | e   ->print_string "Le caractere ";
              print_char e;
              print_string "n'est pas une bonne valeur ";
              flush stdout;
              failwith ""
    done;
    ignore(input_char f_in);
  done;
  close_in f_in;;


(* les monstres sont des images 20x20  *)

(*                                           *)
(* x et y sont des coordonnees dans la table *)
(* d' est la direction                       *)

let transition (x,y) d =
  let (new_x,new_y) =
    match d with
      'j' -> (x-1,y)
    | 'k' -> (x+1,y)
    | 'a' -> (x,y+1)
    | 'z' -> (x,y-1)
    | 'q' -> failwith "end"
    | _ -> (x,y)
  in

  (* ca c'est pour faire des gauches_droites *)
  let new_x = if (new_x < 0) then (max_x - 1) else (new_x mod max_x) in
  let new_y = if (new_y < 0) then (max_y - 1) else (new_y mod max_y) in

  (* ici test avec la table *)
  match table.(new_y).(new_x) with
        Point      -> (new_x,new_y)
      | GrosPoint  -> (new_x,new_y)
      | Horiz      -> (x,y)
      | Vertic     -> (x,y)
      | HautGauche -> (x,y)
      | HautDroite -> (x,y)
      | BasGauche  -> (x,y)
      | BasDroite  -> (x,y)
      | BarHoriz   -> (x,y)
      | BarVertic  -> (x,y)
      | Blanc      -> (new_x,new_y);;

let est_un_point {xi=x;yi=y} = 
  (table.(y).(x) = Point) || (table.(y).(x)= GrosPoint);;

let est_un_gros_point {xi=x;yi=y} = 
  (table.(y).(x)= GrosPoint);;

let supprime_point {xi=x;yi=y}= (table.(y).(x) <- Blanc);;

let print_info (a,b) =
  match (table.(b).(a)) with
    Point      -> print_string "Point"
  | GrosPoint  -> print_string "GrosPoint"
  | Horiz      -> print_string "Horiz"
  | Vertic     -> print_string "Vertic"
  | HautGauche -> print_string "HautGauche ;"
  | BarHoriz   -> print_string "BarHoriz"
  | BarVertic  -> print_string "BarVertic"
  | HautDroite -> print_string "HautDroite"
  | BasGauche  -> print_string "BasGauche"
  | BasDroite  -> print_string "BasDroite"
  | Blanc      -> print_string "Blanc";
      
      flush stdout;;

(* Obtient la texture correspondant au type de brique *)

let get_brique {xi=x;yi=y} =
  match (get_table ()).(y).(x) with
    Point     -> point
  | GrosPoint -> grospoint
  | Horiz      -> horiz
  | Vertic     -> vertic
  | HautGauche -> hautgauche
  | HautDroite -> hautdroite
  | BasGauche  -> basgauche
  | BasDroite  -> basdroite
  | BarHoriz   -> barhoriz
  | BarVertic  -> barvertic
  | Blanc      -> blanc;;


(* ici des fonctions pour les monstres *)

(*                                                                *)
(* il faut ecrire une heuristique pour le deplacement des monstres *)
(*                                                                *)

(* nous nous servons des informations de la table. *)

(* deja calcul des directions possibles *)
(* 1 : haut                             *)
(* 2 : bas                              *)
(* 3 : gauche                           *)
(* 4 : droite                           *)

(* une petite initialisation ... *)
(* du random..                   *)

Random.init (int_of_float (Unix.time ()));;

let choix a b =
  if ((Random.int 10) < 5) then a else b;;

let est_possible {xi=ancx;yi=ancy} d isallow=
  let (new_x,new_y) =
  (match d with
      Dirgauche -> (ancx-1,ancy)
    | Dirdroite -> (ancx+1,ancy)
    | Dirhaut   -> (ancx,ancy+1)
    | Dirbas    -> (ancx,ancy-1))
  in
  let new_x = if (new_x < 0) then (max_x - 1) else (new_x mod max_x) in
  let new_y = if (new_y < 0) then (max_y - 1) else (new_y mod max_y) in
  let test_possibilite () =
  match table.(new_y).(new_x) with
        Point      -> (true,{xi=new_x;yi=new_y})
      | GrosPoint  -> (true,{xi=new_x;yi=new_y})
      | Blanc      -> (true,{xi=new_x;yi=new_y})
      | BarVertic  -> if isallow then
                        (true,{xi=new_x;yi=new_y}) else (false,{xi=0;yi=0})
      | BarHoriz   -> if isallow then
                        (true,{xi=new_x;yi=new_y}) else (false,{xi=0;yi=0})
      | _          -> (false,{xi=0;yi=0})  in 

  match table.(ancy).(ancx) with
  | BarHoriz -> if d = Dirgauche or d = Dirdroite then
                     (false,{xi=0;yi=0})
		else
		     test_possibilite ()
  | BarVertic -> if d = Dirhaut or d = Dirbas then
                     (false,{xi=0;yi=0})
		else
		     test_possibilite ()
  | _ -> test_possibilite ();;


(* la distance entre deux points *)
(* souvenir de 3eme              *)
(* d= sqrt (x-x')^2 + (y-y')^2   *)

let f {xi=x;yi=y} {xi=px;yi=py}  = ((x-px)*(x-px) + (y-py)*(y-py));; 


let calcul_suivant_monstre ({xi=x;yi=y} as fant,({xi=px;yi=py} as p)) odt t isallow =

  let (haut,phaut)    = est_possible fant Dirhaut   isallow in
  let (bas,pbas)      = est_possible fant Dirbas    isallow in
  let (gauche,pgauche)= est_possible fant Dirgauche isallow in
  let (droit,pdroit)  = est_possible fant Dirdroite isallow in

  (* ici code pas terrible   *)
  (* on regarde la direction *)
  
  let d1 = f phaut p in
  let d2 = f pbas  p in
  let d3 = f pgauche p in
  let d4 = f pdroit p in

  (* cette instruction calcule le meilleur choix *)

  let (_,(_,(bestd,bestp))) =
    List.hd (Sort.list
          (fun (a,(b,_)) (c,(d,_)) ->
            (if (t =1) then (b < d) else (b > d)))
          (
           List.remove_assoc false 
             (List.remove_assoc false
                (List.remove_assoc false [(haut,(d1,(Dirhaut,phaut)));
                                         (bas,(d2,(Dirbas,pbas)));
                                         (gauche,(d3,(Dirgauche,pgauche)));
                                         (droit,(d4,(Dirdroite,pdroit)))]))))
  in

  let (dirc,posc) =

  (* on match les 2^4 = 16 choix *)

    (match (haut,bas,gauche,droit) with

    | (true,true,true,true) ->
      (* les 4 sont possibles *)
        begin
          match odt with
            Dirhaut -> (choix (choix (Dirgauche,pgauche) (Dirdroite,pdroit)) (Dirhaut,phaut))
          | Dirbas -> (choix (choix (Dirgauche,pgauche) (Dirdroite,pdroit)) (Dirbas,pbas))
          | Dirgauche -> (choix (choix (Dirhaut,phaut)   (Dirbas,pbas)  ) (Dirhaut,pgauche))
          | Dirdroite -> (choix (choix (Dirhaut,phaut)   (Dirbas,pbas)  ) (Dirdroite,pdroit))
        end
          
    | (true,true,false,false) ->
      (* soit en bas ou en haut *)
        if (odt = Dirhaut) then (Dirhaut,phaut)
        else (Dirbas,pbas)
            
    | (false,false,true,true) ->
      (* a gauche et a droite *)
        if (odt = Dirgauche) then (Dirgauche,pgauche)
        else (Dirdroite,pdroit)
            
    | (true,false,true,false) ->
      (* inferieur droit *)
        if (odt = Dirbas) then (Dirgauche,pgauche)
        else (Dirhaut,phaut) (* ok *)
            
    | (true,false,false,true) ->
      (* inferieur gauche *)
        if (odt = Dirbas) then (Dirdroite,pdroit)
        else (Dirhaut,phaut) (* ok *)
            
    | (false,true,true,false) ->
      (* superieur droit *)
        if (odt = Dirhaut) then (Dirgauche,pgauche)
        else (Dirbas,pbas)
            
    | (false,true,false,true) ->
      (* superieur gauche *)
        if (odt = Dirhaut) then (Dirdroite,pdroit)
        else (Dirbas,pbas)
            
    | (true,false,true,true) ->
      (* _|_ *)
        begin
          match odt with
            Dirdroite -> choix (Dirhaut,phaut)  (Dirdroite,pdroit)
          | Dirgauche -> choix (Dirhaut,phaut)  (Dirgauche,pgauche)
          | _ -> choix (Dirdroite,pdroit) (Dirgauche,pgauche)
        end
          
    | (false,true,true,true) ->
      (* -|- *)
        begin
          match odt with
            Dirdroite -> choix (Dirbas,pbas) (Dirdroite,pdroit)
          | Dirgauche -> choix (Dirbas,pbas) (Dirgauche,pgauche)
          | _ -> choix (Dirdroite,pdroit) (Dirgauche,pgauche)
        end
    | (true,true,false,true) ->
        begin
          match odt with
            Dirhaut -> choix (Dirhaut,phaut) (Dirdroite,pdroit)
          | Dirbas -> choix (Dirbas,pbas)  (Dirdroite,pdroit)
          | _ -> choix (Dirbas,pbas)  (Dirhaut,phaut)
        end
          
    | (true,true,true,false) ->
        begin
          match odt with
            Dirhaut -> choix (Dirhaut,phaut) (Dirgauche,pgauche)
          | Dirbas -> choix (Dirbas,pbas)  (Dirgauche,pgauche)
          | _ -> choix (Dirbas,pbas)  (Dirhaut,phaut)
        end
          
    | (true,_,_,_)    -> (Dirhaut,phaut)
    | (_,true,_,_)    -> (Dirbas,pbas)
    | (_,_,true,_)    -> (Dirgauche,pgauche)
    | (_,_,_,true)    -> (Dirdroite,pdroit)
    | (_,_,_,_)       -> failwith "Il existe une case d'ou l'on ne peut sortir !!"
    )
  in
  if ((Random.int 35) > 5) then
    (dirc,posc)
  else
    (bestd,bestp);;
(*   (Dirdroite,pdroit);; *)
(* *)

let affiche_monstre_inter (x,y) dir img =
  let cx = 20*x in
  let cy = 20*y in

  let (cx,cy) =
  match dir with
      3 -> (cx-10,cy)
    | 4 -> (cx+10,cy)
    | 1 -> (cx,cy+10)
    | 2 -> (cx,cy-10)
    | _ -> (cx,cy)
  in
  let new_x = if (cx < 0) then ((max_x - 1)*20) else (cx mod (max_x *20)) in
  let new_y = if (cy < 0) then ((max_y - 1)*20) else (cy mod (max_y *20)) in
  ();;


