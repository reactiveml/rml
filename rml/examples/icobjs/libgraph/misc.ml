open Gif;;
open Images;;
open Graphic_image;;


type t = Images.t;;

type load_option = Images.load_option;;

let misc_load n =
     let format, header = Images.file_format n in
     match format with
     | Gif -> Gif.load_first n
     | _   -> Images.load n;;

let misc_draw_image = Graphic_image.draw_image;;

let misc_size = Images.size;;

