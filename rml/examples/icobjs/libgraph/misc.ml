open Gif;;
open Image;;
open Graphic_image;;


type t = Image.t;;

type load_option = Image.load_option;;

let misc_load n =
     let format, header = Image.file_format n in
     match format with
     | Gif -> Gif.load_first n
     | _   -> Image.load n;;

let misc_draw_image = Graphic_image.draw_image;;

let misc_size = Image.size;;

