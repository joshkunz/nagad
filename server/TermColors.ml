open List

type color = 
    | Black | Red | Green | Yellow 
    | Blue | Magenta | Cyan | White;;

type color_bases = Foreground | Background;;
type intensity = Normal | Bright;;

type compiled_color = Color of string;;

let colors = [
     (Black,   0)
    ;(Red,     1)
    ;(Green,   2)
    ;(Yellow,  3)
    ;(Blue,    4)
    ;(Magenta, 5)
    ;(Cyan,    6)
    ;(White,   7)
];;

let color_bases = [
     (Foreground, 30)
    ;(Background, 40)
];;

let intensities = [
     (Normal, -1)
    ;(Bright,  1)
];;

let color_text name ?(intensity=Normal) ?(ground=Foreground) str =
    let color_number =   (List.assoc ground color_bases)
                       + (List.assoc name colors) in
    "\x1B[" ^ 
    (match intensity with
     | Normal -> ""
     | Bright -> string_of_int (List.assoc intensity intensities) ^ ";")
    ^ (string_of_int color_number) ^ "m" ^ str ^ "\x1B[0m";;
