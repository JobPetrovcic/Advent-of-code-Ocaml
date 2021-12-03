type vektor= {
    mutable x : int;
    mutable y : int
};;

let forward = {x = 1;y = 0};;
let down = {x=0;y=1};;
let up = {x=0;y=(-1)};;

(*let add p1 p2 =
    let a={x=p1.x+p2.x, y=p1.y+p2.y} in
    a;;*)

let kam = function
    | "forward" -> forward
    | "down" -> down
    | "up" -> up
    | _ -> up;;

let preberi_vrstico niz=
    Scanf.bscanf niz "%s %d" 

let reverse sez =
    let rec rev acc =
        function
        | [] -> acc
        | a :: b -> rev (a :: acc) b
    in
    rev [] sez
    
let preberi_datoteko ime_datoteke =
    let chan = open_in_bin ime_datoteke in
    let vsebina = really_input_string chan (in_channel_length chan) in
    close_in chan;