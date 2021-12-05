
let mx=1000;;
let my=1000;;
let mat = Array.make_matrix mx my 0;;

let rec updx x y1 y2 =
    if y1<=y2 then
        (mat.(x).(y1)<-(mat.(x).(y1)+1);
        updx x (y1+1) y2)
    else ();;

let rec updy y x1 x2 =
    if x1 <=x2 then
        (mat.(x1).(y)<-(mat.(x1).(y)+1);
        updy y (x1 + 1) x2)
    else ();;

(*let rec upddia x y1 y2 =
    if y1<=y2 then
        (mat.(x).(y1)<-(mat.(x).(y1)+1);
        upddia x (y1+1) (y2+1))
    else ();;*)

let update x1 y1 x2 y2 =
    if x1 = x2 then 
        updx x1 (min y1 y2) (max y1 y2)
    else if y1 = y2 then 
        updy y1 (min x1 x2) (max x1 x2)
    else ();;


let ime_datoteke= "day_5.in";;
let chan = Scanf.Scanning.open_in_bin ime_datoteke;;
let () =
    try
        while true; do
            Scanf.bscanf chan "%d,%d -> %d,%d\n" update
        done;
    with End_of_file ->
        Scanf.Scanning.close_in chan;;

let rec prestej x y =
    if y=my then 0
    else if x=mx then (prestej 0 (y+1))
    else (if (mat.(x).(y)>=2) then 1 + (prestej (x+1) y) else (prestej (x+1) y));;

let izpisi_datoteko ime_datoteke vsebina =
    let chan = open_out_bin ime_datoteke in
    output_string chan vsebina;
    close_out chan

let () =
    izpisi_datoteko "day_5_1.out" (string_of_int (prestej 0 0))