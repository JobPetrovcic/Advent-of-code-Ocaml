let explode s = Array.init (String.length s) (fun x -> (int_of_char (String.get s x) - 48))
let load vsebina_datoteke =
    let ss = String.split_on_char '\n' vsebina_datoteke in
    let prss =(ss |> List.map String.trim) in
    let ars = Array.of_list prss in
    Array.map explode ars;;

let preberi_datoteko ime_datoteke =
    let chan = open_in_bin ime_datoteke in
    let vsebina = really_input_string chan (in_channel_length chan) in
    close_in chan;
    vsebina;;

let izpisi_datoteko ime_datoteke vsebina =
    let chan = open_out_bin ime_datoteke in
    output_string chan vsebina;
    close_out chan;;

let arr = load (preberi_datoteko "day_9.in");;
let n = Array.length arr;;
let m = Array.length arr.(0);;

let preveri x y va=
    if x<0 || y<0 || x>=n || y>=m then true
    else va < arr.(x).(y)

let je_nizka x y =
    if (preveri (x+1) (y) arr.(x).(y)) && (preveri (x-1) (y) arr.(x).(y)) && (preveri (x) (y+1) arr.(x).(y)) && (preveri (x) (y-1) arr.(x).(y)) then (arr.(x).(y)+1)
    else 0;;

let () =
    let ans = ref 0 in
    for x = 0 to n-1 do
        for y = 0 to m-1 do
            ans := ((!ans) + (je_nizka x y))
        done
    done;
    izpisi_datoteko "day_9_1.out" (string_of_int (!ans))


let vis = Array.make_matrix n m false;;
let rec dfs x y =
    if x<0 || y<0|| x>=n || y>=m then 0
    else if vis.(x).(y) then 0
    else if arr.(x).(y)==9 then (
        vis.(x).(y) <- true;
        0
    )
    else(
        vis.(x).(y) <- true;
        1 + (dfs (x+1) (y)) +(dfs (x-1) (y)) +(dfs (x) (y+1)) +(dfs (x) (y-1))
    );;

let rec dodaj x y =
    if x>=n then (dodaj 0 (y+1))
    else if y>=m then []
    else ((dfs x y):: (dodaj (x+1) y));;

let cmp a b =
    if a>b then 0
    else 1;;

let () =
    let sizes = (dodaj 0 0) in
    let srt = List.sort cmp sizes in
    let srtarr = Array.of_list srt in
    izpisi_datoteko "day_9_2.out" (string_of_int(srtarr.(0) * srtarr.(1) * srtarr.(2)));;