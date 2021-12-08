let load vsebina_datoteke =
    let s = String.split_on_char ',' vsebina_datoteke in
    s |> List.map (fun x ->  int_of_string(String.trim x))

let preberi_datoteko ime_datoteke =
    let chan = open_in_bin ime_datoteke in
    let vsebina = really_input_string chan (in_channel_length chan) in
    close_in chan;
    vsebina

let izpisi_datoteko ime_datoteke vsebina =
    let chan = open_out_bin ime_datoteke in
    output_string chan vsebina;
    close_out chan

let f1 x = 
    x;;

let f2 x = 
    (x * (x+1)/2);;

let dif1 a b=
    if a>b then f1 (a-b)
    else f1 (b-a);;
let dif2 a b=
    if a>b then f2 (a-b)
    else f2 (b-a);;

let rec poskusi1 x arr =
    match arr with
    | [] -> 0
    | a::b -> (dif1 a x) + (poskusi1 x b);;
let rec poskusi2 x arr =
    match arr with
    | [] -> 0
    | a::b -> (dif2 a x) + (poskusi2 x b);;
let () =
    let arr = load (preberi_datoteko "day_7.in") in
    let ans=ref (203104140194) in
    for i=0 to 2000 do
        ans:=(min (!ans) (poskusi1 i arr))
    done;
    izpisi_datoteko "day_7_1.out" (string_of_int (!ans));;

let () =
    let arr = load (preberi_datoteko "day_7.in") in
    let ans=ref (203104140194) in
    for i=0 to 2000 do
        ans:=(min (!ans) (poskusi2 i arr))
    done;
    izpisi_datoteko "day_7_2.out" (string_of_int (!ans));;