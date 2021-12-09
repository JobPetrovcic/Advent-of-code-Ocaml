let drugi neki =
    match neki with
    | [a; b]-> b
    | _ -> failwith "verjetno je kakšen prazen line"

let load vsebina_datoteke =
    let ss = String.split_on_char '\n' vsebina_datoteke in
    let prss = (ss |> List.map (fun x -> (String.split_on_char ' ' (String.trim (drugi (String.split_on_char '|' x)))))) in
    prss

let preberi_datoteko ime_datoteke =
    let chan = open_in_bin ime_datoteke in
    let vsebina = really_input_string chan (in_channel_length chan) in
    close_in chan;
    vsebina;;

let izpisi_datoteko ime_datoteke vsebina =
    let chan = open_out_bin ime_datoteke in
    output_string chan vsebina;
    close_out chan;;


let arr = load (preberi_datoteko "day_8.in");;

let rec prestej ar=
    match ar with
    | a::b -> (
            let n = String.length a in
            if(n==2||n==3||n==4||n==7) then 1 + (prestej b)
            else (prestej b)
        )
    | [] -> 0;;

let rec predelaj ar =
    match ar with
    | a::b -> (prestej a) +(predelaj b)
    | [] -> 0;;

izpisi_datoteko "day_8_1.out" (string_of_int (predelaj arr))