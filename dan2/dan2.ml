(*load funkcija je skopirana od Luka Horjaka *) 
let load vsebina_datoteke =
    let s = String.split_on_char '\n' vsebina_datoteke in
    s |> List.map String.trim |> List.map (String.split_on_char ' ')

let naloga1 vsebina_datoteke =
    let rec aux cx cy vrstice=
        match vrstice with
        | ["forward";kok] :: rep -> aux cx (cy+int_of_string(kok)) rep
        | ["down";kok] :: rep -> aux (cx+int_of_string(kok)) cy rep
        | ["up";kok] :: rep -> aux (cx-int_of_string(kok)) cy rep
        | [] -> (cx * cy)
        | _ -> 0
    in
    let odg = aux 0 0 (load vsebina_datoteke) in
    string_of_int odg

let naloga2 vsebina_datoteke =
    let rec aux hp depth aim vrstice=
        match vrstice with
        | ["up";kok] :: rep -> aux hp depth (aim-int_of_string(kok)) rep
        | ["down";kok] :: rep -> aux hp depth (aim+int_of_string(kok)) rep
        | ["forward";kok] :: rep -> aux (hp+int_of_string(kok)) (depth + (aim * int_of_string(kok))) aim rep 
        | [] -> (hp * depth)
        | _ -> 0
    in
    let odg = aux 0 0 0 (load vsebina_datoteke) in
    string_of_int odg

let _ =
    let preberi_datoteko ime_datoteke =
        let chan = open_in_bin ime_datoteke in
        let vsebina = really_input_string chan (in_channel_length chan) in
        close_in chan;
        vsebina
    and izpisi_datoteko ime_datoteke vsebina =
        let chan = open_out_bin ime_datoteke in
        output_string chan vsebina;
        close_out chan
    in
    let vsebina_datoteke = preberi_datoteko "day_2.in" in
    let odgovor1 = naloga1 vsebina_datoteke
    and odgovor2 = naloga2 vsebina_datoteke
    in
    izpisi_datoteko "day_2_1.out" odgovor1;
    izpisi_datoteko "day_2_2.out" odgovor2