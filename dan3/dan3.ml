(*load funkcija je skopirana od Luka Horjaka *) 
let load vsebina_datoteke =
    let s = String.split_on_char '\n' vsebina_datoteke in
    s |> List.map String.trim

let dobi_prvi sez = 
    match sez with
    | [] -> failwith "neki bla bla"
    | a :: b -> a
    
let dobi_rep sez = 
    match sez with
    | [] -> []
    | a :: b -> b

let explode s = List.init (String.length s) (String.get s)

let reverse list =
    let rec aux acc = function
      | [] -> acc
      | h :: t -> aux (h :: acc) t in
    aux [] list;;

let naloga1 vsebina_datoteke =
    let rec dodaj ret chars =
        match chars with
        | [] -> []
        | a::rep -> if a=='0' then (dobi_prvi ret ):: dodaj (dobi_rep ret ) rep else ((dobi_prvi ret) +1):: dodaj (dobi_rep ret ) rep
    in 
    let dobi_pojavnost stringi =
        let n = List.length (explode (dobi_prvi  stringi))
        in
        let rec aux vrstice =
            match vrstice with
            | [] -> (List.init n (fun x->0))
            | vrstica :: rep -> dodaj (aux rep) (explode vrstica)
        in
        aux stringi
    in
    let dobi_odgovor stringi =
        let rec vecina pow sez =
            let n = List.length stringi in
            match sez with
            | [] -> 0
            | a :: rep -> if a > n/2 then pow + vecina (pow * 2) rep else vecina (pow * 2) rep
        and manjsina pow sez =
            let n = List.length stringi in
            match sez with
            | [] -> 0
            | a :: rep -> if a > n/2 then manjsina (pow * 2) rep else pow + manjsina (pow * 2) rep
        in
        let pojavnost = reverse (dobi_pojavnost stringi)
        in 
        (vecina 1 pojavnost) * (manjsina 1 pojavnost)
    in
    string_of_int (dobi_odgovor (load vsebina_datoteke))

let naloga2 vsebina_datoteke =
    "0"

        

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
    let vsebina_datoteke = preberi_datoteko "day_3.in" in
    let odgovor1 = naloga1 vsebina_datoteke
    and odgovor2 = naloga2 vsebina_datoteke
    in
    izpisi_datoteko "day_3_1.out" odgovor1;
    izpisi_datoteko "day_3_2.out" odgovor2