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

let f x = 
    (x * (x+1)/2);;

let dif a b =
    if a>b then f (a-b)
    else f (b-a);;

let rec poskusi x arr =
    match arr with
    | [] -> 0
    | a::b -> (dif a x) + (poskusi x b);;

let () =
    let arr = load (preberi_datoteko "day_7.in") in
    let ans=ref (203104140194) in
    for i=0 to 2000 do
        ans:=(min (!ans) (poskusi i arr))
    done;
    print_int (!ans);;