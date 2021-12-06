(*zaradi branja s scanf je potrebno dodati na koncu inputa ",-1" *)

let ime_datoteke= "day_6.in";;
let chan = Scanf.Scanning.open_in_bin ime_datoteke;;

let le=8;;

let zac = Array.init (le+1) (fun x-> 0);;
let dodaj neki =
    zac.(neki) <- (zac.(neki) + 1);;

let rec sum ind=
    if ind<=le then (zac.(ind)) + (sum (ind+1))
    else 0;;

let () =
    try
        while true; do
            Scanf.bscanf chan "%d," dodaj
        done;
    with End_of_file ->
        Scanf.Scanning.close_in chan;;

let izpisi_datoteko ime_datoteke vsebina =
    let chan = open_out_bin ime_datoteke in
    output_string chan vsebina;
    close_out chan;;
(* za drugi del naloge samo spremenite številko iz 80 na 256 *)
let () =
    for i=1 to 256 do
        let novdp = Array.init (le+1) (fun x-> 0) in
        for i=1 to le do
            novdp.(i-1) <- ((novdp.(i-1))+(zac.(i)))
        done;
        novdp.(6) <- ((novdp.(6))+(zac.(0)));
        novdp.(8) <- ((novdp.(8))+(zac.(0)));
        for i=0 to le do
            zac.(i) <- novdp.(i)
        done;
    done;
    izpisi_datoteko "day_6_1.out" (string_of_int (sum 0));;