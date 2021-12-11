let pretvori ch =
    (int_of_char ch)  - (int_of_char '0')

let explode s = Array.init (String.length s) (fun x -> pretvori (String.get s x))

let load vsebina_datoteke =
    let s = String.split_on_char '\n' vsebina_datoteke in
    Array.of_list (List.map (fun x -> (explode (String.trim x))) s)

let preberi_datoteko ime_datoteke =
    let chan = open_in_bin ime_datoteke in
    let vsebina = really_input_string chan (in_channel_length chan) in
    close_in chan;
    vsebina;;

let izpisi_datoteko ime_datoteke vsebina =
    let chan = open_out_bin ime_datoteke in
    output_string chan vsebina;
    close_out chan;;

let arr = load (preberi_datoteko "day_11.in");;
let mn = 10;;

let vis = Array.make_matrix mn mn false;;
let rec dfs x y = 
    if x<0 || y<0 || x>=mn || y>=mn then ()
    else if vis.(x).(y) then ()
    else if arr.(x).(y)<9 then arr.(x).(y) <- (arr.(x).(y)+1)
    else(
        vis.(x).(y) <- true;

        dfs (x+1) (y);
        dfs (x-1) (y);
        dfs (x) (y+1);
        dfs (x) (y-1);

        dfs (x+1) (y+1);
        dfs (x+1) (y-1);
        dfs (x-1) (y+1);
        dfs (x-1) (y-1);
        ()
    );;

let reset ()=
    for i=0 to mn-1 do
        for j=0 to mn-1 do
            vis.(i).(j) <- false
        done;
    done;;

let korak ()=
    reset ();
    for i=0 to mn-1 do
        for j=0 to mn -1 do
            dfs i j
        done;
    done;
    let ret = ref 0 in
    for i=0 to mn-1 do
        for j=0 to mn -1 do
            if vis.(i).(j) then (
                arr.(i).(j) <- 0;
                ret := (!ret + 1)
                )
        done;
    done;
    !ret;;

let resi ()= 
    let ret2 = ref (-1) in
    let ret = ref 0 in  
    for i=1 to 1000 do
        let df =korak() in
        if i<=100 then ret:=(!ret +df);
        if df=(mn * mn) && ((!ret2) =(-1)) then ret2 := i
    done;
    izpisi_datoteko "day_11_1.out" (string_of_int (!ret));
    izpisi_datoteko "day_11_2.out" (string_of_int (!ret2));;
resi ();;

