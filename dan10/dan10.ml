let load vsebina_datoteke =
    let s = String.split_on_char '\n' vsebina_datoteke in
    s |> List.map String.trim

let preberi_datoteko ime_datoteke =
    let chan = open_in_bin ime_datoteke in
    let vsebina = really_input_string chan (in_channel_length chan) in
    close_in chan;
    vsebina;;

let izpisi_datoteko ime_datoteke vsebina =
    let chan = open_out_bin ime_datoteke in
    output_string chan vsebina;
    close_out chan;;

let f ch =
    match ch with
    | '(' -> ')'
    | '[' -> ']'
    | '{' -> '}'
    | '<' -> '>'
    | _ -> 'n';;

let vrednost ch =
    match ch with
    | ')' -> 3
    | ']' -> 57
    | '}' -> 1197
    | '>' -> 25137
    | _ -> failwith "hmm";;

let predelaj s =
    let ans = ref 0 in
    let n = String.length s in
    let st = Stack.create() in
    for i=0 to n-1 do (
        let ch = s.[i] in
        if ((f ch)=='n') then (
            if !ans=0 then (
                if Stack.is_empty st then ans:=(vrednost ch)
                else (
                    let t = Stack.pop st in
                    if(t==ch) then()
                    else ans:=(vrednost ch)
                )
            )
            else ()
        )
        else Stack.push (f ch) st
    )
    done;
    (!ans);;
let novavrednost ch =
    match ch with
    | ')' -> 1
    | ']' -> 2
    | '}' -> 3
    | '>' -> 4
    | _ -> failwith "hmm";;
let rec resi1 sez=
    match sez with
    | a::b -> (
        (predelaj a) + (resi1 b))
    | [] -> 0;;

let dopolni s=
    let n = String.length s in
    let st = Stack.create() in
    for i=0 to n-1 do (
        let ch = s.[i] in
        if ((f ch)=='n') then( 
            Stack.pop st;
            ()
            )
        else (Stack.push (f ch) st)
    )
    done;
    let ans=ref 0 in
    while not (Stack.is_empty st) do
        ans:= ((!ans) * 5  + (novavrednost (Stack.pop st)))
    done;
    (!ans);;

let rec resi2 sez=
    match sez with
    | a::b -> (
        if (predelaj a)==0 then (dopolni a) :: (resi2 b)
        else (resi2 b)
    )
    | [] -> [];;

let cmp x y =
    if x<y then 1
    else -1

let dobi_mediano sez=
    let srt =List.sort cmp sez in
    let arr =Array.of_list srt in
    let n=Array.length arr in
    arr.(n/2)


let liss =load (preberi_datoteko "day_10.in");;
izpisi_datoteko "day_10_1.out" (string_of_int (resi1 liss));;
izpisi_datoteko "day_10_2.out" (string_of_int (dobi_mediano(resi2 liss)));;