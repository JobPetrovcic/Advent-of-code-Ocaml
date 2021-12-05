let dobi_rep sez = 
    match sez with
    | [] -> []
    | a :: b -> b

let dobi_prvi sez = 
    match sez with
    | [] -> failwith "neki bla bla"
    | a :: b -> a

let pretvorideci sez =
    match sez with
    | [] -> 0
    | a:: rep  -> if a=='1' then (1 + 2 * (pretvorideci rep)) else 2 * (pretvorideci rep)

let rec count listoflistofchars =
    match listoflistofchars with
    | [] -> 0
    | a :: rep -> if (dobi_prvi a)=='0' then count rep else (1+ (count rep))


let rec precistivecina listoflistofchars =
    let izb = if (count listoflistofchars) > ((List.length listoflistofchars)/2) then '1' else '0'
    in
    let rec aux sez =
        match sez with
        | [] -> []
        | a :: rep -> if (dobi_prvi a)==izb then (dobi_rep a):: (aux rep) else (aux rep)
    in
    aux listoflistofchars

let rec resivecina listoflistofchars =
    match listoflistofchars with
    | [] -> []
    | a:: b -> (if (count listoflistofchars) > ((List.length listoflistofchars)/2) then '1' else '0') :: (resivecina (precistivecina listoflistofchars ))

let rec countvecina listoflistofchars=
    match listoflistofchars with
    | [] -> []
    | a:: b ->  ((List.length listoflistofchars)/2) then '1' else '0') :: (resivecina (precistivecina listoflistofchars ))

let rec dobi_vecina listoflistofchars =



let rec precistimanjsina listoflistofchars =
    let izb = if (count listoflistofchars) <= ((List.length listoflistofchars)/2) then '1' else '0'
    in
    let rec aux sez =
        match sez with
        | [] -> []
        | a :: rep -> if (dobi_prvi a)==izb then (dobi_rep a):: (aux rep) else (aux rep)
    in
    aux listoflistofchars



    
