let mx =100;;
let my=100;;

let rec prestej x y =
    if y=my then 0
    else if x=mx then (prestej 0 (y+1))
    else (prestej (x+1) y);;

print_int (prestej 0 0);;