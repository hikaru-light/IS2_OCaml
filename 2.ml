let rec dellt a list =
        if a>=0 then
                if a=0 then
                        list
                else
                        let n::rest = list in dellt (a-1) rest
        else
                failwith "Error"
;;


let rec dellt2 a list =
        if a>=0 then
                match (a, list) with
                        |(0, list) -> list
                        |(1, n2::r2) -> dellt2 0 r2
                        |(_, n1::r1) -> n1 :: dellt2 (a-1) r1
        else
                failwith "Error"
;;

let rec find a list =
        match (a, list) with
                |(_, []) -> false
                |(_, n::rest) -> if n=a then
                                         true
                                 else
                                         find a rest
;;

let rec posl a list =
        match (a, list) with
                |(0, _) -> failwith "Not Exist..."
                |(1, n2::r2) -> n2
                |(_, n1::r1) -> posl (a-1) r1
;;

let rec divlist list1 list2 =
        match (list1, list2) with
                |([], []) -> []
                |(n1::r1, n2::r2) -> n1/n2 :: divlist r1 r2
;;

let rec mul2list list =
        match list with
                | _::[] -> []
                | n1::n2::rest -> n1*n2 :: mul2list (n2::rest)
;;

let rec chglist (a,b) list =
        match list with
                | [] -> []
                | n::rest -> if n=a then
                                     b :: chglist (a,b) rest
                             else
                                     n :: chglist (a,b) rest
;;

let rec inslist n m xs =
        if n>=0 then
                match (n, xs) with
                        |(1,[]) -> m::[]
                        |(1,y::ys) -> m::y::ys
                        |(_,z::zs) -> z::inslist (n-1) m zs
                        |(0,[]) -> failwith "Error"
                        |(_,[]) -> failwith "Error"
        else
                failwith "Error"
;;

let rec replicate n l =
        if n>1 then l :: replicate (n-1) l
        else if n=1 then l :: []
             else
                     failwith "Error"
;;

let rec merge list1 list2 =
        match (list1, list2) with
                | ([], list2) -> list2
                | (list1, []) -> list1
                | (n1::r1, n2::r2) -> n1 :: n2 ::  merge r1 r2
;;

let rec inside_length list =
        match list with
                |[] -> 0
                | n1::r1 -> let rec inside_l n1 =
                                    match n1 with
                                            | [] -> 0
                                            | n2::r2 -> 1 + inside_l r2 in inside_l n1 + inside_length r1
;;

let rec concat list =
        match list with
                | [] -> []
                | n1::r1 -> let rec inside_c n1 =
                                    match n1 with
                                            | [] -> []
                                            | n2::r2 -> n2::inside_c r2 in inside_c n1 @ concat r1
;;

let rec assoc a list =
        match list with
                | [] -> failwith "Not Found..."
                | (x,y)::rest -> if x=a then y
                                 else
                                         if y=a then x
                                         else
                                                 assoc a rest
;;

let rec maximum l =
        match l with
                | [] -> failwith "Error"
                | x::y::rest -> if x>y then maximum (x::rest)
                              else
                                      maximum (y::rest)
                | z::[] -> z
;;

let index l n =
        let rec length_i list c =
                match list with
                        | x::xs -> if x=n then c
                                 else length_i xs (c+1)
                        | [] -> -1
        in length_i l 1
;;

let keirosu t y =
        let rec comb (n,m) =
                if n=m || m=0 then 1
                else (comb (n, m-1)) * (n-m+1) / m
        in comb (t+y, y)
;;
