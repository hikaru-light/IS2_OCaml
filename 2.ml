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
                | [] -> []
                | _::[] -> []
                | n1::n2::rest -> n1*n2 :: mul2list (n2::rest)
;;

