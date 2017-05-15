let rec dellt a list =
        if a>=0 then
                if a=0 then
                        list
                else
                        let n::rest = list in dellt (a-1) rest
        else
                failwith "Exception: Failure"
;;


let rec dellt2 a list =
        if a>=0 then
                match (a, list) with
                        |(0, list) -> list
                        |(1, n2::r2) -> dellt2 0 r2             
                        |(_, n1::r1) -> n1 :: dellt2 (a-1) r1
        else
                failwith "Exception: Failure"
;;

let rec find a list =
        match (a, list) with
                |(_, []) -> false
                |(_, n::rest) -> if n=a then
                                         true
                                 else
                                         find a rest
;;

