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
                match a with
                        | 0 -> list
                        | 1 -> list
                        | _ -> let n::rest = list in let temp = n::[], dellt2 (a-1) rest
        else
                failwith "Exception: Failure"
;;

