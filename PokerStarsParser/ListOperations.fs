namespace PokerStarsParser

module ListOperations = 

    let rec internal last = function
        | [x] -> x
        | _ :: tl -> last tl
        | _ -> failwith "Empty list."
