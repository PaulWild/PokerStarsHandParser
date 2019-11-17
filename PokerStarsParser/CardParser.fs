namespace PokerStarsParser

open FParsec
open Poker

module CardParser =

    let str s = pstring s 
    
    let charToValue cardValue = 
        match cardValue with 
        | 'A' -> Ace
        | '2' -> Two
        | '3' -> Three
        | '4' -> Four
        | '5' -> Five
        | '6' -> Six
        | '7' -> Seven
        | '8' -> Eight
        | '9' -> Nine
        | 'T' -> Ten
        | 'J' -> Jack
        | 'Q' -> Queen
        | 'K' -> King
        | x -> failwith (sprintf "Invalid Card Value %c" x)

    let charToCard cardType = 
        match cardType with 
        | 'h' -> Heart
        | 'c' -> Club
        | 's' -> Spade
        | 'd' -> Diamond
        | x -> failwith (sprintf "Invalid Card Value %c" x)

    let pCardValue = anyOf ['A'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'; 'T'; 'J'; 'Q'; 'K'] |>> charToValue
 
    let pCardType = anyOf ['h';  'c'; 's'; 'd'] |>> charToCard 
 
    let parseCard = pipe2 (pCardValue) (pCardType) (fun value suit -> { Suit = suit; Value = value })

    let parseCards = between (str "[") (str "]") (sepBy parseCard (str " "))

    let private runCardParser p = run parseCards p
