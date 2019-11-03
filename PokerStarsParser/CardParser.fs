namespace PokerStarsParser

open FParsec
open Poker

module CardParser =

    let private str s = pstring s 
    
    let private charToValue cardValue = 
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

    let private charToCard cardType = 
        match cardType with 
        | 'h' -> Heart
        | 'c' -> Club
        | 's' -> Spade
        | 'd' -> Diamond
        | x -> failwith (sprintf "Invalid Card Value %c" x)

    let private pCardValue = anyOf ['A'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'; 'T'; 'J'; 'Q'; 'K'] |>> charToValue
 
    let private pCardType = anyOf ['h';  'c'; 's'; 'd'] |>> charToCard 
 
    let private parseCard = pipe2 (pCardValue) (pCardType) (fun value suit -> { Suit = suit; Value = value })

    let internal parseCards = str "[" >>. sepBy parseCard (str " ") .>> str "]"

    let private runCardParser p = run parseCards p
