namespace PokerStarsParser

open CardParser
open DateParser
open ListOperations
open FParsec
open Poker

module Parser = 
    
    //Basic parsers
    let private ws = many1 (skipChar ' ')
    let private ws0 = many (skipChar ' ')

    let private endOfLine  = restOfLine false
    let private unknownLine = notFollowedByL newline "new line uh-oh"  >>. (endOfLine |>> fun x -> Unknown(x))

    let private skipUntil p =  many (notFollowedBy p >>. anyChar) >>. p

    let private betweenParentheses p = pchar '(' >>. p .>> pchar ')'
    
    //Hand Information
    let private handNumber = pchar '#' >>. pint64 .>> pchar ':' 

    let private variant = 
        (pstring "Hold'em No Limit"  |>> fun _ -> HoldEmNoLimit)
        <|> (pstring  "Hold'em Pot Limit"  |>> fun _ -> HoldEmPotLimit)
  
    let private handInformation =  pipe3 (skipUntil handNumber) (skipUntil variant) (skipUntil pDateTime .>> endOfLine) (fun hand variant date -> (variant, date, hand))
 
    let private tableInformation = unknownLine

    let private pPlayer = (pstring "Seat" 
        .>> ws 
        >>. pint32 
        .>> pchar ':' 
        .>> ws 
        .>>. charsTillString " " true 1000 
        .>> endOfLine |>> fun (seatNum, player)  -> { Position = seatNum; Player =player })

    let private toRoundType (str, cards: Card list list, actions: Actions list) = (
        match str with 
        | "HOLE CARDS" -> PreFlop (actions)
        | "FLOP" -> Flop ( { Cards = cards.Head; Actions = actions })
        | "TURN" -> Turn ({ Cards = last cards; Actions = actions })
        | "RIVER" -> River ({ Cards = last cards; Actions = actions })
        | "SUMMARY" -> Summary
        | "SHOW DOWN" -> Showdown (actions)
        | str -> failwith (sprintf "unknown round type %s" str))

    let private betweenStars = pstring "*** " >>. charsTillString " ***" true 20
    let private handType = pipe2 (betweenStars) (ws0 >>. sepBy parseCards ws) (fun x y -> (x, y))    
    
    //actions 
    let private pDealt = (pipe2 
                            (pstring "Dealt to " >>. charsTillString " " true 40)
                            (parseCards) 
                            (fun un cards -> Dealt({Player = un ; Hand = cards })))
     
    let private pShow = (pipe2 
                        (charsTillString ":" true 40 .>> pstring " shows ")
                        (parseCards .>> endOfLine) 
                        (fun un cards -> Show({Player = un ; Hand = cards })))


    let private pBigBlind = (pipe2 
                                (charsTillString ":" true 40 .>> pstring " posts big blind ")
                                (pstring "$" >>. pfloat) 
                                (fun un amount -> BigBlind({Player = un ; Amount = { Amount = decimal amount; Currency = USD} })))
    

    let private pSmallBlind = (pipe2 
                                (charsTillString ":" true 40 .>> pstring " posts small blind ")
                                (pstring "$" >>. pfloat) 
                                (fun un amount -> SmallBlind({Player = un; Amount = { Amount = decimal amount; Currency = USD} }))) 

    let private pBet = (pipe2 
                            (charsTillString ": bets " true 40)
                            (pstring "$" >>. pfloat .>> endOfLine) 
                            (fun un amount -> Bet({Player = un ; Amount = { Amount = decimal amount; Currency = USD} })))
    
    let private pCall = (pipe2 
                            (charsTillString ": calls " true 20)
                            (pstring "$" >>. pfloat .>> endOfLine) 
                            (fun un amount -> Call({Player = un ; Amount = { Amount = decimal amount; Currency = USD} })))
    
    let private pRaise = (pipe2
                            (charsTillString ": raises " true 20 .>> pstring "$" .>> pfloat .>> pstring " to ")
                            (pstring "$" >>. pfloat .>> endOfLine)
                            (fun un amount -> Raise({Player = un ; Amount = { Amount = decimal amount; Currency = USD} })))

    let private pFold = charsTillString ": folds" true 20 .>> endOfLine |>> fun (un) -> Fold (un)
    
    let private pCheck = charsTillString ": checks" true 20 .>> endOfLine |>> fun (un) -> Fold (un)
    
    let private pNoShow = charsTillString ": doesn't show hand" true 20 .>> endOfLine |>> fun (un) -> DoesNotShow (un)
    
    let private pUncalled = (pipe2 
                                (pstring "Uncalled bet " >>. betweenParentheses (pstring "$" >>. pfloat)) 
                                (pstring " returned to " >>. endOfLine) 
                                (fun bet un -> Uncalled({Player = un ; Amount = { Amount = decimal bet; Currency = USD} }))
    )

    let private pCollected = (pipe2 
                                (charsTillString " collected " true 40)
                                (pstring "$" >>. pfloat .>> endOfLine)
                                (fun un bet -> Won({Player = un ; Amount = { Amount = decimal bet; Currency = USD} }))
    )

    let private pActions = (notFollowedByL (pchar '*') "No more actions" 
        >>. 
        (attempt(pCall)
        <|> attempt(pRaise) 
        <|> attempt(pFold) 
        <|> attempt(pDealt)
        <|> attempt(pCheck)
        <|> attempt(pUncalled) 
        <|> attempt(pBet) 
        <|> attempt(pCollected)
        <|> attempt(pSmallBlind)
        <|> attempt(pBigBlind)
        <|> attempt(pShow)
        <|> attempt(pNoShow)
        <|> attempt(unknownLine)))
    
    let private endOfBlock = newline .>> ((notFollowedByL (pchar '*') "* error" |>> ignore) <|> notFollowedByNewline)   

    let private pStreet = (pipe2 
                            (handType) 
                            (newline >>. sepEndBy pActions (attempt endOfBlock))
                            (fun (streetStr, cards) actions -> (toRoundType(streetStr, cards, actions))) 
        )
       
    let private filterSummary p =
        match p with
            | Summary -> false
            | _ -> true

    let private toHand variant handTime handNumber seats actions streets = {
        Date = handTime; 
        Type = variant; 
        HandNumber =  handNumber; 
        Players = seats; 
        PreAction = actions; 
        Streets = (Seq.filter filterSummary streets) |> List.ofSeq
    }
       
    let private pHand = (pipe4 (handInformation .>> newline)
                            (tableInformation .>> newline >>. sepEndBy1 pPlayer newline)
                            (sepEndBy pActions (attempt (endOfBlock)))
                            (many pStreet)
                            (fun (variant, handTime, handNumber) seats actions streets -> toHand variant handTime handNumber seats actions streets))

    let private manyHands = sepEndBy pHand (many newline)

    let private parseHands str = run manyHands str

    let private runParser(str, p) = run p str
    
    type ParseResult =
        | Hands of Hand list
        | Error of string

    let private mapResult p = 
        match p with
            | Success(result, _, _)   -> Hands(result)
            | Failure(errorMsg, _, _) ->  Error(errorMsg)

    let parse str = parseHands str |> mapResult