namespace PokerStarsParser

open CardParser
open DateParser
open ListOperations
open FParsec
open Poker

module Parser = 

    //Basic parsers
    let ws = many1 (skipChar ' ')
    let ws0 = many (skipChar ' ')

    let endOfLine = restOfLine false

    let skipUntil p = many (notFollowedBy p >>. anyChar) >>. p

    let betweenParentheses p = pchar '(' >>. p .>> pchar ')'

    //Hand Information
    let handNumber = pchar '#' >>. pint64 .>> pchar ' ' 

    let variant = 
        (pstring "Hold'em No Limit" |>> fun _ -> HoldEmNoLimit)
        <|> (pstring "Hold'em Pot Limit" |>> fun _ -> HoldEmPotLimit)

    let handInformation = pipe2 (skipUntil handNumber) (skipUntil variant) (fun hand variant -> (variant, hand ))

    let date = pDateTime
    
    let toRoundType (str, cards: Card list list, actions: Actions list) = (
        match str with 
        | "HOLE CARDS" -> PreFlop (actions)
        | "FLOP" -> Flop ( { Cards = cards.Head; Actions = actions })
        | "TURN" -> Turn ({ Cards = last cards; Actions = actions })
        | "RIVER" -> River ({ Cards = last cards; Actions = actions })
        | "SHOW DOWN" -> Showdown (actions)
        | str -> failwith (sprintf "unknown round type %s" str))

    let betweenStars = pstring "*** " >>. charsTillString " ***" true 20

    let handType = (pipe2 (betweenStars) (ws0 >>. sepBy parseCards ws) (fun x y -> (x, y)))

    //actions 
    let pDealt = (pipe2 
                      (pstring "Dealt to " >>. charsTillString " " true 40)
                      (parseCards) 
                      (fun un cards -> Dealt({Player = un ; Hand = cards })))

    let pShow = (pipe2 
                    (charsTillString ":" true 40 .>> pstring " shows ")
                    (parseCards .>> endOfLine) 
                    (fun un cards -> Show({Player = un ; Hand = cards })))


    let pBigBlind = (pipe2 
                        (charsTillString ":" true 40 .>> pstring " posts big blind ")
                        (pstring "$" >>. pfloat) 
                        (fun un amount -> BigBlind({Player = un ; Amount = { Amount = decimal amount; Currency = USD} })))


    let pSmallBlind = (pipe2 
                        (charsTillString ":" true 40 .>> pstring " posts small blind ")
                        (pstring "$" >>. pfloat) 
                        (fun un amount -> SmallBlind({Player = un; Amount = { Amount = decimal amount; Currency = USD} }))) 

    let pBet = (pipe2 
                  (charsTillString ": bets " true 40)
                  (pstring "$" >>. pfloat .>> endOfLine) 
                  (fun un amount -> Bet({Player = un ; Amount = { Amount = decimal amount; Currency = USD} })))

    let pCall = (pipe2 
                  (charsTillString ": calls " true 20)
                  (pstring "$" >>. pfloat .>> endOfLine) 
                  (fun un amount -> Call({Player = un ; Amount = { Amount = decimal amount; Currency = USD} })))

    let pRaise = (pipe2
                      (charsTillString ": raises " true 20 .>> pstring "$" .>> pfloat .>> pstring " to ")
                      (pstring "$" >>. pfloat .>> endOfLine)
                      (fun un amount -> Raise({Player = un ; Amount = { Amount = decimal amount; Currency = USD} })))

    let pFold = charsTillString ": folds" true 20 .>> endOfLine |>> fun (un) -> Fold (un)

    let pCheck = charsTillString ": checks" true 20 .>> endOfLine |>> fun (un) -> Fold (un)

    let pNoShow = charsTillString ": doesn't show hand" true 20 .>> endOfLine |>> fun (un) -> DoesNotShow (un)

    let pUncalled = (pipe2 
                        (pstring "Uncalled bet " >>. betweenParentheses (pstring "$" >>. pfloat)) 
                        (pstring " returned to " >>. endOfLine) 
                        (fun bet un -> Uncalled({Player = un ; Amount = { Amount = decimal bet; Currency = USD} }))
                    )

    let pCollected = (pipe2 
                        (charsTillString " collected " true 40)
                        (pstring "$" >>. pfloat .>> endOfLine)
                        (fun un bet -> Won({Player = un ; Amount = { Amount = decimal bet; Currency = USD} }))
                    )
    
    let private unknownLine = notFollowedByL newline "new line uh-oh"  >>. (endOfLine |>> fun x -> Unknown(x))


    let pActions = (notFollowedByL (pchar '*') "No more actions" 
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
    
    let endOfBlock = newline .>> ((notFollowedByL (pchar '*') "* error" |>> ignore) <|> notFollowedByNewline)  

    let pStreet = (pipe2 
                      (handType) 
                      (newline >>. sepEndBy pActions (attempt endOfBlock))
                      (fun (streetStr, cards) actions -> (toRoundType(streetStr, cards, actions))) 
                    )


    let toHand variant handTime handNumber streets = {
        Date = handTime; 
        Type = variant; 
        HandNumber = handNumber; 
        Streets = streets
        }

    let pHand = (pipe3 (handInformation .>> newline)
                  (date .>> newline)
                  (many pStreet)
                  (fun (variant, handNumber) date streets -> toHand variant date handNumber streets))

    let manyHands = sepEndBy pHand (many newline)

    let parseHands str = run manyHands str

    let runParser(str, p) = run p str

    type ParseResult =
    | Hands of Hand list
    | Error of string

    let mapResult p = 
        match p with
          | Success(result, _, _)  -> Hands(result)
          | Failure(errorMsg, _, _) -> Error(errorMsg)

    let parse str = parseHands str |> mapResult