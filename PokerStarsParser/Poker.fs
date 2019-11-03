namespace PokerStarsParser

open System

 module Poker =
 
    type Suit = Club | Diamond | Heart | Spade
    type Value = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King
    type Variant = HoldEmNoLimit | HoldEmPotLimit

    type Card = {
        Suit: Suit
        Value: Value
    }

    type Player = String
    
    type Currency = Chips | USD | GBP

    type Amount = {
        Amount: decimal
        Currency: Currency
    }

    type Dealt = {
        Player: Player
        Hand: Card List
    }

    type Seat = {
        Position: int
        Player: Player
    }

    type Action = {
        Player: Player
        Amount: Amount
    }
    
    type Actions = 
        | SmallBlind of Action 
        | BigBlind of Action 
        | Dealt of Dealt 
        | Bet of Action 
        | Call of Action 
        | Raise of Action 
        | Fold of Player 
        | Uncalled of Action 
        | Won of Action 
        | Unknown of string 
        | Show of Dealt
        | DoesNotShow of Player

    type StreetType = {
        Cards: Card List
        Actions: Actions List
    } 

    type Street = 
        | PreFlop of Actions List 
        | Flop of StreetType 
        | Turn of StreetType 
        | River of StreetType 
        | Showdown of Actions List
        | Summary

    type Hand = {
        Date: DateTime
        Type: Variant
        HandNumber: Int64
        Players: Seat List
        PreAction: Actions List
        Streets: Street List
    }

    type UnParsedLine = {
        Content: String
    }