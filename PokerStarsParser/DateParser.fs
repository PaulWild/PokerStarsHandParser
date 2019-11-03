namespace PokerStarsParser

open FParsec
open System

module DateParser =

    let internal pDateTime = 
        pipe3 (pint32 .>> pchar '/')  (pint32 .>> pchar '/') (pint32) (fun a b c -> (a, b, c)) .>>
        spaces1 .>>.
        pipe3 (pint32 .>> pchar ':')  (pint32 .>> pchar ':') (pint32 .>> spaces1 .>> pstring "UTC") (fun a b c -> (a, b, c)) |>>
        fun ((year, month, day), (hour, minute, second)) -> DateTime(year, month, day, hour, minute, second)

    let private runDate p = run pDateTime p
        

 
        
