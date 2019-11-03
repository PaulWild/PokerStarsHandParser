# Poker Stars Parser

Hand parser for poker stars cash games hands.

## Maybe

```fsharp
let parsedHands = parse file

match result with
    | Hands(x) -> ... //Hand list
    | Error(str) -> ... //Parsing error string
```