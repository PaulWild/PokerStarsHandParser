# Poker Stars Parser

Hand parser for poker stars cash games hands.

[![Build Status](https://dev.azure.com/paul-wild/Utilities/_apis/build/status/PaulWild.PokerStarsHandParser?branchName=master)](https://dev.azure.com/paul-wild/Utilities/_build/latest?definitionId=2&branchName=master)

## Example

```fsharp
let parsedHands = parse str

match result with
    | Hands(x) -> ... //Hand list
    | Error(str) -> ... //Parsing error string
```
