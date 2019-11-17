module Tests

open Xunit
open PokerStarsParser
open Parser

 [<Fact>]
 let  ``Parse File`` () = 
     let file = """*********** # 1 **************
PokerStars Hand #204242826503 Hold'em No Limit
2019/09/14 9:36:38 UTC
*** HOLE CARDS ***
$ergeyBak$: posts small blind $0.02
t'sprat: posts big blind $0.05
Dealt to xdontxpanicx [Qh 6h]
danrune: raises $0.10 to $0.15
pekvakt: folds
_HunterPan_: folds
xdontxpanicx: calls $0.15
$ergeyBak$: folds
t'sprat: folds
*** FLOP *** [Th 3h 3s]
danrune: bets $0.05
xdontxpanicx: raises $0.20 to $0.25
danrune: raises $0.20 to $0.45
xdontxpanicx: calls $0.20
*** TURN *** [Th 3h 3s] [8h]
danrune: bets $0.05
xdontxpanicx: calls $0.05
*** RIVER *** [Th 3h 3s 8h] [Kc]
danrune: bets $1.31
xdontxpanicx: calls $1.31
*** SHOW DOWN ***
danrune: shows [Kh 7h] (a flush, King high)
xdontxpanicx: mucks hand
danrune collected $3.82 from pot

"""
     let result = parse file

     match result with
        | Hands(x) -> Assert.Equal(1, x.Length)
        | Error(str) -> Assert.True(false, str)

     