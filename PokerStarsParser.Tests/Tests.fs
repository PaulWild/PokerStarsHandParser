module Tests

open Xunit
open PokerStarsParser
open Parser

 [<Fact>]
 let  ``Parse File`` () = 
     let file = """ *********** # 2 **************
PokerStars Hand #204242837932:  Hold'em No Limit ($0.02/$0.05 USD) - 2019/09/14 9:37:20 UTC [2019/09/14 5:37:20 ET]
Table 'Felicia V' 6-max Seat #1 is the button
Seat 1: $ergeyBak$ ($5 in chips)
Seat 2: t'sprat ($2.72 in chips)
Seat 3: danrune ($4.70 in chips)
Seat 4: pekvakt ($6.89 in chips)
Seat 5: _HunterPan_ ($5 in chips)
Seat 6: xdontxpanicx ($2.09 in chips)
t'sprat: posts small blind $0.02
danrune: posts big blind $0.05
*** HOLE CARDS ***
Dealt to xdontxpanicx [Qc Kd]
pekvakt: folds
_HunterPan_: raises $0.10 to $0.15
xdontxpanicx: raises $0.15 to $0.30
$ergeyBak$: raises $0.82 to $1.12
t'sprat: folds
danrune: folds
_HunterPan_: folds
xdontxpanicx: raises $0.97 to $2.09 and is all-in
$ergeyBak$: calls $0.97
*** FLOP *** [Ad 6c Jd]
*** TURN *** [Ad 6c Jd] [Ah]
*** RIVER *** [Ad 6c Jd Ah] [3d]
*** SHOW DOWN ***
xdontxpanicx: shows [Qc Kd] (a pair of Aces)
$ergeyBak$: shows [Qh Qd] (two pair, Aces and Queens)
$ergeyBak$ collected $4.22 from pot
*** SUMMARY ***
Total pot $4.40 | Rake $0.18
Board [Ad 6c Jd Ah 3d]
Seat 1: $ergeyBak$ (button) showed [Qh Qd] and won ($4.22) with two pair, Aces and Queens
Seat 2: t'sprat (small blind) folded before Flop
Seat 3: danrune (big blind) folded before Flop
Seat 4: pekvakt folded before Flop (didn't bet)
Seat 5: _HunterPan_ folded before Flop
Seat 6: xdontxpanicx showed [Qc Kd] and lost with a pair of Aces


*********** # 2 **************
PokerStars Hand #204242826503:  Hold'em No Limit ($0.02/$0.05 USD) - 2019/09/14 9:36:38 UTC [2019/09/14 5:36:38 ET]
Table 'Felicia V' 6-max Seat #6 is the button
Seat 1: $ergeyBak$ ($5 in chips)
Seat 2: t'sprat ($2.77 in chips)
Seat 3: danrune ($2.84 in chips)
Seat 4: pekvakt ($6.89 in chips)
Seat 5: _HunterPan_ ($5 in chips)
Seat 6: xdontxpanicx ($4.05 in chips)
$ergeyBak$: posts small blind $0.02
t'sprat: posts big blind $0.05
*** HOLE CARDS ***
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
*** SUMMARY ***
Total pot $3.99 | Rake $0.17
Board [Th 3h 3s 8h Kc]
Seat 1: $ergeyBak$ (small blind) folded before Flop
Seat 2: t'sprat (big blind) folded before Flop
Seat 3: danrune showed [Kh 7h] and won ($3.82) with a flush, King high
Seat 4: pekvakt folded before Flop (didn't bet)
Seat 5: _HunterPan_ folded before Flop (didn't bet)
Seat 6: xdontxpanicx (button) mucked [Qh 6h]



"""
     let result = parse file

     match result with
        | Hands(x) -> Assert.Equal(2, x.Length)
        | Error(str) -> Assert.True(false, str)
     