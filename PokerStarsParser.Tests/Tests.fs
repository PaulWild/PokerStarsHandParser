module Tests

open Xunit
open PokerStarsParser
open Parser

 [<Fact>]
 let  ``Parse File`` () = 
     let file = """*********** # 1 **************
PokerStars Hand #205368507579 Hold'em No Limit
2019/10/20 18:02:42 UTC
*** HOLE CARDS ***
darkslaver: posts small blind $0.02
xdontxpanicx: posts big blind $0.05
Dealt to xdontxpanicx [2c 5c]
lotdpower: folds 
kulesn: folds 
crazy_man211: calls $0.05
rzeszow17: folds 
darkslaver: raises $0.20 to $0.25
xdontxpanicx: calls $0.20
crazy_man211: calls $0.20
*** FLOP *** [2h 8d Kc]
darkslaver: bets $0.22
xdontxpanicx: calls $0.22
crazy_man211: calls $0.22
*** TURN *** [2h 8d Kc] [7c]
darkslaver: checks 
xdontxpanicx: checks 
crazy_man211: bets $0.42 and is all-in
darkslaver: folds 
xdontxpanicx: calls $0.42
crazy_man211 has timed out
*** RIVER *** [2h 8d Kc 7c] [4h]
*** SHOW DOWN ***
xdontxpanicx: shows [2c 5c] (a pair of Deuces)
crazy_man211: shows [Kh 8c] (two pair, Kings and Eights)
crazy_man211 collected $2.16 from pot

"""
     let result = parse file

     match result with
        | Hands(x) -> Assert.Equal(1, x.Length)
        | Error(str) -> Assert.True(false, str)

     