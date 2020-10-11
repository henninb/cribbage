# cribbage

https://github.com/sukkis/spell-playground/blob/dc8e03c7d5ab5a2dbf22ba09c76b73af7c79558f/haskell/poker.hs

https://github.com/haihoang20/OverUnder/blob/7cec473c76c017a1e39f38dadb6ad1efb181da16/RandomTest.hs

https://github.com/patrickleboutillier/cribbage/blob/master/Cards.hs

https://github.com/aornota/cribbage/blob/94dd31bac23f62dddbc825c0784731371195ade0/src/domain/scoring.fs

checkGameStatus
determineWinner
checkPlayerScore
dropPlayer
getPlayerScore
showPlayerScore
showPlayerCards
setupGame
scoreAllPlayers
scorePlayer
createPlayers

type PeggingScoreEvent =
    | PeggingPair of Rank
    | ThreeOfAKind of Rank
    | FourOfAKind of Rank
    | PeggingRun of high:Rank * low:Rank
    | PeggingFifteen
    | ThirtyOne
    | Go
