# cribbage

## build with a specific version
```
stack --resolver lts-11.22 install <pacakge> 
```

## potential future functions
- checkGameStatus
- determineWinner
- checkPlayerScore
- dropPlayer
- getPlayerScore
- showPlayerScore
- showPlayerCards
- setupGame
- scoreAllPlayers
- scorePlayer
- createPlayers

# Pegging Game
```
type PeggingScoreEvent =
    | PeggingPair of Rank
    | ThreeOfAKind of Rank
    | FourOfAKind of Rank
    | PeggingRun of high:Rank * low:Rank
    | PeggingFifteen
    | ThirtyOne
    | Go
```