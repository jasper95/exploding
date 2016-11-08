module Game where

import Common
import Shuffler

initialCardCount :: Int
initialCardCount = 5

initGame :: Int -> State

initGame n = State { players = [ HPlayer { name = "Player " ++ show x, hand = [ ]} | x <- [1..n] ],
                     deck = initDeck,
                     d_stack = [ ] }


setupGame :: State -> IO State
setupGame gs = do
    gs1 <- shuffleDeck gs
    shuffleDeck ( State {players = initPlayersCard gs1, deck = finalDeck gs1, d_stack = discardedExplodingCards gs1})

playersCount :: State -> Int
playersCount gs = length $ players gs

initActionAndCatCardCount:: State -> Int
initActionAndCatCardCount gs = 
    (*) 4 $ playersCount gs

initActionAndCatCardToDeal :: State -> [Hand]
initActionAndCatCardToDeal gs = 
    group (playersCount gs) (take (initActionAndCatCardCount gs) $ deck gs)

initPlayersCard :: State -> [Player]
initPlayersCard gs = 
    zipWith (\player cards -> player { hand = cards ++ [DefuseCard] }) (players gs) (initActionAndCatCardToDeal gs)

finalDeck :: State -> Deck
finalDeck gs = 
    drop (initActionAndCatCardCount gs) (deck gs) ++ defuseCardsToDeck gs ++ explodingCardsToDeck gs

defuseCardsToDeck :: State -> [Card]
defuseCardsToDeck gs = take ( 6 - playersCount gs) $ repeat DefuseCard

explodingCardsToDeck :: State -> [Card]
explodingCardsToDeck gs = take (playersCount gs - 1) $ repeat ExplodingCard

discardedExplodingCards :: State -> [Card]
discardedExplodingCards gs = take (4 - (playersCount gs - 1)) $ repeat ExplodingCard 


group :: Int -> [Card] -> [[Card]]
group _ [] = []
group n l
  | n > 0 = (take n l) : (group n (drop n l))
  | otherwise = error "Negative n"