module Game where

import Common
import Shuffler

initGame :: Int -> State

initGame n = State { players = [ HPlayer { name = "Player " ++ show x, hand = [ ]} | x <- [1..n] ],
                     deck = fullDeck,
                     d_stack = [ ] }


setupGame :: State -> IO State
--1. Remove all the Exploding Kittens (4) and Defuse cards (6) from the deck.
--2. Shuffle the remaining deck and deal 4 cards face down to each player.
--3. Deal 1 Defuse card to each player so that everyone has a hand of 5 cards total. Keep your hand secret
--4. Insert enough Exploding Kittens back into the deck so that there is one fewer than the number of people playing. This ensures that everyone eventually explodes except for one person.
-- Remove any extra Exploding Kittens from the game.
--5. Insert the extra Defuse cards back in the deck.
--6. Shuffle the deck
setupGame gs = do
    gs1 <- shuffleDeck (gs {deck = deckNoExplDef})
    let splitDeck  = splitAt deckToDealCount $ deck gs1
    let deckToDeal = group (length (players gs1)) (fst splitDeck)
    let playersWithCards = map (\player -> player {hand = DefuseCard:(hand player) }) $ zipWith (\player cards -> player { hand = cards}) (players gs1) deckToDeal
    let finalDeck = (snd splitDeck) ++ (take (playersCount - 1) $ repeat ExplodingCard) ++ remainingDef
    let discardPile = remainingExpl
    shuffleDeck (gs1 {players = playersWithCards, deck= finalDeck, d_stack = remainingExpl})
    where
        initialCardsEach = 4
        playersCount = length $ players gs
        defuseCount = length $ getCards (deck gs) DefuseCard
        explCount = length $ getCards (deck gs) ExplodingCard
        deckToDealCount = playersCount * initialCardsEach 
        deckNoExplDef = removeCardsByType DefuseCard $ removeCardsByType ExplodingCard $ deck gs
        remainingExpl = take (explCount - (playersCount - 1)) $ repeat ExplodingCard
        remainingDef = take (defuseCount - playersCount) $ repeat DefuseCard

removeCardsByType :: Card -> [Card] -> [Card]
removeCardsByType x xs = [ x' | x' <- xs, x' /= x]

group :: Int -> [Card] -> [[Card]]
group _ [] = []
group n l
  | n > 0 = (take n l) : (group n (drop n l))
  | otherwise = error "Negative n"