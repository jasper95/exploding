module Shuffler where

import Common
import System.Random.Shuffle

shuffleDeck :: State -> IO State


shuffleDeck state = do
    newDeck <- shuffleM (deck state)
    return ( state { deck = newDeck})
