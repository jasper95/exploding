module Game where

import Common

initGame :: Int -> State

initGame n = State { players = [ HPlayer { name = "Player " ++ show x, hand = [ ]} | x <- [1..n] ],
                     deck = fullDeck,
                     d_stack = [ ] }

-- TODO: Implement a method to setup the game
setupGame :: State -> IO State
setupGame gs = return (gs)
