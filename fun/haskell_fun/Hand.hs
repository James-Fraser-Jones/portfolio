module Hand where

import Data.List
import Data.Maybe

type Card = (Bool, Bool)
type Hand = [Card]

seeCard :: Card -> String
seeCard (_, False) = "*****"
seeCard (False, _) = "Joker"
seeCard _ = "Ace!!"

seeHand :: Hand -> [String]
seeHand = map seeCard

target :: Bool -> Int -> Hand -> Hand
target b 0 ((f,_):a) = (f,b):a
target b 1 (f:(s,_):a) = f:(s,b):a
target b 2 (f:s:(t,_):a) = f:s:(t,b):a

reveal :: Int -> Hand -> Hand
reveal = target True

revealAll :: Hand -> Hand
revealAll = map (const True <$>)

hide :: Int -> Hand -> Hand
hide = target False

hideAll :: Hand -> Hand
hideAll = map (const False <$>)

generate :: Int -> Hand
generate n =
  let joker = (False, False)
      ace = (True, False)
  in case n of
    0 -> [ace, joker, joker]
    1 -> [joker, ace, joker]
    2 -> [joker, joker, ace]

getAce :: Hand -> Int
getAce h = fromJust $ elemIndex True (fst <$> h)

getJokers :: Hand -> [Int]
getJokers h = elemIndices False (fst <$> h)

getHiddenJokers :: Hand -> [Int] --do mod the (length of list -1) in order to choose from it randomly
getHiddenJokers = elemIndices (False, False)
