import Control.Applicative
import Control.Monad
import Data.List
import System.IO

import Data.Maybe

type Perm = [Int]
type Fac = Int

--------------------------------------------------------------------------------
--General use functions
fac :: Int -> Int
fac n = product [1 .. n]

extract :: Int -> [a] -> (a, [a])
extract n xs = let (a, b) = splitAt n xs in (xs !! n, a ++ tail b)

insertAt :: (a -> Bool) -> [a] -> [a] -> [a]
insertAt _ [] _  = [] --too many items to insert
insertAt _ xs [] = xs --ran out of items to insert
insertAt b (x:xs) i@(y:ys) = if b x then y : insertAt b xs ys else x : insertAt b xs i --insert items
--------------------------------------------------------------------------------
decToFac :: Int -> Fac --convert decimal number to factorial number
decToFac n =
  let fun = (\(x, i) -> if i == -1 then Nothing else let (d, r) = quotRem x i in Just (r, (d, if d == 0 then -1 else succ i)))
      digits = reverse $ unfoldr fun (n, 1)
   in ((read :: String -> Int).concat) $ show <$> digits
--------------------------------------------------------------------------------
getPerm :: [Int] -> [Int] -> Perm -> Perm
getPerm [] [] p = p
getPerm (x:xs) o p =
  let (y, ys) = extract x o
   in getPerm xs ys (p ++ [y])

decToPerm :: Int -> Int -> Perm --given a number of unique digits (starting from 0), and the nth permutation of these (also starting from 0 and in lexicographic order)
decToPerm l n = --return the permutation
  let order = [0 .. l]
      facN = decToFac n
      digits = (read :: String -> Int).(:[]) <$> (show facN)
      diff = (length order) - (length digits)
   in getPerm ((replicate diff 0) ++ digits) order []

allPerms :: Int -> [Perm] --given a number of unique digits (starting from 0)
allPerms l = (decToPerm l) <$> [0 .. fac (l+1) - 1]

permToInt' :: Perm -> Maybe Int
permToInt' p =
  let l = (length p) - 1
   in elemIndex p (allPerms l)

facToInt :: Fac -> Int --convert factorial number to decimal number
facToInt n =
 let digits = (read :: String -> Int).(:[]) <$> (show n)
     fun = (\d (s, i) -> (s + d * fac i, succ i))
  in fst $ foldr fun (0, 0) digits
--------------------------------------------------------------------------------
--This is actually half the problem right here, just getting a list of all the permutations it could be. We save a lot of hassle by using haskell's
--permutations function because, unlike the next part of the problem, this list of permutations does not need to be lexicographically ordered
possiblePerms :: Perm -> [Perm] --given a permutation of integers from 1 to n with 0's indicating empty spaces, return all possible permutations of missing numbers
possiblePerms p =
  let missing = foldr delete [1 .. length p] p
  in (insertAt (==0) p) <$> (permutations missing)

--This is the other half of the problem
permToInt :: Perm -> Int
permToInt p =
  let digits = reverse $ facDigits p [1 .. length p] []
      fun = (\d (s, i) -> (s + d * fac i, succ i))
  in  (fst $ foldr fun (0, 0) digits) + 1

facDigits :: [Int] -> [Int] -> [Int] -> [Int]
facDigits [] [] fac = fac
facDigits (x:xs) order fac =
  let n = fromJust $ elemIndex x order
      (o1,o2) = splitAt n order
   in facDigits xs (o1 ++ tail o2) (n : fac) --gives answer in reverse to allow for the use of cons
