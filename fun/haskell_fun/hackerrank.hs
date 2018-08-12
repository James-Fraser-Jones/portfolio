import Control.Applicative
import Control.Monad
import System.IO

import Data.List
import Data.Maybe
type Perm = [Int]

--One half of the problem, given a permutation of integers from 1 to n with 0's indicating empty spaces, return all possible permutations with missing numbers filled
possiblePerms :: Perm -> [Perm] --
possiblePerms p =
  let missing = foldr delete [1 .. length p] p
  in (insertAt (==0) p) <$> (permutations missing)

insertAt :: (a -> Bool) -> [a] -> [a] -> [a]
insertAt _ [] _  = [] --too many items to insert
insertAt _ xs [] = xs --ran out of items to insert
insertAt b (x:xs) i@(y:ys) = if b x then y : insertAt b xs ys else x : insertAt b xs i --insert items

--The other half of the problem, returns the row number of any permutation if ordered lexicographically, with numbers 1 to n
permToInt :: Perm -> Int
permToInt p =
  let digits = reverse $ facDigits p [1 .. length p] []
      fac n = product [1 .. n]
      fun = (\d (s, i) -> (s + d * fac i, succ i))
  in  (fst $ foldr fun (0, 0) digits) + 1

facDigits :: [Int] -> [Int] -> [Int] -> [Int]
facDigits [] [] fac = fac
facDigits (x:xs) order fac =
  let n = fromJust $ elemIndex x order
      (o1,o2) = splitAt n order
   in facDigits xs (o1 ++ tail o2) (n : fac) --gives answer in reverse to allow for the use of cons

answer :: Perm -> Int
answer p = foldr madd 0 (permToInt <$> possiblePerms p)
  where madd x y = (x + y) `mod` (10 ^ 9 + 7)

main :: IO ()
main = do
    n_temp <- readFile "test_input_1.txt"
    let a_temp = (lines n_temp) !! 1
    let a = map read $ words a_temp :: [Int]
    let ans = answer a
    putStrLn $ show ans
    return ()
