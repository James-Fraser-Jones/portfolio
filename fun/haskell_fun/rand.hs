import Control.Monad.State
--------------------------------------------------------------------------------
--Pure data structures and functions:

data Settings = Settings {a :: Int, b :: Int, m :: Int} deriving Show
type Seed = Int --used to distinguish random numbers from other integers
type RandState = (Seed, Settings)

--the cool thing is that the implementation for how random numbers are generated
--can be modified here without having to change any of the monadic functions that rely on it
rand :: RandState -> Seed
rand (seed, s) = ((a s)*seed+(b s)) `mod` (m s)

--------------------------------------------------------------------------------
--Stateful function using the State Monad:

--use the state to get a new seed, update state accordingly and return new seed
newRand :: State RandState Seed
newRand = do
  state @ (seed, s) <- get --get the current state
  let newSeed = rand state --make new seed using current seed and settings
  put (newSeed, s)         --replace the current seed with the new seed in the state
  return newSeed           --return the new seed

--n is number of random numbers you wish to get, seed is initial seed, a b and m are coefficients to random number function
getRand :: Int -> Int -> Int -> Int -> Seed -> [Seed]
getRand n a b m seed = let state = (seed, Settings a b m)
                           main = sequence (replicate n newRand) --using a list of n "rand" functions, sequence produces a State RandState [Seed]
                       in evalState main state                   --which is the result of running each of these monadic computations in sequence
                                                                 --hence allowing previous seeds to be correctly used to generate new ones

--------------------------------------------------------------------------------
--Testing

test1, test2 :: [Seed]
test1 = getRand 10 34893480 45875987 94894889 1
test2 = getRand 10 34893480 45875987 94894889 80769467
