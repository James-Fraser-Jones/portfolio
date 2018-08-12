import Control.Monad.State
import Hand

data Settings = Settings {a :: Int, b :: Int, m :: Int} deriving Show --settings for rng
type Seed = Int
type MyState = Seed

--------------------------------------------------------------------------------

deal :: StateT MyState IO Hand
deal = do
  randNum <- newRand
  return $ generate (randNum `mod` 3)

play :: StateT MyState IO ()
play = do
  hand <- deal

  lift $ putStr ("Hand: " ++ show (seeHand hand) ++ "\nChoose card 1, 2 or 3: ")
  input <- (lift getLine)
  let number = ((read :: String -> Int) input) - 1

  lift $ putStr ("You chose: " ++ show (number + 1) ++ "\nHand: " ++ show (seeHand hand) ++ "\nPress Enter to continue: ")
  lift getLine

  lift $ putStrLn "Monty reveals another card: "
  randNum <- newRand

  let options = filter getHiddenJokers hand --problem where hidden doesn't mean unchosen by the player
      number = randNum `mod` (length options)
      hand' = reveal number hand
  lift $ putStr ("Hand: " ++ show (seeHand hand') ++ "\nDo you swap? y or n: ")
  swapS <- (lift getLine)
  return ()

loop :: StateT MyState IO ()
loop = do
  play
  loop

main :: IO ()
main = do
  putStr "Input Seed: "
  seedS <- getLine
  let seed = (read :: String -> Int) seedS
  runStateT loop seed
  return ()
--------------------------------------------------------------------------------

s :: Settings
s = Settings 34893480 45875987 94894889

rand :: Seed -> Seed
rand seed = ((a s)*seed+(b s)) `mod` (m s)

newRand :: StateT MyState IO Seed
newRand = do
  seed <- get             --get the current state
  let newSeed = rand seed --make new seed using current seed and settings
  put newSeed             --replace the current seed with the new seed in the state
  return newSeed          --return the new seed
