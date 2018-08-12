import Control.Monad.State
--return, (>>=), get, put, State, runState, evalState, execState

------------------------------------------------------------------

data RandSettings = RandSettings {a :: Int, b :: Int, m :: Int}
type Seed = Int

rand :: RandSettings -> Seed -> Seed
rand r s = ((a r)*s + (b r)) `mod` (m r)

------------------------------------------------------------------
--This method stores the list inside the state itself, always returns unit and uses the state containing the list as the
--final result of the computation

type RandState = (RandSettings,[Seed])

getRand :: State RandState ()
getRand = do
  (settings, seeds) <- get
  let seed = head seeds
  let newSeed = rand settings seed
  put $ (settings, newSeed:seeds)
  return ()

getRandList :: Int -> State RandState ()
getRandList 0 = return ()
getRandList n = do
  getRand
  getRandList (n-1)

randList :: Int -> RandSettings -> Seed -> [Seed]
randList n settings seed = snd $ execState (getRandList n) (settings,[seed])

------------------------------------------------------------------
--This method explicitly passes around the list and returns it as a value which is used as the
--final result of the computation

getRand' :: [Seed] -> State RandSettings [Seed]
getRand' seeds = do
  settings <- get
  let seed = head seeds
  let newSeed = rand settings seed
  return $ newSeed:seeds

getRandList' :: [Seed] -> Int -> State RandSettings [Seed]
getRandList' seeds 0 = return seeds
getRandList' seeds n = do
  newSeeds <- getRand' seeds
  getRandList' newSeeds (n-1)

randList' :: Int -> RandSettings -> Seed -> [Seed]
randList' n settings seed = evalState (getRandList' [seed] n) settings
