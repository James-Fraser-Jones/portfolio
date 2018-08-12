import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Interface.Environment
import qualified Data.Map.Strict as Map

type Inputs = Map.Map Key KeyState
type World = Inputs

background = black

drawNothing :: World -> IO Picture
drawNothing = const $ return Blank

initWorld :: World
initWorld = Map.empty

gameKeys :: [Key]
gameKeys = [(Char 'q'), (Char 'w'), (Char 'a'), (Char 's'), (Char 'z'), (Char 'x')]

windowSize = (800, 800)

fps = 60

--{-
main = do
  (w, h) <- getScreenSize
  let (w', h') = windowSize
      midPoint = ((w-w') `div` 2, (h-h') `div` 2)
      window = InWindow "Game" windowSize midPoint
  playIO window background fps initWorld drawNothing eventHandler timeHandler
--}

eventHandler :: Event -> World -> IO World
eventHandler (EventKey key state _ _) w = return w'
  where w' = if key `elem` gameKeys then Map.insert key state w else w
eventHandler _ w = return w

timeHandler :: Float -> World -> IO World
timeHandler _ w = do
  putStrLn $ show w
  return w
