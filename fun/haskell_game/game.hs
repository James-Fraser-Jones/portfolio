import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Interface.Environment
import qualified Data.Map.Strict as Map
import Data.Maybe

--------------------------------------------------------------------------------

type Board = Map.Map Key KeyState

keys :: [Key]
keys = [(Char 'q'), (Char 'w'), (Char 'a'), (Char 's'), (Char 'z'), (Char 'x')]

emptyBoard :: Board
emptyBoard = Map.empty

{-
startBoard :: Board
startBoard = foldr f emptyBoard keys
  where f key board = Map.insert key Up board
-}

putKey :: Key -> KeyState -> Board -> Board
putKey key state board = if key `elem` keys then Map.insert key state board else board

getKey :: Key -> Board -> KeyState
getKey = Map.findWithDefault Up

--------------------------------------------------------------------------------

data Pos = L
         | N
         | R
         deriving (Show, Eq, Read)

type Axis = (Key, Key, Pos)
type Axes = [Axis]

getPos :: KeyState -> KeyState -> Pos
getPos Up Up = N
getPos Down Up = L
getPos Up Down = R
getPos Down Down = N

updateAxis :: Board -> Axis -> Axis
updateAxis b (k1, k2, p) = (k1, k2, p')
  where p' = getPos s1 s2
        s1 = getKey k1 b
        s2 = getKey k2 b

third :: (a, b, c) -> c
third (a, b, c) = c

emptyAxes :: Axes
emptyAxes = [(Char 'q', Char 'w', N), (Char 'a', Char 's', N), (Char 'z', Char 'x', N)]

posToInt :: Pos -> Float
posToInt p = case p of
  L -> -1
  N -> 0
  R -> 1

--------------------------------------------------------------------------------

type World = (Color, Board, Axes)

windowSize = (800, 800)
background = black
fps = 60

initWorld :: World
initWorld = (makeColor 0 0 0 1, emptyBoard, emptyAxes)

showWorld :: World -> IO Picture
showWorld (c, _, _) = return $ Color c (circleSolid 160)

eventHandler :: Event -> World -> IO World
eventHandler (EventKey k s _ _) (c, b, a) = return (c, b', a')
  where b' = putKey k s b
        a' = (updateAxis b') <$> a
eventHandler _ w = return w

timeHandler :: Float -> World -> IO World
timeHandler n (c, b, a) = let (red,green,blue,opacity) = rgbaOfColor c
                              change@[x,y,z] = (((n/3)*).posToInt.third) <$> a
                              [r',g',b'] = (max 0) <$> (zipWith (+) [red,green,blue] change)
                          in do
                            putStrLn $ "r: " ++ show red ++ " g: " ++ show green ++ " b: " ++ show blue
                            return (makeColor r' g' b' opacity, b, a)

main = do
  (w, h) <- getScreenSize
  let (w', h') = windowSize
      midPoint = ((w-w') `div` 2, (h-h') `div` 2)
      window = InWindow "Game" windowSize midPoint
  playIO window background fps initWorld showWorld eventHandler timeHandler
