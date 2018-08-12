import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Interface.Environment

--basically don't model or draw anything
background = black
initWorld = ()
drawNothing = const $ return Blank

--change these for testing
windowSize = (800, 800)
fps = 2

main = do
  (w, h) <- getScreenSize
  let (w', h') = windowSize
      midPoint = ((w-w') `div` 2, (h-h') `div` 2)
      window = InWindow "Game" windowSize midPoint
  playIO window background fps initWorld drawNothing printer ignorer

{-
event and time printing functions:

when printing time, it will effectively print (1/fps) every (1/fps) seconds

when printing events, it will print each time any button is pressed or released,
as well as modifiers held at that time along with the mouse co-ordinates when it triggered

it will also print each time the mouse position changes and its new co-ordinates

response to events is NOT dependent on fps, it happens instantly
-}
printer e w = (putStrLn $ show e) >> return w
ignorer = const return
