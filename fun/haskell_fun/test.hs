import System.IO.Unsafe
import Control.Monad
import Control.Monad.Reader
import Debug.Trace
import Data.List

showMe :: Show a => a -> ()
showMe = unsafePerformIO.putStrLn.show

test3 :: Show a => a -> a
test3 thing = seq (showMe thing) thing

--inspect is a very evil function which uses both unsafePerformIO
--and seq in order to print the value of any expression inside a pure function
inspectAny :: Show a => String -> a -> b -> b
inspectAny name thing = seq output
  where output = unsafePerformIO.putStrLn $ message
        message = ">>Inspect: " ++ name ++ " = " ++ show thing

inspect :: Show a => String -> a -> a
inspect = join.inspectAny

newtype Vector = Vector {getV :: [Float]} deriving (Eq, Show)
type Row = Vector
type Col = Vector
newtype Matrix = Matrix {getM :: [Row]} deriving (Eq, Show)

class Vec v where
  scale :: Float -> v -> v
  vecSum :: v -> v -> v
  vecLen :: v -> Int
  vecNegate :: v -> v

instance Vec Float where
  scale = (*)
  vecSum = (+)
  vecLen = const 1
  vecNegate = (0.0 -)

instance Vec Vector where
  scale f = Vector.(map (f *)).getV
  vecSum = (((Vector .) . (. getV)) . zipWith (+)) . getV
  vecLen = length.getV
  vecNegate = Vector.(map (0.0 -)).getV

{-
vecSum a b = (((((Vector .) . (. getV)) . zipWith (+)) . getV) a) b
vecSum a b = ((Vector .) . (. getV) . zipWith (+) . getV) a b

getV :: Vector -> [Float]
zipWith (+) :: [Float] -> [Float] -> [Float]
(.) :: (b -> c) -> (a -> b) -> (a -> c)
-}

{-
fmap :: (a -> b) -> m a -> m b
join :: m m a -> m a

bind :: m a -> (a -> m b) -> m b
bind ma f = join $ fmap f ma

instance Monad (e -> ) where
     fmap :: (a -> b) -> (e -> a) -> (e -> b)
     fmap = (.)

     join :: (e -> e -> a) -> e -> a
     join f e = f e e --join duplicates the argument so that it can be passed to multiple different functions which wish to "read" from the shared environment

     bind :: (e -> a) -> (a -> e -> b) -> e -> b
     bind ma f e = (f . ma) e e

     (f . ma) :: e -> e -> b
-}

fix' :: (a -> a) -> a
fix' f = f $ fix' f

traceTest = fmap (trace "hello" succ) [1..10]

triLine :: Int -> String
triLine n = let uns = (31 - n)
                sco = 1 + 2 * n
             in replicate uns '_' ++ replicate sco '1' ++ replicate uns '_'

sierpinski :: Int -> IO ()
sierpinski n = do
  let lines' = map triLine [0..31]
  mapM_ putStrLn lines'

sequ :: Monad m => [m a] -> m [a]
sequ [] = return []
sequ (x:xs) = do
  a <- x
  as <- sequ xs
  return $ a:as

sequ' :: Applicative f => [f a] -> f [a]
sequ' [] = pure []
sequ' (x:xs) = (:) <$> x <*> sequ' xs

repA :: Applicative m => Int -> m a -> m [a]
repA n = sequenceA . replicate n

{-
(:) :: a -> ([a] -> [a])
(<$>) :: (a -> b) -> f a -> f b
(<*>) :: f (a -> b) -> f a -> f b
sequ' :: [f a] -> f [a]
x :: f a
xs :: [f a]
-}
