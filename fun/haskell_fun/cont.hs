newtype Cont r a = Cont { runCont :: (a -> r) -> r }

idCont :: a -> Cont r a
idCont = Cont . flip ($)

idContTest :: a -> (a -> r) -> r
idContTest x k = (runCont (idCont x)) k

test1 :: String
test1 = idContTest 5 show

test2 :: Int
test2 = idContTest 5 (+3)

instance Monad (Cont r) where
  return = Cont . flip ($)
  ma >>= f = f a
    where a = undefined --I don't think any valid definition exists
