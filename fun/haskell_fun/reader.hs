import Prelude hiding (return, (>>=))

--pretending to make a new datatype with a monad instance here, avoiding this allows us to avoid having to unwrap functions
type Reader e a = (e -> a)

--simplest reader instance of bind
(>>=) :: Reader e a -> (a -> Reader e b) -> Reader e b
m >>= k = (\r -> k (m r) r)
--m >>= k  = Reader $ \r -> runReader (k (runReader m r)) r (this is the definition with the Reader wrapper)

--simplest reader instance of return
return :: a -> Reader e a
return = const
--return a = Reader $ const a (this is the definition with the Reader wrapper)

--ask just lets you access the environment because it will produce the input from the original function
ask :: Reader e e
ask = id
--ask = Reader id (this is the definition with the Reader wrapper)

greeter :: Reader String String --greeter is a function from an input string to an output string
greeter = do
  name <- ask --ask will always return the input string
  return $ "hello " ++ name ++ "!"

greeter2 = ask >>= (\name -> return $ "hello " ++ name ++ "!")

greeter3 = (\e -> (\name -> const $ "hello " ++ name ++ "!") e e)
greeter4 = (\e -> (const $ "hello " ++ e ++ "!") e)
greeter5 = (\e -> "hello " ++ e ++ "!")
