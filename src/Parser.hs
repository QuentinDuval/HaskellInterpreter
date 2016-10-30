module Parser where

import Control.Arrow(first, second)
import Control.Applicative
import Control.Monad


--
-- Abstraction for a stream to be parsed
--

class IStream a where
  isEmpty :: a -> Bool
  front :: a -> Char
  popFront :: a -> a


instance IStream String where
  isEmpty = null
  front = head
  popFront = tail


--
-- Parser result
--

data ParseResult a
  = Success { result :: a }
  | Error { errorMsg :: String }
  deriving (Show, Eq, Ord, Functor)

instance Applicative ParseResult where
  pure a = Success a
  (Success f) <*> (Success a) = Success (f a)
  (Error msg) <*> _ = (Error msg)
  _ <*> (Error msg) = (Error msg)

instance Alternative ParseResult where
  empty = Error "Empty result"
  s@(Success a) <|> _ = s
  _ <|> result = result

instance Monad ParseResult where
  (Error msg) >>= _ = (Error msg)
  (Success v) >>= f = f v


--
-- Parser implementation
--

newtype Parser stream a
  = Parser { runParser :: stream -> ParseResult (a, stream) }

instance Functor (Parser stream) where
  fmap :: (a -> b) -> Parser stream a -> Parser stream b
  fmap f (Parser p) = Parser $ \s -> fmap (first f) (p s)

instance Applicative (Parser stream) where
  pure :: a -> Parser stream a
  pure a = Parser $ \s -> Success (a, s)
  (<*>) :: Parser stream (a -> b) -> Parser stream a -> Parser stream b
  (Parser pf) <*> (Parser pa) =
    Parser $ \s1 -> do
      (f, s2) <- pf s1
      (a, s3) <- pa s2
      pure (f a, s3)

instance Alternative (Parser stream) where
  empty :: Parser stream a
  empty = Parser $ \_ -> empty
  (<|>) :: Parser stream a -> Parser stream a -> Parser stream a
  (Parser p1) <|> (Parser p2) = Parser $ \s -> (p1 s) <|> (p2 s)

instance Monad (Parser stream) where
  (>>=) :: (Parser stream a) -> (a -> Parser stream b) -> Parser stream b
  (Parser pa) >>= f =
    Parser $ \s1 -> do
      (a, s2) <- pa s1
      runParser (f a) s2


--
-- Parser factories and combinators
--

parseChar :: (IStream s) => Parser s Char
parseChar =
  Parser $ \s ->
    if isEmpty s
      then Error "Empty stream!"
      else Success (front s, popFront s)

raiseError :: String -> Parser s a
raiseError msg = Parser $ \_ -> Error msg

skipSpace :: (IStream s) => Parser s ()
skipSpace = do
  c <- parseChar
  when (c /= ' ') $
    raiseError "No space found"


-- TEST DRIVER

testParser :: IO ()
testParser = do
  let p = (,) <$> (skipSpace *> parseChar) <*> parseChar
  print $ runParser p " Hello"


--
