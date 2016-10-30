module Parser where

import Control.Arrow(second)


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
-- Parser implementation
--

data ParseResult a
  = Success { result :: a }
  | Error { errorMsg :: String }
  deriving (Show, Eq, Ord, Functor)

newtype Parser stream a
  = Parser { runParser :: stream -> ParseResult (stream, a) }

instance (IStream stream) => Functor (Parser stream) where
  fmap :: (a -> b) -> Parser stream a -> Parser stream b
  fmap f (Parser p) = Parser $ \s -> fmap (second f) (p s)

-- instance (IStream stream) => Applicative (Pa)

--
