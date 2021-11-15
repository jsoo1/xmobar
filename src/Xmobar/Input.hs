module Xmobar.Input where

import Text.Parsec (many, many1, try, (<|>))
import Xmobar.Config.Template.Parse

-- | Items received from a user like template strings, or output from
-- | Runnables
data Input a = InputRaw String | InputParsed a
  deriving (Show)

-- | Read is here because of the use of Input in the Config
instance Read (Input a) where
  readsPrec i = fmap (\(x, s) -> (InputRaw x, s)) . readsPrec i

-- | Possible output from Runnables. A StdinReader, for instance,
-- | might pipe in the whole bar.
data New = NewSegs [PlainSeg] | NewBar (Bar [PlainSeg])

parseNew :: Parser New
parseNew =
  try (NewSegs <$> many1 segParser')
  <|> (NewBar  <$> barParser (many plainSegParser))
