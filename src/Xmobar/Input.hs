module Xmobar.Input where

data Input a = InputRaw String | InputParsed a
  deriving (Show)

instance Read (Input a) where
  readsPrec i = fmap (\(x, s) -> (InputRaw x, s)) . readsPrec i
