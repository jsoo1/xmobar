{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Xmobar.Parsers
-- Copyright   :  (c) Andrea Rossato
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Jose A. Ortega Ruiz <jao@gnu.org>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Parsing for template substrings
--
-----------------------------------------------------------------------------

module Xmobar.X11.Parsers ( parseString
                          , emptyParseState
                          , emptyFormat
                          , ParseState(..)
                          , Seg(..)
                          , Format(..)
                          , Box(..)
                          , BoxBorder(..)
                          , BoxOffset(..)
                          , BoxMargins(..)
                          , TextRenderInfo(..)
                          , Widget(..)
                          , ParseError
                          ) where

import Xmobar.X11.Actions
import Xmobar.Config.Align
import Xmobar.Run.Command
import Xmobar.Run.Runnable
import Xmobar.Run.Exec

import Control.Monad (guard, mzero)
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Int (Int32)
import Text.Parsec
import Text.Read (readMaybe)
import Graphics.X11.Types (Button)
import Foreign.C.Types (CInt)

type Parser = Parsec String ParseState

data ParseState = ParseState { formatState :: Format
                             , sepChar :: String
                             , commands :: [Runnable]
                             }

emptyParseState :: String -> String -> [Runnable] -> ParseState
emptyParseState fgColor sepChar commands = ParseState
  { formatState = emptyFormat fgColor
  , sepChar
  , commands
  }

data  Format = Format { fontIndex :: FontIndex
                      , textRenderInfo :: TextRenderInfo
                      , actions :: Maybe [Action]
                      }

emptyFormat :: String -> Format
emptyFormat fgColor = Format { fontIndex = 0
                             , textRenderInfo = emptyTextRenderInfo fgColor
                             , actions = Nothing
                             }

data Seg = Seg { widget :: Widget
               , format :: Format
               , runnable :: Maybe (Runnable, String, String)
               }

data Widget = Icon String | Text String | Hspace Int32

data BoxOffset = BoxOffset Align Int32 deriving (Eq, Show)
-- margins: Top, Right, Bottom, Left
data BoxMargins = BoxMargins Int32 Int32 Int32 Int32 deriving (Eq, Show)
data BoxBorder = BBTop
               | BBBottom
               | BBVBoth
               | BBLeft
               | BBRight
               | BBHBoth
               | BBFull
                 deriving ( Read, Eq, Show )
data Box = Box BoxBorder BoxOffset CInt String BoxMargins deriving (Eq, Show)
data TextRenderInfo =
    TextRenderInfo { tColorsString   :: String
                   , tBgTopOffset    :: Int32
                   , tBgBottomOffset :: Int32
                   , tBoxes          :: [Box]
                   } deriving Show

emptyTextRenderInfo :: String -> TextRenderInfo
emptyTextRenderInfo fgColor = TextRenderInfo fgColor 0 0 []

type FontIndex   = Int

-- | Runs the string parser
parseString :: ParseState -> String -> Either ParseError [Seg]
parseString initial =
  runParser (fmap concat stringParser) initial mempty

allParsers :: Parser [Seg]
allParsers = textParser
                <|> try iconParser
                <|> try hspaceParser
                <|> try rawParser
                <|> try actionParser
                <|> try fontParser
                <|> try boxParser
                <|> colorParser

-- | Gets the string and combines the needed parsers
stringParser :: Parser [[Seg]]
stringParser = manyTill allParsers eof

-- | Parses a maximal string without markup.
textParser :: Parser [Seg]
textParser = do
  state@ParseState { formatState = format, commands } <- getState
  s <- many1 $
       noneOf "<" <|>
         try (notFollowedBy' (char '<')
              (try (string "fc=")  <|>
                try (string "box")  <|>
                try (string "fn=")  <|>
                try (string "action=") <|>
                try (string "/action>") <|>
                try (string "icon=") <|>
                try (string "hspace=") <|>
                try (string "raw=") <|>
                try (string "/fn>") <|>
                try (string "/box>") <|>
                string "/fc>"))

  let sub = runParser templateStringParser state mempty s
      widget = Text s
  return $ case sub of
    Left _ ->
      [ Seg { runnable = Nothing, format, widget } ]

    Right (com, prefix, suffix) ->
      [ Seg { runnable = Just (r, prefix, suffix), format, widget } ]
        where r = fromMaybe (Run (Com com [] [] 10)) $
                  find ((==) com . alias) commands

allTillSep :: String -> Parser String
allTillSep = many . noneOf

-- | Parses the output template string
templateStringParser :: Parser (String,String,String)
templateStringParser = do
  ParseState { sepChar } <- getState
  s   <- allTillSep sepChar
  com <- templateCommandParser
  ss  <- allTillSep sepChar
  return (com, s, ss)

-- | Parses the command part of the template string
templateCommandParser :: Parser String
templateCommandParser = do
  ParseState { sepChar } <- getState
  let chr = char (head sepChar)
  between chr chr (allTillSep sepChar)

-- | Parse a "raw" tag, which we use to prevent other tags from creeping in.
-- The format here is net-string-esque: a literal "<raw=" followed by a
-- string of digits (base 10) denoting the length of the raw string,
-- a literal ":" as digit-string-terminator, the raw string itself, and
-- then a literal "/>".
rawParser :: Parser [Seg]
rawParser = do
  format <- formatState <$> getState
  string "<raw="
  lenstr <- many1 digit
  char ':'
  case reads lenstr of
    [(len,[])] -> do
      guard ((len :: Integer) <= fromIntegral (maxBound :: Int))
      widget <- Text <$> count (fromIntegral len) anyChar
      string "/>"
      return [Seg { widget, format, runnable = Nothing }]
    _ -> mzero

-- | Wrapper for notFollowedBy that returns the result of the first parser.
--   Also works around the issue that, at least in Parsec 3.0.0, notFollowedBy
--   accepts only parsers with return type Char.
notFollowedBy' :: Parser a -> Parser b -> Parser a
notFollowedBy' p e = do x <- p
                        notFollowedBy $ try (e >> return '*')
                        return x

iconParser :: Parser [Seg]
iconParser = do
  format <- formatState <$> getState
  string "<icon="
  widget <- Icon <$> manyTill (noneOf ">") (try (string "/>"))
  return [Seg { widget, format, runnable = Nothing }]

hspaceParser :: Parser [Seg]
hspaceParser = do
  format <- formatState <$> getState
  string "<hspace="
  pVal <- manyTill digit (try (string "/>"))
  let widget = Hspace (fromMaybe 0 $ readMaybe pVal)

  return [Seg { widget, format, runnable = Nothing }]

actionParser :: Parser [Seg]
actionParser = do
  act <- actions . formatState <$> getState
  string "<action="
  command <- choice [between (char '`') (char '`') (many1 (noneOf "`")),
                   many1 (noneOf ">")]
  buttons <- (char '>' >> return "1") <|> (space >> spaces >>
    between (string "button=") (string ">") (many1 (oneOf "12345")))
  let a = Spawn (toButtons buttons) command
      a' = case act of
        Nothing -> Just [a]
        Just act' -> Just $ a : act'
  modifyState (\s -> s { formatState = (formatState s) { actions = a' } })
  s <- manyTill allParsers (try $ string "</action>")
  return (concat s)

toButtons :: String -> [Button]
toButtons = map (\x -> read [x])

-- | Parsers a string wrapped in a color specification.
colorParser :: Parser [Seg]
colorParser = do
  oldRenderInfo@(TextRenderInfo _ _ _ bs) <- textRenderInfo . formatState <$> getState
  c <- between (string "<fc=") (string ">") colors
  let colorParts = break (==':') c
      (ot,ob) = case break (==',') (Prelude.drop 1 $ snd colorParts) of
             (top,',':btm) -> (top, btm)
             (top,      _) -> (top, top)
      renderInfo = TextRenderInfo (fst colorParts) (fromMaybe (-1) $ readMaybe ot) (fromMaybe (-1) $ readMaybe ob) bs
  modifyState (\st -> st { formatState = (formatState st) { textRenderInfo = renderInfo } })
  s <- manyTill allParsers (try $ string "</fc>")
  modifyState (\st -> st { formatState = (formatState st) { textRenderInfo = oldRenderInfo } })
  return (concat s)

-- | Parses a string wrapped in a box specification.
boxParser :: Parser [Seg]
boxParser = do
  TextRenderInfo cs ot ob bs <- textRenderInfo . formatState <$> getState
  c <- between (string "<box") (string ">") (option "" (many1 (alphaNum <|> char '=' <|> char ' ' <|> char '#' <|> char ',')))
  let b = Box BBFull (BoxOffset C 0) 1 cs (BoxMargins 0 0 0 0)
      g = boxReader b (words c)
      renderInfo = TextRenderInfo cs ot ob (g : bs)
  modifyState (\s -> s { formatState = (formatState s) { textRenderInfo = renderInfo } })
  s <- manyTill allParsers (try $ string "</box>")
  return (concat s)

boxReader :: Box -> [String] -> Box
boxReader b [] = b
boxReader b (x:xs) = do
  let (param,val) = case break (=='=') x of
                 (p,'=':v) -> (p, v)
                 (p,    _) -> (p, "")
  boxReader (boxParamReader b param val) xs

boxParamReader :: Box -> String -> String -> Box
boxParamReader b _ "" = b
boxParamReader (Box bb off lw fc mgs) "type" val =
  Box (fromMaybe bb $ readMaybe ("BB" ++ val)) off lw fc mgs
boxParamReader (Box bb (BoxOffset alg off) lw fc mgs) "offset" (a:o) =
  Box bb (BoxOffset (fromMaybe alg $ readMaybe [a]) (fromMaybe off $ readMaybe o)) lw fc mgs
boxParamReader (Box bb off lw fc mgs) "width" val =
  Box bb off (fromMaybe lw $ readMaybe val) fc mgs
boxParamReader (Box bb off lw _ mgs) "color" val =
  Box bb off lw val mgs
boxParamReader (Box bb off lw fc mgs@(BoxMargins mt mr mb ml)) ('m':pos) val = do
  let mgs' = case pos of
         "t" -> BoxMargins (fromMaybe mt $ readMaybe val) mr mb ml
         "r" -> BoxMargins mt (fromMaybe mr $ readMaybe val) mb ml
         "b" -> BoxMargins mt mr (fromMaybe mb $ readMaybe val) ml
         "l" -> BoxMargins mt mr mb (fromMaybe ml $ readMaybe val)
         _ -> mgs
  Box bb off lw fc mgs'
boxParamReader b _ _ = b

-- | Parsers a string wrapped in a font specification.
fontParser :: Parser [Seg]
fontParser = do
  f <- between (string "<fn=") (string ">") colors
  let fontNum = fromMaybe 0 $ readMaybe f
  modifyState (\s -> s { formatState = (formatState s) { fontIndex = fontNum } })
  s <- manyTill allParsers (try $ string "</fn>")
  return (concat s)

-- | Parses a color specification (hex or named)
colors :: Parser String
colors = many1 (alphaNum <|> char ',' <|> char ':' <|> char '#')
