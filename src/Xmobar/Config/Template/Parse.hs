{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Xmobar.Config.Template.Parse
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

module Xmobar.Config.Template.Parse ( parseString
                                    , runTemplateParser
                                    , evalTemplateParser
                                    , allParsers
                                    , emptyParseState
                                    , emptyFormat
                                    , Parser
                                    , ParseState(..)
                                    , ConfigTemplate(..)
                                    , Bar(..)
                                    , barParser
                                    , allSegments
                                    , Seg(..)
                                    , Format(..)
                                    , Box(..)
                                    , BoxBorder(..)
                                    , BoxOffset(..)
                                    , BoxMargins(..)
                                    , TextRenderInfo(..)
                                    , Widget(..)
                                    , RunnableWidget(..)
                                    , ParseError
                                    ) where

import Xmobar.X11.Actions
import Xmobar.Config.Align
import Xmobar.Run.Command
import Xmobar.Run.Runnable
import Xmobar.Run.Exec

import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Int (Int32)
import Data.UUID
import Text.Parsec
import Text.Read (readMaybe)
import Graphics.X11.Types (Button)
import Foreign.C.Types (CInt)
import System.Random

import Control.Monad.State.Strict as MTL

data Bar = Bar { left, center, right :: [Seg] }
  deriving (Show)

allSegments :: Bar -> [Seg]
allSegments Bar { left, center, right } = left <> center <> right

type Parser a = ParsecT String ParseState (MTL.State StdGen) a

data ParseState = ParseState { formatState :: Format
                             , alignSep :: String
                             , sepChar :: String
                             , commands :: [Runnable]
                             }

emptyParseState :: String -> String -> String -> [Runnable] -> ParseState
emptyParseState fgColor sepChar alignSep commands = ParseState
  { formatState = emptyFormat fgColor
  , sepChar
  , alignSep
  , commands
  }

data  Format = Format { fontIndex :: FontIndex
                      , textRenderInfo :: TextRenderInfo
                      , actions :: Maybe [Action]
                      } deriving (Show)

emptyFormat :: String -> Format
emptyFormat fgColor = Format { fontIndex = 0
                             , textRenderInfo = emptyTextRenderInfo fgColor
                             , actions = Nothing
                             }

data ConfigTemplate = Unparsed String | Parsed Bar
  deriving (Show)

instance Read ConfigTemplate where
  readsPrec i = fmap (\(x, s) -> (Unparsed x, s)) . readsPrec i

data Seg = Seg { widget :: Widget
               , format :: Format
               } deriving (Show)

data Widget = Icon String
            | Text String
            | Hspace Int32
            | Runnable RunnableWidget
  deriving (Show)

data RunnableWidget = RunnableWidget { runnableId :: UUID
                                     , com :: Runnable
                                     , res , suf , pref :: String
                                     } deriving (Show)

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
                   } deriving (Eq, Show)

emptyTextRenderInfo :: String -> TextRenderInfo
emptyTextRenderInfo fgColor = TextRenderInfo fgColor 0 0 []

type FontIndex   = Int

-- | Runs the string parser
runTemplateParser :: Parser a -> StdGen -> ParseState -> String -> (Either ParseError a, StdGen)
runTemplateParser p g initial s = runParserT p initial mempty s `MTL.runState` g

-- | Runs the string parser, discarding the StdGen
evalTemplateParser :: Parser a -> StdGen -> ParseState -> String -> Either ParseError a
evalTemplateParser p g initial s = runParserT p initial mempty s `MTL.evalState` g

parseString :: StdGen -> ParseState -> String -> Either ParseError Bar
parseString = evalTemplateParser barParser

defaultAlign :: String
defaultAlign = "}{"

barParser :: Parser Bar
barParser = do
  ParseState { alignSep } <- getState
  let [alignSepL, alignSepR] = if length alignSep == 2 then alignSep else defaultAlign
      allParsersTill p = concat <$> manyTill allParsers p

  (try (Bar <$> allParsersTill (char alignSepL)
            <*> allParsersTill (char alignSepR)
            <*> allParsersTill eof)
   <|> (Bar <$> allParsersTill eof <*> mempty <*> mempty))

allParsers :: Parser [Seg]
allParsers = textParser
                <|> try iconParser
                <|> try hspaceParser
                <|> try rawParser
                <|> try actionParser
                <|> try fontParser
                <|> try boxParser
                <|> colorParser

-- | Parses a maximal string without markup.
textParser :: Parser [Seg]
textParser = do
  st@ParseState { formatState = format, alignSep } <- getState
  let aligners = if length alignSep == 2 then alignSep else defaultAlign
  s <- many1 $
       noneOf ("<" <> aligners) <|>
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

  g <- lift get
  let (rw, g') = runTemplateParser runnableWidgetParser g st s
  lift (put g')
  return [ Seg { widget = either (const (Text s)) Runnable rw
               , format
               }
         ]

allTillSep :: String -> Parser String
allTillSep = many . noneOf

-- | Parses the output template string for a runnable widget
runnableWidgetParser :: Parser RunnableWidget
runnableWidgetParser = do
  ParseState { sepChar, commands } <- getState

  pref <- allTillSep sepChar
  res  <- templateCommandParser
  suf  <- allTillSep sepChar

  let word32 = lift (MTL.state genWord32)
      com = fromMaybe (Run (Com res [] [] 10))
            $ find ((==) res . alias) commands

  runnableId <- fromWords <$> word32 <*> word32 <*> word32 <*> word32

  return RunnableWidget { runnableId, com, res, pref, suf }

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
      return [Seg { widget, format }]
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
  return [Seg { widget, format }]

hspaceParser :: Parser [Seg]
hspaceParser = do
  format <- formatState <$> getState
  string "<hspace="
  pVal <- manyTill digit (try (string "/>"))
  let widget = Hspace (fromMaybe 0 $ readMaybe pVal)

  return [Seg { widget, format }]

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
