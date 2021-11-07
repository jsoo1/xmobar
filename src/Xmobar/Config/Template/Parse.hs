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

module Xmobar.Config.Template.Parse where

import Xmobar.Config.Actions
import Xmobar.Config.Align
import Xmobar.Run.Command
import Xmobar.Run.Runnable
import Xmobar.Run.Exec

import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Int (Int32)
import Text.Parsec hiding (runParser)
import Text.Read (readMaybe)
import Graphics.X11.Types (Button)
import Foreign.C.Types (CInt)

import qualified Control.Monad.State.Strict as MTL
import qualified Control.Monad.Reader as MTL


data Bar = Bar { left, center, right :: [Seg] }
  deriving (Show)

allSegments :: Bar -> [Seg]
allSegments Bar { left, center, right } = left <> center <> right

plainSegments :: Seg -> [PlainSeg]
plainSegments (Plain s)     = pure s
plainSegments (Runnable rw) = val rw

type Parser a = ParsecT String ParseState (MTL.Reader ParseState) a

data ParseState = ParseState { formatState :: Format
                             , stack :: [Tag]
                             , alignSep :: String
                             , sepChar :: String
                             , commands :: [Runnable]
                             }

-- | context-free tags: foreground-color, font, actions and box, respectively
data Tag = Fc | Fn | Act | Bx | Hsp deriving (Eq)

closeTag :: Tag -> String
closeTag Fc  = "</fc>"
closeTag Fn  = "</fn>"
closeTag Act = "</action>"
closeTag Bx  = "</box>"
closeTag Hsp = "</hspace>"

push :: Tag -> Parser ()
push t = modifyState (\s@ParseState { stack } -> s { stack = t : stack })

pop :: Tag -> Parser ()
pop t = do
  tags <- stack <$> getState
  case tags of
    (t':stack) | t == t' -> modifyState (\s -> s { stack })

    (t':_) | otherwise ->
      fail ("unexpected closing tag, expected " <> closeTag t <> ", got: " <> closeTag t')

    [] ->
      fail ("missing closing tag " <> closeTag t)


emptyParseState :: String -> String -> String -> [Runnable] -> ParseState
emptyParseState fgColor sepChar alignSep commands = ParseState
  { formatState = emptyFormat fgColor
  , stack = []
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

data Seg = Plain PlainSeg | Runnable RunnableWidget
  deriving (Show)

data PlainSeg = PlainSeg { widget :: Widget
                         , format :: Format
                         } deriving (Show)

data Widget = Icon FilePath
            | Text String
            | Hspace Int32
  deriving (Show)

data RunnableWidget = RunnableWidget { com            :: Runnable
                                     , runnableFormat :: Format
                                     , val            :: [PlainSeg]
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
runParser :: Parser a -> ParseState -> String -> Either ParseError a
runParser p initial s = runParserT p initial mempty s `MTL.runReader` initial

parseString :: ParseState -> String -> Either ParseError Bar
parseString = runParser barParser

defaultAlign :: String
defaultAlign = "}{"

barParser :: Parser Bar
barParser = do
  ParseState { alignSep } <- getState
  let [alignSepL, alignSepR] = if length alignSep == 2 then alignSep else defaultAlign

  (try (Bar <$> manyTill segParser (char alignSepL)
            <*> manyTill segParser (char alignSepR)
            <*> manyTill segParser eof)
   <|> (Bar <$> manyTill segParser eof <*> mempty <*> mempty))

segParser :: Parser Seg
segParser = tagged
  (try (Runnable <$> runnableParser)
   <|> (Plain    <$> plainSegParser))

segParser' :: Parser PlainSeg
segParser' = tagged plainSegParser

tagged :: Parser a -> Parser a
tagged p =
      try (fnParser (tagged p))
  <|> try (boxParser (tagged p))
  <|> try (actionParser (tagged p))
  <|> try (fcParser (tagged p))
  <|>     p

plainSegParser :: Parser PlainSeg
plainSegParser = PlainSeg <$> widgetParser <*> (formatState <$> getState)

widgetParser :: Parser Widget
widgetParser =
      try (Icon     <$> iconParser)
  <|> try (Hspace   <$> hspaceParser)
  <|> try (Text     <$> rawParser)
  <|>     (Text     <$> textParser)

-- | Parses the output template string for a runnable widget
runnableParser :: Parser RunnableWidget
runnableParser = do
  ParseState { formatState, sepChar, commands } <- getState

  alias'  <- between (string sepChar) (string sepChar) (many1 (noneOf sepChar))

  let com = fromMaybe (Run (Com alias' [] [] 10))
            $ find ((==) alias' . alias) commands

  return RunnableWidget { com, runnableFormat = formatState, val = [] }

-- | Parse a "raw" tag, which we use to prevent other tags from creeping in.
-- The format here is net-string-esque: a literal "<raw=" followed by a
-- string of digits (base 10) denoting the length of the raw string,
-- a literal ":" as digit-string-terminator, the raw string itself, and
-- then a literal "/>".
rawParser :: Parser String
rawParser = do
  string "<raw="
  lenstr <- many1 digit
  char ':'
  case readMaybe lenstr of
    Just len -> do
      raw <- count len (noneOf "/>")
      string "/>"
      return raw
    Nothing -> fail ("expected an Int, got: " <> lenstr)

hspaceParser :: Parser Int32
hspaceParser = do
  string "<hspace="
  pVal <- manyTill digit (try (string "/>"))
  return (fromMaybe 0 $ readMaybe pVal)

textParser :: Parser String
textParser = do
  ParseState { alignSep, sepChar } <- getState
  many1 (noneOf ('<' : (alignSep <> sepChar)))

iconParser :: Parser FilePath
iconParser = do
  string "<icon="
  path <- manyTill (noneOf ">") (try (string "/>"))
  pure path

-- | Parse anything between `open` and `close` with local state used in the inner parser `p`
betweenTags :: Tag -> Parser a -> Parser b -> Parser c -> Parser c
betweenTags t open close p = do
  MTL.void open
  push t
  st <- getState
  x <- MTL.local (const st) p
  MTL.void close
  pop t
  putState =<< MTL.lift MTL.ask
  pure x

-- | Parses anything within an action tag
actionParser :: Parser a -> Parser a
actionParser = betweenTags Act actionOpen actionClose

actionOpen :: Parser ()
actionOpen = do
  acts <- actions . formatState <$> getState
  action <- between (string "<action=") (char '>') $ do
    cmd <- spawnParser
    skipMany (char ' ')
    btns <- option [1] (string "button=" >> buttonsParser)
    pure (Spawn btns cmd)

  let actions = Just (maybe [action] (action:) acts)
  modifyState (\s@ParseState { formatState } -> s { formatState = formatState { actions } })

actionClose :: Parser String
actionClose = string "</action>"

spawnParser :: Parser String
spawnParser = between (char '`') (char '`') (many1 (noneOf "`"))

buttonsParser :: Parser [Button]
buttonsParser = many1 buttonParser

buttonParser :: Parser Button
buttonParser = do
  dig <- readMaybe . pure <$> oneOf "12345"
  case dig of
    Just b -> pure b
    Nothing -> fail "not a button (valid choices are one of: 1, 2, 3, 4 or 5)"

-- | Parsers a string wrapped in a color specification.
fcParser :: Parser a -> Parser a
fcParser = betweenTags Fc fcOpen fcClose

fcOpen :: Parser ()
fcOpen = do
  oldInfo <- textRenderInfo . formatState <$> getState
  (fg, bg, ot, ob) <- between (string "<fc=") (string ">") colors
  modifyState (\s@ParseState { formatState } ->
                 s { formatState =
                     formatState { textRenderInfo =
                                   oldInfo { tColorsString   = fg <> maybe "" ("," <>) bg
                                           , tBgTopOffset    = ot
                                           , tBgBottomOffset = ob
                                           }
                                 }
                   })

fcClose :: Parser String
fcClose = string "</fc>"

-- | Parses a color specification (hex or named), and xft offset specification
colors :: Parser (String, Maybe String, Int32, Int32)
colors = do
  fg <- color
  bg <- optionMaybe (char ',' >> color)
  offsets <- optionMaybe (char ':' >> xftOffsets)
  pure $ case offsets of
    Just (ot, ob) -> (fg, bg, ot, fromMaybe (-1) ob)
    Nothing       -> (fg, bg, -1, -1)

xftOffsets :: Parser (Int32, Maybe Int32)
xftOffsets = do
  ot <- xftOffset
  ob <- optionMaybe (char ',' >> xftOffset)
  pure (ot, ob)

xftOffset :: Parser Int32
xftOffset = do
  digs <- many1 digit
  case readMaybe digs of
    Just n  -> pure n
    Nothing -> fail ("expected Int32, got: " <> digs)

color :: Parser String
color = try hexColor <|> namedColor

hexColor :: Parser String
hexColor = (:) <$> char '#' <*> (try (count 6 hexDigit)
                                 <|>  count 3 hexDigit)

namedColor :: Parser String
namedColor = many1 alphaNum

-- | Things wrapped in a box tag
boxParser :: Parser a -> Parser a
boxParser = betweenTags Bx boxOpen boxClose

boxOpen :: Parser ()
boxOpen = do
  oldInfo@TextRenderInfo { tColorsString, tBoxes } <- textRenderInfo . formatState <$> getState

  c <- between (string "<box") (string ">") (option "" (many1 (alphaNum <|> char '=' <|> char ' ' <|> char '#' <|> char ',')))
  let b = Box BBFull (BoxOffset C 0) 1 tColorsString (BoxMargins 0 0 0 0)
      g = boxReader b (words c)
      textRenderInfo = oldInfo { tBoxes = g : tBoxes }

  modifyState (\s@ParseState { formatState } -> s { formatState = formatState { textRenderInfo } })

boxClose :: Parser String
boxClose = string "</box>"

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

-- | Parsers anything within a fn tag
fnParser :: Parser a -> Parser a
fnParser = betweenTags Fn fnOpen fnClose

fnOpen :: Parser ()
fnOpen = do
  f <- between (string "<fn=") (string ">") (many1 digit)
  let fontIndex = fromMaybe 0 $ readMaybe f
  modifyState (\s@ParseState { formatState } ->
                 s { formatState = formatState { fontIndex } })

fnClose :: Parser String
fnClose = string "</fn>"
