{-# LANGUAGE NamedFieldPuns #-}

------------------------------------------------------------------------------
-- |
-- Module: Xmobar.Config.Template.ParseSpec
-- Copyright: (c) 2021 John Soo
-- License: BSD3-style (see LICENSE)
--
-- Maintainer: jao@gnu.org
-- Stability: unstable
-- Portability: portable
-- Created: Sun Oct 24, 2021 13:41
--
--
-- Tests for parsing template substrings
--
------------------------------------------------------------------------------

module Xmobar.Config.Template.ParseSpec where

import Data.List
import Data.Maybe
import Test.Hspec
import Text.Parsec
import Xmobar.App.Config
import Xmobar.Run.Exec
import Xmobar.Run.Runnable
import Xmobar.Config.Types
import Xmobar.Config.Template.Parse hiding (sepChar, alignSep, commands)
import Xmobar.X11.Actions

import System.Random

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  let g = mkStdGen 3854207386 -- Randomly chosen
      defaultParseState = emptyParseState (fgColor defaultConfig) (sepChar defaultConfig) (alignSep defaultConfig) (commands defaultConfig)
      (Unparsed defaultTemplate) = template defaultConfig

  describe "Template Parsing" $ do
    context "Unaligned" $ do
      let unalignedTemplate = "<fc=#00FF00><action=`echo hi`>%uname%</action></fc> * <fc=#FF0000>%theDate%</fc>"
          res = parseString g defaultParseState unalignedTemplate
          segments = allSegments <$> res
          runnable Seg { widget = Runnable rw } = [rw]
          runnable _                            = []

      it "succeeds on the default configuration template" $ do
        let res = parseString g defaultParseState defaultTemplate
        -- `shouldBe` would fail here because of the Show constraint
        -- The show constraint is used functionally, so don't derive them
        either (pure False) (pure True) res

      it "keeps only actions for a given segment" $ do
        let tmpl = unalignedTemplate <> "<action=`echo hi again`>plain</action>"
            res = parseString g defaultParseState tmpl
            segs = allSegments <$> res
            actions' = foldMap (maybeToList . actions . format) <$> segs
        length actions' `shouldBe` 1

      it "parses command aliases in a segment" $ do
        let resRunnables = foldMap (fmap (alias . com) . runnable) <$> segments
        resRunnables `shouldBe` Right ["uname", "theDate"]

      it "parses unique uuids for runnables" $ do
        let resRunnables = foldMap (fmap runnableId . runnable) <$> segments
        resRunnables `shouldSatisfy` either (const False) (\ids -> length (nub ids) == length ids)

    context "Aligned" $ do
      let alignedTemplate = "<fc=#00FF00><action=`echo hi`>%uname%</action></fc>} * {<fc=#FF0000>%theDate%</fc>"

      it "splits the template on alignSep" $ do
        let prsL = manyTill segParser (char '}')
            prsC = manyTill segParser (char '{')
            prsR = manyTill segParser eof
            prs = (,,) <$> prsL <*> prsC <*> prsR
            res = evalParser prs g defaultParseState alignedTemplate
        res `shouldSatisfy` \r -> case r of Right ([_], [_], [_]) -> True; _ -> False

      it "splits the template into center, left and right" $ do
        let res = parseString g defaultParseState alignedTemplate
        res `shouldSatisfy` \r -> case r of
          Right Bar { left, center, right } ->
            length left == 1 && length center == 1 && length right == 1

          _ -> False

  describe "Markup Tags" $ do
    context "<action></action>" $ do
      it "parses a single action" $ do
        let tmpl = "<action=`hi`>text</action>"
            res = evalParser (actionParser textParser) g defaultParseState tmpl
        res `shouldSatisfy` either (const False) (== "text")

    context "<fn></fn>" $ do
      it "parses a single fn" $ do
        let tmpl = "<fn=2>text</fn>"
            res  = evalParser (fnParser textParser) g defaultParseState tmpl
        res `shouldSatisfy` either (const False) (== "text")

    context "<fc></fc>" $ do
      it "parses a single fc" $ do
        let tmpl = "<fc=#AB0,brightblack:0,2>text</fc>"
            res  = evalParser (fcParser textParser) g defaultParseState tmpl
        res `shouldSatisfy` either (const False) (== "text")

    context "<box></box>" $ do
      it "parses a single box" $ do
        let tmpl = "<box type=Bottom color=#4AB width=2>text</box>"
            res  = evalParser (boxParser textParser) g defaultParseState tmpl
        res `shouldSatisfy` either (const False) (== "text")

  describe "Widgets" $ do
    let adorned   = "some prefix %theDate% some suffix"
        unadorned = "%uname%"

    context "Runnable %alias% widgets" $ do
      it ("parses an adorned \"" <> adorned <> "\"") $ do
        evalParser (many1 widgetParser) g defaultParseState adorned `shouldSatisfy` either (const False)
          (\widgets -> case widgets of
              [Text "some prefix ", Runnable RunnableWidget { val = "theDate" }, Text " some suffix"] ->
                True

              _ -> False)

      it ("parses an unadorned \"" <> unadorned <> "\"") $ do
        evalParser runnableParser g defaultParseState unadorned `shouldSatisfy` either (const False)
          (\RunnableWidget { val } -> val == "uname")


    context "Self-closing Tags" $ do
      context "<raw=/>" $ do
        it "parses a single raw tag" $ do
          let tmpl = "<raw=20:iiiiiiiiiiiiiiiiiiii/>"
          evalParser rawParser g defaultParseState tmpl `shouldSatisfy` either (const False)
            (== "iiiiiiiiiiiiiiiiiiii")

      context "<icon=/>" $ do
        it "parses a single icon tag" $ do
          let tmpl = "<icon=/home/example/.icons/example.xbm/>"
          evalParser iconParser g defaultParseState tmpl `shouldSatisfy` either (const False)
            (== "/home/example/.icons/example.xbm")
