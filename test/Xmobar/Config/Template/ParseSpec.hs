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

import Data.Maybe
import Test.Hspec
import Text.Parsec
import Xmobar.App.Config
import Xmobar.Run.Exec
import Xmobar.Run.Runnable
import Xmobar.Config.Types
import Xmobar.Config.Template.Parse hiding (sepChar, alignSep, commands)

import System.Random

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  let g = mkStdGen 3854207386 -- Randomly chosen

  describe "Parse.parseString" $ do
    let (Unparsed defaultTemplate) = template defaultConfig

    context "Unaligned templates" $ do
      let unalignedTemplate = "<fc=#00FF00><action=`echo hi`>%uname%</action></fc> * <fc=#FF0000>%theDate%</fc>"
          res = parseString g defaultParseState unalignedTemplate
          segments = allSegments <$> res

      it "succeeds on the default configuration template" $ do
        let res = parseString g defaultParseState defaultTemplate
        -- `shouldBe` would fail here becuase of the Show constraint
        -- The show constraint is used functionally, so don't derive them
        either (pure False) (pure True) res

      it "keeps only actions for a given segment" $ do
        let tmpl = unalignedTemplate <> "<action='echo hi again`>plain</action>"
            res = parseString g defaultParseState unalignedTemplate
            segs = allSegments <$> res
            actions' = foldMap (maybeToList . actions . format) <$> segs
        length actions' `shouldBe` 1

      it "parses command aliases in a segment" $ do
        let runnable Seg { widget = (Runnable _ r _ _ _) } = [alias r]
            runnable _                                     = []
            resRunnables = fmap (foldMap runnable) segments
        resRunnables `shouldBe` Right ["uname", "theDate"]

    context "Aligned templates" $ do
      let alignedTemplate = "<fc=#00FF00><action=`echo hi`>%uname%</action></fc>} * {<fc=#FF0000>%theDate%</fc>"

      it "splits the template on alignSep" $ do
        let prsL = concat <$> manyTill allParsers (char '}')
            prsC = concat <$> manyTill allParsers (char '{')
            prsR = concat <$> manyTill allParsers eof
            prs = (,) <$> prsL <*> prsC
            res = evalTemplateParser prs g defaultParseState alignedTemplate
        res `shouldSatisfy` \r -> case r of Right ([_], [_]) -> True; _ -> False

      it "splits the template into center, left and right" $ do
        let res = parseString g defaultParseState alignedTemplate
        res `shouldSatisfy` \r -> case r of
          Right Bar { left, center, right } ->
            length left == 1 && length center == 1 && length right == 1

          _ -> False

defaultParseState :: ParseState
defaultParseState =
  emptyParseState (fgColor defaultConfig) (sepChar defaultConfig) (alignSep defaultConfig) (commands defaultConfig)
