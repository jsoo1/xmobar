{-# LANGUAGE NamedFieldPuns #-}

------------------------------------------------------------------------------
-- |
-- Module: Xmobar.X11.ParsersSpec
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

module Xmobar.X11.ParsersSpec where

import Data.Maybe
import Test.Hspec
import Xmobar.App.Config
import Xmobar.Config.Types
import Xmobar.X11.Parsers

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Parse.parseString" $ do
    it "succeeds on the default configuration template" $ do
      let tmpl = parseString defaultConfig (template defaultConfig)
      -- `shouldBe` would fail here becuase of the Show constraint
      -- The show constraint is used functionally, so don't derive them
      either (pure False) (pure True) tmpl

    it "keeps only actions for a given segment"  $ do
      let tmpl = "<fc=#00FF00><action=`echo hi`>%uname%</action></fc> * <fc=#FF0000>%theDate%</fc>"
          res  = parseString defaultConfig tmpl
          resActions = fmap (foldMap (\(_, Format { actions }) -> fromMaybe [] actions)) res
      length resActions `shouldBe` 1
