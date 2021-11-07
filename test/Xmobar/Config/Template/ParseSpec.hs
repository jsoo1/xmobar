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
import Text.Parsec hiding (runParser)
import Xmobar.App.Config
import Xmobar.Run.Exec
import Xmobar.Run.Runnable
import Xmobar.Config.Types hiding (commands)
import Xmobar.Config.Template.Parse hiding (sepChar, alignSep)
import Xmobar.Config.Actions

import qualified Control.Concurrent.STM as STM
import qualified Xmobar.Config.Types

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  let defaultParseState = emptyParseState
        (fgColor defaultConfig)
        (sepChar defaultConfig)
        (alignSep defaultConfig)
        (Xmobar.Config.Types.commands defaultConfig)
      (Unparsed defaultTemplate) = template defaultConfig

  describe "Template Parsing" $ do
    let unalignedTemplate =
          "<fc=#00FF00><action=`echo hi`>%uname%</action></fc> * <fc=#FF0000>%theDate%</fc>"
    context unalignedTemplate $ do
      let segments = runParser (many1 segParser) defaultParseState unalignedTemplate
          runnable Seg { widget = Runnable rw } = [rw]
          runnable _                            = []

      let extraAction = "<action=`echo hi again`>plain</action>"
      it ("parses extra actions: " <> extraAction) $ do
        let tmpl = unalignedTemplate <> "<action=`echo hi again`>plain</action>"
            res = do
              segs <- runParser (many1 segParser) defaultParseState tmpl
              pure (foldMap (maybeToList . actions . format) segs)
        length res `shouldBe` 1

      it "parses command aliases in a segment" $ do
        let resRunnables = foldMap (fmap (alias . com) . runnable) <$> segments
        resRunnables `shouldBe` Right ["uname", "theDate"]

      it "parses unique uuids for runnables" $ do
        let resRunnables = foldMap (fmap (alias . com) . runnable) <$> segments
        resRunnables `shouldSatisfy` either (const False) (\ids -> length (nub ids) == length ids)

    let alignedTemplate = "<fc=#00FF00><action=`echo hi`>%uname%</action></fc>} * {<fc=#FF0000>%theDate%</fc>"
    context alignedTemplate $ do

      it "Parses into left, right, and center" $ do
        let res = parseString defaultParseState alignedTemplate
        res `shouldSatisfy` \r -> case r of
          Right Bar { left, center, right } ->
            length left == 1 && length center == 1 && length right == 1

          _ -> False

    context defaultTemplate $ do
      it defaultTemplate $ do
        let res = parseString defaultParseState defaultTemplate
        -- `shouldBe` would fail here because of the Show constraint
        -- The show constraint is used functionally, so don't derive them
        either (pure False) (pure True) res

  describe "Markup Tags" $ do
    context "<action></action>" $ do
      let simpleAction = "<action=`hi`>text</action>"
      it simpleAction $ do
        let res = runParser (actionParser textParser) defaultParseState simpleAction
        res `shouldSatisfy` either (const False) (== "text")

      let complicatedAction = "<action=`amixer -q set Master toggle` button=1>text</action>"
      it complicatedAction $ do
        let res = runParser (actionParser textParser) defaultParseState complicatedAction
        res `shouldSatisfy` either (const False) (== "text")

    context "<fn></fn>" $ do
      let singleFn =  "<fn=2>text</fn>"
      it singleFn $ do
        let res  = runParser (fnParser textParser) defaultParseState singleFn
        res `shouldSatisfy` either (const False) (== "text")

    context "<fc></fc>" $ do
      let singleFc = "<fc=#AB0,brightblack:0,2>text</fc>"
      it singleFc $ do
        let res  = runParser (fcParser textParser) defaultParseState singleFc
        res `shouldSatisfy` either (const False) (== "text")

    context "<box></box>" $ do
      let simpleBox = "<box type=Bottom color=#4AB width=2>text</box>"
      it simpleBox $ do
        let res  = runParser (boxParser textParser) defaultParseState simpleBox
        res `shouldSatisfy` either (const False) (== "text")

  describe "Segments" $ do
    let adorned   = "some prefix %theDate% some suffix"
        unadorned = "%uname%"

    context "Runnable %alias% segments" $ do
      it (show adorned) $ do
        let res = runParser (many1 segParser) defaultParseState adorned
        res `shouldSatisfy` either (const False)
          (\res -> case res of
              [ Seg { widget = Text "some prefix "}, Seg { widget = Runnable RunnableWidget { com } } , Seg { widget = Text " some suffix" } ] ->
                alias com == "theDate"

              _ -> False)

      it (show unadorned) $ do
        runParser runnableParser defaultParseState unadorned `shouldSatisfy` either (const False)
          (\RunnableWidget { com } -> alias com == "uname")


    context "Self-closing Tags" $ do
      context "<raw=/>" $ do
        let simpleRaw =  "<raw=20:iiiiiiiiiiiiiiiiiiii/>"
        it simpleRaw $ do
          let res = runParser rawParser defaultParseState simpleRaw
          res `shouldSatisfy` either (const False) (== "iiiiiiiiiiiiiiiiiiii")

      context "<icon=/>" $ do
        let simpleIcon = "<icon=/home/example/.icons/example.xbm/>"
        it simpleIcon $ do
          let res = runParser iconParser defaultParseState simpleIcon
          res `shouldSatisfy` either (const False) (== "/home/example/.icons/example.xbm")
