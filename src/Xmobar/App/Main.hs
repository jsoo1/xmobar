{-# LANGUAGE NamedFieldPuns #-}

------------------------------------------------------------------------------
-- |
-- Module: Xmobar.App.Main
-- Copyright: (c) 2018, 2019, 2020 Jose Antonio Ortega Ruiz
-- License: BSD3-style (see LICENSE)
--
-- Maintainer: jao@gnu.org
-- Stability: unstable
-- Portability: portable
-- Created: Sun Nov 25, 2018 21:53
--
--
-- Support for creating executable main functions
--
------------------------------------------------------------------------------


module Xmobar.App.Main (xmobar, xmobarMain, configFromArgs) where

import Control.Concurrent.Async (cancel)
import Control.Exception (bracket)
import Control.Monad (unless, (>=>))

import Data.Foldable (for_)
import qualified Data.Map as Map
import Data.List (intercalate)
import System.Posix.Process (executeFile)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath ((</>), takeBaseName, takeDirectory, takeExtension)
import System.IO (hPrint, stderr)
import System.Random (getStdGen)
import Data.List.NonEmpty (NonEmpty(..))

import Graphics.X11.Xlib

import Text.Parsec (ParseError)

import Xmobar.Config.Types
import Xmobar.Config.Parse
import Xmobar.Config.Template.Parse hiding (sepChar, alignSep, commands)
import Xmobar.System.Signal (setupSignalHandler, withDeferSignals)
import Xmobar.X11.Types
import Xmobar.X11.Text
import Xmobar.X11.Window
import Xmobar.App.Opts (recompileFlag, verboseFlag, getOpts, doOpts)
import Xmobar.App.EventLoop (startLoop, startCommand, newRefreshLock, refreshLock, Running(..))
import Xmobar.App.Compile (recompile, trace)
import Xmobar.App.Config
import Xmobar.App.Timer (withTimer)

xmobar :: Config -> IO ()
xmobar conf = withDeferSignals $ do
  g <- getStdGen
  let tmpl = case template conf of
        Unparsed s -> parseString g (emptyParseState' conf) s
        Parsed t   -> pure t

  bar <- either (hPrint stderr >=> const exitFailure) pure tmpl

  let runnable Seg { widget = Runnable rw } = [rw]
      runnable _                            = []
      cls = foldMap runnable (allSegments bar)

  initThreads
  d <- openDisplay ""
  fs    <- initFont d (font conf)
  fl    <- mapM (initFont d) (additionalFonts conf)
  sig   <- setupSignalHandler
  refLock <- newRefreshLock
  withTimer (refreshLock refLock) $
    bracket (mapM (startCommand sig) cls) cleanupThreads $
      \vars -> do
        (r,w) <- createWin d fs conf
        let ic = Map.empty
            to = textOffset conf
            ts = textOffsets conf ++ replicate (length fl) (-1)
            xconf = XConf d r w (fs :| fl) (to :| ts) ic conf
        startLoop bar xconf sig refLock vars

emptyParseState' :: Config -> ParseState
emptyParseState' conf =
  emptyParseState (fgColor conf) (sepChar conf) (alignSep conf) (commands conf)

configFromArgs :: Config -> IO Config
configFromArgs cfg = getArgs >>= getOpts >>= doOpts cfg . fst

cleanupThreads :: [Running] -> IO ()
cleanupThreads vars =
  for_ vars $ \cmd ->
    for_ (handles cmd) cancel

buildLaunch :: [String] -> Bool -> Bool -> String -> ParseError -> IO ()
buildLaunch args verb force p e = do
  let exec = takeBaseName p
      confDir = takeDirectory p
      ext = takeExtension p
  if ext `elem` [".hs", ".hsc", ".lhs"]
    then xmobarDataDir >>= \dd -> recompile confDir dd exec force verb >>
         executeFile (confDir </> exec) False args Nothing
    else trace True ("Invalid configuration file: " ++ show e) >>
         trace True "\n(No compilation attempted: \
                    \only .hs, .hsc or .lhs files are compiled)"

xmobar' :: [String] -> Config -> IO ()
xmobar' defs cfg = do
  unless (null defs || not (verbose cfg)) $ putStrLn $
    "Fields missing from config defaulted: " ++ intercalate "," defs
  xmobar cfg

xmobarMain :: IO ()
xmobarMain = do
  args <- getArgs
  (flags, rest) <- getOpts args
  cf <- case rest of
          [c] -> return (Just c)
          [] -> xmobarConfigFile
          _ -> error $ "Too many arguments: " ++ show rest
  case cf of
    Nothing -> case rest of
                (c:_) -> error $ c ++ ": file not found"
                _ -> doOpts defaultConfig flags >>= xmobar
    Just p -> do r <- readConfig defaultConfig p
                 case r of
                   Left e ->
                     buildLaunch (filter (/= p) args) (verboseFlag flags) (recompileFlag flags) p e
                   Right (c, defs) -> doOpts c flags >>= xmobar' defs
