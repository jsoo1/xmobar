{-# LANGUAGE CPP #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NamedFieldPuns #-}

------------------------------------------------------------------------------
-- |
-- Module: Xmobar.X11.EventLoop
-- Copyright: (c) 2018, 2020 Jose Antonio Ortega Ruiz
-- License: BSD3-style (see LICENSE)
--
-- Maintainer: jao@gnu.org
-- Stability: unstable
-- Portability: portable
-- Created: Sat Nov 24, 2018 19:40
--
--
-- Event loop
--
------------------------------------------------------------------------------


module Xmobar.App.EventLoop
    ( startLoop
    , startCommand
    , refreshLock
    , Running(..)
    ) where

import Prelude hiding (lookup)
import Graphics.X11.Xlib hiding (textExtents, textWidth)
import Graphics.X11.Xlib.Extras
import Graphics.X11.Xinerama
import Graphics.X11.Xrandr

import Control.Monad.Except
import Control.Monad.Reader
import Control.Concurrent
import Control.Concurrent.Async (Async, async)
import Control.Concurrent.STM
import Control.Exception (bracket_, handle, SomeException(..))
import Data.Bits
import Data.Either (fromRight)
import Data.Foldable (for_, foldrM)
import Data.Map hiding (foldr, map, mapMaybe, filter)
import Data.Maybe (mapMaybe)
import qualified Data.List.NonEmpty as NE
import Text.Parsec (ParseError, many, many1, try, (<|>))

import Xmobar.System.Signal
import Xmobar.Config.Actions
import Xmobar.Config.Types
import Xmobar.Config.Template.Parse hiding (sepChar, alignSep, commands)
import Xmobar.Run.Exec
import Xmobar.X11.Window
import Xmobar.X11.Text
import Xmobar.X11.Draw
import Xmobar.X11.Bitmap as Bitmap
import Xmobar.X11.Types
import Xmobar.System.Utils (safeIndex)

#ifndef THREADED_RUNTIME
import Xmobar.X11.Events(nextEvent')
#endif

#ifdef XFT
import Graphics.X11.Xft
#endif

#ifdef DBUS
import Xmobar.System.DBus
#endif

runX :: XConf -> X () -> IO ()
runX xc f = runReaderT f xc

refreshLock :: TMVar () -> IO a -> IO a
refreshLock var = bracket_ lock unlock
    where
        lock = atomically $ takeTMVar var
        unlock = atomically $ putTMVar var ()

-- | Starts the main event loop and threads
startLoop :: Bar Running
          -> XConf
          -> TMVar SignalType
          -> IO ()
startLoop bar xcfg@(XConf _ _ w _ _ _ _) sig = do
#ifdef XFT
    xftInitFtLibrary
#endif
#ifdef THREADED_RUNTIME
    _ <- forkOS (handle (handler "eventer") (eventer sig))
#else
    _ <- forkIO (handle (handler "eventer") (eventer sig))
#endif
#ifdef DBUS
    runIPC sig
#endif
    eventLoop bar xcfg [] sig
  where
    handler thing (SomeException e) =
      void $ putStrLn ("Thread " ++ thing ++ " failed: " ++ show e)
    -- Reacts on events from X
    eventer signal =
      allocaXEvent $ \e -> do
        dpy <- openDisplay ""
        xrrSelectInput dpy (defaultRootWindow dpy) rrScreenChangeNotifyMask
        selectInput dpy w (exposureMask .|. structureNotifyMask .|. buttonPressMask)

        forever $ do
#ifdef THREADED_RUNTIME
          nextEvent dpy e
#else
          nextEvent' dpy e
#endif
          ev <- getEvent e
          case ev of
            ConfigureEvent {} -> atomically $ putTMVar signal Reposition
            ExposeEvent {} -> atomically $ putTMVar signal Wakeup
            RRScreenChangeNotifyEvent {} -> atomically $ putTMVar signal Reposition
            ButtonEvent {} -> atomically $
                   putTMVar signal (Action (ev_button ev) (fi $ ev_x ev))
            _ -> return ()

-- | Continuously wait for a signal from a thread or a interrupt handler
eventLoop :: Bar Running
             -> XConf
             -> [([Action], Position, Position)]
             -> TMVar SignalType
             -> IO ()
eventLoop bar xc@(XConf d r w fs vos is cfg) as signal = do
      typ <- atomically $ takeTMVar signal
      case typ of
         Wakeup -> do
            b <- runExceptT $ traverse updateRunning bar
            let plain = either id (fmap segs) b
            iconS <- updateCache d w is (iconRoot cfg) plain
            let xc' = xc { iconS }
            as' <- updateActions xc' r plain
            runX xc' $ drawInWin r plain
            eventLoop (fromRight bar b) xc' as' signal

         Reposition ->
            reposWindow cfg

         ChangeScreen -> do
            ncfg <- updateConfigPosition cfg
            reposWindow ncfg

         Hide   t -> hide   (t*100*1000)
         Reveal t -> reveal (t*100*1000)
         Toggle t -> toggle t

         TogglePersistent -> eventLoop
            bar xc { config = cfg { persistent = not $ persistent cfg } } as signal

         Action but x -> action but x

    where
        isPersistent = not $ persistent cfg

        hide t
            | t == 0 =
                when isPersistent (hideWindow d w) >> eventLoop bar xc as signal
            | otherwise = do
                void $ forkIO
                     $ threadDelay t >> atomically (putTMVar signal $ Hide 0)
                eventLoop bar xc as signal

        reveal t
            | t == 0 = do
                when isPersistent (showWindow r cfg d w)
                eventLoop bar xc as signal
            | otherwise = do
                void $ forkIO
                     $ threadDelay t >> atomically (putTMVar signal $ Reveal 0)
                eventLoop bar xc as signal

        toggle t = do
            ismapped <- isMapped d w
            atomically (putTMVar signal $ if ismapped then Hide t else Reveal t)
            eventLoop bar xc as signal

        reposWindow rcfg = do
          r' <- repositionWin d w (NE.head fs) rcfg
          eventLoop bar (XConf d r' w fs vos is rcfg) as signal

        updateConfigPosition ocfg =
          case position ocfg of
            OnScreen n o -> do
              srs <- getScreenInfo d
              return (if n == length srs
                       then
                        (ocfg {position = OnScreen 1 o})
                       else
                        (ocfg {position = OnScreen (n+1) o}))
            o -> return (ocfg {position = OnScreen 1 o})

        action button x = do
          for_ as $ \(as', from, to) ->
            when (from <= x && x <= to) $
              for_ as' $ \a@(Spawn bs _) ->
                when (button `elem` bs) (runAction a)

          eventLoop bar xc as signal

-- $command

-- | A running command.
data Running = Running { handles :: [Async ()]
                       , runningFormat :: Format
                       , segs :: [PlainSeg]
                       , var :: TMVar (Either ParseError New)
                       }

data New = NewSegs [PlainSeg] | NewBar (Bar [PlainSeg])

parseNew :: Parser New
parseNew =
  try (NewSegs <$> many1 segParser')
  <|> (NewBar  <$> barParser (many plainSegParser))

showError :: Format -> ParseError -> [PlainSeg]
showError format e = [ PlainSeg { format, widget = Text (show e) } ]

updateRunning :: Running -> ExceptT (Bar [PlainSeg]) IO Running
updateRunning r@Running { runningFormat, var } = do
  v <- liftIO $ atomically $ tryTakeTMVar var
  case v of
    Nothing                  -> pure r
    Just (Left e)            -> pure (r { segs = showError runningFormat e })
    Just (Right (NewSegs s)) -> pure (r { segs = s })
    Just (Right (NewBar b))  -> throwError b

-- | Runs a command as an independent thread and returns its Async handles
-- and the TVar the command will be writing to.
startCommand :: TMVar SignalType -> ParseState -> RunnableWidget -> IO Running
startCommand sig ps RunnableWidget { runnableFormat = runningFormat, com }
    | alias com == "" = do var <- newEmptyTMVarIO
                           return Running { handles = []
                                          , segs = plain "Command missing an alias"
                                          , runningFormat
                                          , var
                                          }


    | otherwise       = do var <- newEmptyTMVarIO
                           let cb res = atomically $ do
                                 putTMVar var $ runParser parseNew (ps { formatState = runningFormat }) res
                                 putTMVar sig Wakeup

                           a1 <- async $ start com cb
                           a2 <- async $ trigger com $ maybe (return ()) (atomically . putTMVar sig)
                           return Running { handles = [ a1, a2 ]
                                          , segs = plain (is (alias com))
                                          , runningFormat
                                          , var
                                          }


    where is x = "Updating " <> x <> "..."
          plain msg = [ PlainSeg { format = runningFormat, widget = Text msg } ]

getCoords :: XConf -> PlainSeg -> IO (Maybe [Action], Position, Position)
getCoords XConf { display, fontListS } PlainSeg { widget = Text s, format = Format { fontIndex, actions } } = do
  tw <- textWidth display (safeIndex fontListS fontIndex) s
  return (actions, 0, fi tw)

getCoords XConf { iconS } PlainSeg { widget = Icon s, format = Format { actions } } = do
  let iconW i = maybe 0 Bitmap.width (lookup i iconS)
  return (actions, 0, fi $ iconW s)

getCoords _ PlainSeg { widget = Hspace w, format = Format { actions } } =
  return (actions, 0, fi w)

segStr :: XConf -> Seg [PlainSeg] -> IO [(Maybe [Action], Position, Position)]
segStr conf seg = case seg of
  Plain s -> pure <$> getCoords conf s
  Runnable ss -> mapM (getCoords conf) ss

updateActions :: XConf -> Rectangle -> Bar [PlainSeg] -> IO [([Action], Position, Position)]
updateActions conf (Rectangle _ _ wid _) Bar { left, center, right } = do
  let strLn :: [Seg [PlainSeg]] -> IO [(Maybe [Action], Position, Position)]
      strLn = foldrM (\seg widths -> (<> widths) <$> segStr conf seg) []
      totSLen = foldr (\(_,_,len) -> (+) len) 0
      remWidth xs = fi wid - totSLen xs
      offs = 1
      partCoord off xs = mapMaybe (\(a, x, x') -> fmap (,x,x') a) $
                         scanl (\(_,_,x') (a,_,w') -> (a, x', x' + w'))
                               (Nothing, 0, off)
                               xs

  left' <- strLn left
  center' <- strLn center
  right' <- strLn right

  let left'' = partCoord offs left'
      center'' = partCoord (remWidth center' + offs `div` 2) center'
      right'' = partCoord (remWidth right') right'

  pure (left'' <> center'' <> right'')
