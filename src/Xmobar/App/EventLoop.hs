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
    , newRefreshLock
    , refreshLock
    , Running(..)
    ) where

import Prelude hiding (lookup)
import Graphics.X11.Xlib hiding (textExtents, textWidth)
import Graphics.X11.Xlib.Extras
import Graphics.X11.Xinerama
import Graphics.X11.Xrandr

import Control.Arrow ((&&&))
import Control.Monad.Reader
import Control.Concurrent
import Control.Concurrent.Async (Async, async)
import Control.Concurrent.STM
import Control.Exception (bracket_, handle, SomeException(..))
import Data.Bits
import Data.Map hiding (foldr, map, filter)
import Data.Maybe (fromJust, isJust)
import qualified Data.List.NonEmpty as NE
import Data.UUID (UUID)

import Xmobar.System.Signal
import Xmobar.Config.Types
import Xmobar.Config.Template.Parse hiding (sepChar, alignSep, commands)
import Xmobar.Run.Exec
import Xmobar.X11.Actions
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

newRefreshLock :: IO (TMVar ())
newRefreshLock = newTMVarIO ()

refreshLock :: TMVar () -> IO a -> IO a
refreshLock var = bracket_ lock unlock
    where
        lock = atomically $ takeTMVar var
        unlock = atomically $ putTMVar var ()

refreshLockT :: TMVar () -> STM a -> STM a
refreshLockT var action = do
    takeTMVar var
    r <- action
    putTMVar var ()
    return r

-- | Starts the main event loop and threads
startLoop :: Bar
          -> XConf
          -> TMVar SignalType
          -> TMVar ()
          -> [Running]
          -> IO ()
startLoop bar xcfg@(XConf _ _ w _ _ _ _) sig pauser vs = do
#ifdef XFT
    xftInitFtLibrary
#endif
    tv <- newTVarIO bar
    _ <- forkIO (handle (handler "checker") (checker tv bar vs sig pauser))
#ifdef THREADED_RUNTIME
    _ <- forkOS (handle (handler "eventer") (eventer sig))
#else
    _ <- forkIO (handle (handler "eventer") (eventer sig))
#endif
#ifdef DBUS
    runIPC sig
#endif
    eventLoop tv xcfg [] sig
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

-- | Send signal to eventLoop every time a var is updated
checker :: TVar Bar
           -> Bar
           -> [Running]
           -> TMVar SignalType
           -> TMVar ()
           -> IO ()
checker tvar ov vs signal pauser = do
      nval <- atomically $ refreshLockT pauser $ do
        mapM (readTVar . chan) vs >>= traverse (\(i,new) -> do
          let nv = updateBar i new ov
          writeTVar tvar nv
          return nv)
      atomically $ putTMVar signal Wakeup
      checker tvar (last nval) vs signal pauser

updateBar :: UUID -> String -> Bar -> Bar
updateBar i val bar@Bar { left, center, right } =
  case (l2, c2, r2) of
    (s@Seg { widget = Runnable rw }:segs,_,_) ->
      bar { left   = l1 <> (s { widget = Runnable rw { val } } : segs) }

    (_,s@Seg { widget = Runnable rw }:segs,_) ->
      bar { center = c1 <> (s { widget = Runnable rw { val } } : segs) }

    (_,_,s@Seg { widget = Runnable rw }:segs) ->
      bar { right  = r1 <> (s { widget = Runnable rw { val } } : segs) }

    _ -> bar

  where
    (l1, l2) = break isRunnable left
    (c1, c2) = break isRunnable center
    (r1, r2) = break isRunnable right

    isRunnable Seg { widget = Runnable rw } = i == runnableId rw
    isRunnable _ = False

-- | Continuously wait for a signal from a thread or a interrupt handler
eventLoop :: TVar Bar
             -> XConf
             -> [([Action], Position, Position)]
             -> TMVar SignalType
             -> IO ()
eventLoop tv xc@(XConf d r w fs vos is cfg) as signal = do
      typ <- atomically $ takeTMVar signal
      case typ of
         Wakeup -> do
            b <- readTVarIO tv
            c <- updateCache d w is (iconRoot cfg) b
            let xc' = xc { iconS = c }
            as' <- updateActions xc r b
            runX xc' $ drawInWin r b
            eventLoop tv xc' as' signal

         Reposition ->
            reposWindow cfg

         ChangeScreen -> do
            ncfg <- updateConfigPosition cfg
            reposWindow ncfg

         Hide   t -> hide   (t*100*1000)
         Reveal t -> reveal (t*100*1000)
         Toggle t -> toggle t

         TogglePersistent -> eventLoop
            tv xc { config = cfg { persistent = not $ persistent cfg } } as signal

         Action but x -> action but x

    where
        isPersistent = not $ persistent cfg

        hide t
            | t == 0 =
                when isPersistent (hideWindow d w) >> eventLoop tv xc as signal
            | otherwise = do
                void $ forkIO
                     $ threadDelay t >> atomically (putTMVar signal $ Hide 0)
                eventLoop tv xc as signal

        reveal t
            | t == 0 = do
                when isPersistent (showWindow r cfg d w)
                eventLoop tv xc as signal
            | otherwise = do
                void $ forkIO
                     $ threadDelay t >> atomically (putTMVar signal $ Reveal 0)
                eventLoop tv xc as signal

        toggle t = do
            ismapped <- isMapped d w
            atomically (putTMVar signal $ if ismapped then Hide t else Reveal t)
            eventLoop tv xc as signal

        reposWindow rcfg = do
          r' <- repositionWin d w (NE.head fs) rcfg
          eventLoop tv (XConf d r' w fs vos is rcfg) as signal

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
          mapM_ runAction $
            filter (\(Spawn b _) -> button `elem` b) $
            concatMap (\(a,_,_) -> a) $
            filter (\(_, from, to) -> x >= from && x <= to) as
          eventLoop tv xc as signal

-- $command

-- | A running command.
data Running = Running { handles :: [Async ()]
                       , chan :: TVar (UUID, String)
                       }

-- | Runs a command as an independent thread and returns its Async handles
-- and the TVar the command will be writing to.
startCommand :: TMVar SignalType -> RunnableWidget -> IO Running
startCommand sig RunnableWidget { runnableId, com, val }
    | alias com == "" = do chan <- newTVarIO (runnableId, is val)
                           atomically $ writeTVar chan (runnableId, mempty)
                           return Running { handles = [], chan }

    | otherwise       = do chan <- newTVarIO (runnableId, is val)
                           let cb = atomically . writeTVar chan . (runnableId,)

                           a1 <- async $ start com cb
                           a2 <- async $ trigger com $ maybe (return ()) (atomically . putTMVar sig)
                           return Running { handles = [ a1, a2 ], chan }

    where is x = "Updating " <> x <> "..."



updateActions :: XConf -> Rectangle -> Bar -> IO [([Action], Position, Position)]
updateActions conf (Rectangle _ _ wid _) Bar { left, center, right } = do
  let (d,fs) = (display &&& fontListS) conf
      strLn :: [Seg] -> IO [(Maybe [Action], Position, Position)]
      strLn  = liftIO . mapM getCoords
      iconW i = maybe 0 Bitmap.width (lookup i $ iconS conf)
      getCoords Seg { widget = Text s, format = Format { fontIndex, actions } } = do
        tw <- textWidth d (safeIndex fs fontIndex) s
        return (actions, 0, fi tw)
      getCoords Seg { widget = Runnable RunnableWidget { val }, format = Format { fontIndex, actions } } = do
        tw <- textWidth d (safeIndex fs fontIndex) val
        return (actions, 0, fi tw)
      getCoords Seg { widget = Icon s, format = Format { actions } } = return (actions, 0, fi $ iconW s)
      getCoords Seg { widget = Hspace w, format = Format { actions } } = return (actions, 0, fi w)
      partCoord off xs = map (\(a, x, x') -> (fromJust a, x, x')) $
                         filter (\(a, _,_) -> isJust a) $
                         scanl (\(_,_,x') (a,_,w') -> (a, x', x' + w'))
                               (Nothing, 0, off)
                               xs
      totSLen = foldr (\(_,_,len) -> (+) len) 0
      remWidth xs = fi wid - totSLen xs
      offs = 1
      offset a xs = case a of
                     C -> (remWidth xs + offs) `div` 2
                     R -> remWidth xs
                     L -> offs
  fmap concat $ mapM (\(a,xs) ->
                       (\xs' -> partCoord (offset a xs') xs') <$> strLn xs) $
                     zip [L,C,R] [left,center,right]
