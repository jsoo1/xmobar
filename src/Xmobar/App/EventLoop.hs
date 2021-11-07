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
import Data.Foldable (foldlM)
import Data.Map hiding (foldr, map, mapMaybe, filter)
import Data.Maybe (mapMaybe)
import qualified Data.List.NonEmpty as NE
import Text.Parsec (ParseError, many1, try, (<|>))

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
startLoop bar xcfg@(XConf _ _ w _ _ _ conf) sig pauser vs = do
#ifdef XFT
    xftInitFtLibrary
#endif
    tv <- newTVarIO bar
    _ <- forkIO (handle (handler "checker") (checker (emptyParseState' conf) tv bar vs sig pauser))
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
checker :: ParseState
           -> TVar Bar
           -> Bar
           -> [Running]
           -> TMVar SignalType
           -> TMVar ()
           -> IO ()
checker parseState tvar ov vs signal pauser = do
      nval <- atomically $ refreshLockT pauser $ do
        xs <- traverse (readTVar . chan) vs
        foldlM (updateRunning parseState tvar) ov xs
      atomically $ putTMVar signal Wakeup
      checker parseState tvar nval vs signal pauser

emptyParseState' :: Config -> ParseState
emptyParseState' conf =
  emptyParseState (fgColor conf) (sepChar conf) (alignSep conf) (commands conf)

data New = NewSegs [PlainSeg] | NewBar Bar

parseNew :: Parser New
parseNew =
  try (NewSegs <$> many1 segParser')
  <|> (NewBar  <$> barParser)

updateRunning :: ParseState -> TVar Bar -> Bar -> (String, String) -> STM Bar
updateRunning ps tvar ov (alias',s) = do
  let nv = updateRunnables ps alias' s ov
  writeTVar tvar nv
  pure nv

-- | Update the runnable with alias alias` in bar.
updateRunnables :: ParseState -> String -> String -> Bar -> Bar
updateRunnables ps alias' s bar@Bar { left, center, right } =
  case (l2, c2, r2) of
    ((Runnable rw):segs,_,_) ->
      case (updateRunnableWidget ps s rw) of
        Left rw' -> bar { left = l1 <> (Runnable rw' : segs) }
        Right b  -> b

    (_,(Runnable rw):segs,_) ->
      case (updateRunnableWidget ps s rw) of
        Left rw' -> bar { center = c1 <> (Runnable rw' : segs) }
        Right b  -> b

    (_,_,(Runnable rw):segs) ->
      case (updateRunnableWidget ps s rw) of
        Left rw' -> bar { right = r1 <> (Runnable rw' : segs) }
        Right b  -> b

    _ -> bar

  where
    (l1, l2) = break (isRunnable alias') left
    (c1, c2) = break (isRunnable alias') center
    (r1, r2) = break (isRunnable alias') right

updateRunnableWidget :: ParseState -> String -> RunnableWidget -> Either RunnableWidget Bar
updateRunnableWidget ps s rw@RunnableWidget { runnableFormat } =
  case runParser parseNew (ps { formatState = runnableFormat }) s of
    Right (NewSegs ss) -> Left (rw { val = ss })
    Right (NewBar b)   -> Right b
    Left e             -> Left (rw { val = showError runnableFormat e })

showError :: Format -> ParseError -> [PlainSeg]
showError format e = [PlainSeg { format, widget = Text (show e) }]

isRunnable :: String -> Seg -> Bool
isRunnable alias' (Runnable rw) = alias' == alias (com rw)
isRunnable _  _                 = False

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
                       , chan :: TVar (String, String)
                       }

-- | Runs a command as an independent thread and returns its Async handles
-- and the TVar the command will be writing to.
startCommand :: TMVar SignalType -> RunnableWidget -> IO Running
startCommand sig RunnableWidget { com }
    | alias com == "" = do chan <- newTVarIO (alias com, is (alias com))
                           atomically $ writeTVar chan (alias com, mempty)
                           return Running { handles = [], chan }

    | otherwise       = do chan <- newTVarIO (alias com, is (alias com))
                           let cb = atomically . writeTVar chan . (alias com,)

                           a1 <- async $ start com cb
                           a2 <- async $ trigger com $ maybe (return ()) (atomically . putTMVar sig)
                           return Running { handles = [ a1, a2 ], chan }

    where is x = "Updating " <> x <> "..."



updateActions :: XConf -> Rectangle -> Bar -> IO [([Action], Position, Position)]
updateActions conf (Rectangle _ _ wid _) Bar { left, center, right } = do
  let (d,fs) = (display &&& fontListS) conf
      strLn :: [PlainSeg] -> IO [(Maybe [Action], Position, Position)]
      strLn  = liftIO . mapM getCoords
      iconW i = maybe 0 Bitmap.width (lookup i $ iconS conf)
      getCoords PlainSeg { widget = Text s, format = Format { fontIndex, actions } } = do
        tw <- textWidth d (safeIndex fs fontIndex) s
        return (actions, 0, fi tw)
      getCoords PlainSeg { widget = Icon s, format = Format { actions } } = return (actions, 0, fi $ iconW s)
      getCoords PlainSeg { widget = Hspace w, format = Format { actions } } = return (actions, 0, fi w)
      partCoord off xs = mapMaybe (\(a, x, x') -> fmap (,x,x') a) $
                         scanl (\(_,_,x') (a,_,w') -> (a, x', x' + w'))
                               (Nothing, 0, off)
                               xs
      totSLen = foldr (\(_,_,len) -> (+) len) 0
      remWidth xs = fi wid - totSLen xs
      offs = 1

  left' <- strLn (plainSegments =<< left)
  center' <- strLn (plainSegments =<< center)
  right' <- strLn (plainSegments =<< right)

  let left'' = partCoord offs left'
      center'' = partCoord (remWidth center' + offs `div` 2) center'
      right'' = partCoord (remWidth right') right'

  pure (left'' <> center'' <> right'')
