------------------------------------------------------------------------------
-- |
-- Module: Xmobar.Config.Align
-- Copyright: (c) 2021 Jose Antonio Ortega Ruiz
-- License: BSD3-style (see LICENSE)
--
-- Maintainer: jao@gnu.org
-- Stability: unstable
-- Portability: portable
-- Created: Sun Oct 24, 2021 13:38
--
--
-- Alignment of elements
--
------------------------------------------------------------------------------


module Xmobar.Config.Align (Align(..)) where

data Align = L | R | C deriving ( Read, Show, Eq )
