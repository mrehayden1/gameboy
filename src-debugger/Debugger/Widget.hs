module Debugger.Widget (
  Widget
) where

import Control.Monad.Fix
import Control.Monad.IO.Class

import Reflex
import Reflex.Vty

type Widget t m =
  ( Adjustable t m
  , HasLayout t m
  , HasInput t m
  , HasImageWriter t m
  , HasDisplayRegion t m
  , HasFocus t m
  , HasFocusReader t m
  , HasTheme t m
  , MonadFix m
  , MonadHold t m
  , MonadIO m
  , MonadIO (Performable m)
  , PerformEvent t m
  , PostBuild t m
  , TriggerEvent t m
  )
