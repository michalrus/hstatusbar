module HStatusBar.Bspwm
  ( bspwm
  ) where

import           Control.Concurrent
import           HStatusBar.Decl
import           HStatusBar.Types
import           System.Process     (proc)

bspwm :: Decl
bspwm = bspwm_ <$ decl "bspwm"

bspwm_ :: Module
bspwm_ = processByLine (proc "bspc" ["subscribe"]) $ flip writeChan
