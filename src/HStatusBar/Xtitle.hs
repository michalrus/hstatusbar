module HStatusBar.Xtitle
  ( xtitle
  ) where

import           Control.Concurrent
import           HStatusBar.Decl
import           HStatusBar.Types
import           System.Process     (proc)

xtitle :: Decl
xtitle = xtitle_ <$ decl "xtitle"

xtitle_ :: Module
xtitle_ = processByLine (proc "xtitle" ["-s"]) $ flip writeChan
