module HStatusBar.Types
  ( Module
  ) where

import           Control.Concurrent.Chan

type Module = Chan String -> IO ()
