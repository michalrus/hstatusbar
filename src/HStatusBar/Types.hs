module HStatusBar.Types
  ( Module
  ) where

import           ClassyPrelude

type Module = Chan String -> IO ()
