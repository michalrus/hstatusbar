module HStatusBar.Types
  ( Module
  ) where

type Module = Chan String -> IO ()
