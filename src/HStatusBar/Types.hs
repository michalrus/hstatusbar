module HStatusBar.Types
  ( Module
  ) where

type Module = Chan Text -> IO ()
