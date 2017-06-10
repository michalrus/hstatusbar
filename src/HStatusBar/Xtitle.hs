module HStatusBar.Xtitle
  ( xtitle
  ) where

import           HStatusBar.Common
import           HStatusBar.Decl
import           HStatusBar.Types
import           System.Process    (proc)

xtitle :: Decl
xtitle = xtitle_ <$> (decl "xtitle" *> argInt)

xtitle_ :: Int -> Module
xtitle_ maxLen =
  processByLine (proc "xtitle" ["-s"]) $ \line ->
    flip writeChan $ ellipsis maxLen line

ellipsis :: Int -> String -> String
ellipsis maxl s =
  if length s > maxl
    then take (maxl - 1) s ++ "…"
    else s
