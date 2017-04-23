module HStatusBar.Memory
  ( memory
  ) where

import           Control.Concurrent
import           Control.Monad      (forever)
import qualified Data.Map           as Map
import           Data.Maybe         (catMaybes)
import           HStatusBar.Decl
import           HStatusBar.Types
import           Text.Megaparsec

memory :: Decl
memory = memory_ <$ decl "memory"

memory_ :: Module
memory_ chan =
  forever $ do
    contents <- parseMeminfo <$> readFile "/proc/meminfo"
    case catMaybes $
         flip Map.lookup contents <$>
         [ "MemTotal"
         , "MemFree"
         , "Cached"
         , "SReclaimable"
         , "Buffers"
         , "SwapTotal"
         , "SwapFree"
         ] of
      [memTotal, memFree, cached, sReclaimable, buffers, swapTotal, swapFree]
      -- See https://gitlab.com/procps-ng/procps/blob/master/proc/sysinfo.c for these formulas:
       -> do
        let mainUsed = memTotal - memFree - (cached + sReclaimable) - buffers
        let swapUsed = swapTotal - swapFree
        writeChan chan $
          humanSI 1 (mainUsed * 1024) ++ " " ++ humanSI 1 (swapUsed * 1024)
      _ -> pure ()
    threadDelay $ 2300 * 1000

parseMeminfo :: String -> Map.Map String Integer
parseMeminfo raw = Map.fromList $ catMaybes $ parseMaybe line <$> lines raw
  where
    line :: Parsec Dec String (String, Integer)
    line =
      (,) <$> some (noneOf ":") <* char ':' <* space <*>
      (read <$> some digitChar) <*
      optional (space <* string "kB")
