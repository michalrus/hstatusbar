module HStatusBar.Alsa
  ( volume
  ) where

import qualified Data.Map          as M
import           HStatusBar.Common
import           HStatusBar.Decl
import           HStatusBar.Types
import           System.Process    (proc, readCreateProcess)
import           Text.Regex.PCRE

volume :: Decl
volume = volume_ <$> (decl "alsa.volume" *> arg) <*> arg

-- | FIXME: very-very inefficient…
volume_ :: Text -> Text -> Module
volume_ fmtMuted fmtNormal chan = do
  refresh
  (processByLine (proc "alsactl" ["monitor"]) $ \_ _ -> refresh) chan
  where
    refresh = do
      output <- readCreateProcess (proc "amixer" ["sget", "Master"]) ""
      let matches = output =~ ("\\[(\\d+)%\\]" :: String) :: [[String]]
          nums :: [Int] =
            catMaybes $ (readMay =<<) . headMay . drop 1 <$> matches
          avg :: Int =
            round $
            fromIntegral (sum nums) / (fromIntegral (length nums) :: Double)
          fmt =
            if avg > 0
              then fmtNormal
              else fmtMuted
      writeChan chan $ customFormat (M.fromList [('v', tshow avg)]) fmt
