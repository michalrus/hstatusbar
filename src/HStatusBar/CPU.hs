module HStatusBar.CPU
  ( cpu
  ) where

import           ClassyPrelude
import           Control.Monad    (forever)
import           HStatusBar.Decl
import           HStatusBar.Types
import           Prelude          (read)

cpu :: Decl
cpu = cpu_ <$> (decl "cpu" *> arg)

cpu_ :: FilePath -> Module
cpu_ tempPath chan =
  forever $ do
    temp :: Int <-
      round . (/ 1000) . (read :: String -> Double) . unpack <$>
      readFileUtf8 tempPath -- FIXME: Text
    load :: String <-
      (\case
         h:_ -> h
         _ -> "") .
      words . unpack <$>
      readFileUtf8 "/proc/loadavg" -- FIXME: Text
    writeChan chan $ show temp ++ "°C " ++ load
    threadDelay $ 5200 * 1000
