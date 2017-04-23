module HStatusBar.CPU
  ( cpu
  ) where

import           Control.Concurrent
import           Control.Monad      (forever)
import           HStatusBar.Decl
import           HStatusBar.Types

cpu :: Decl
cpu = cpu_ <$> (decl "cpu" *> arg)

cpu_ :: FilePath -> Module
cpu_ tempPath chan =
  forever $ do
    temp :: Int <-
      round . (/ 1000) . (read :: String -> Double) <$> readFile tempPath
    load :: String <-
      (\case
         h:_ -> h
         _ -> "") .
      words <$>
      readFile "/proc/loadavg"
    writeChan chan $ show temp ++ "°C " ++ load
    threadDelay $ 5200 * 1000
