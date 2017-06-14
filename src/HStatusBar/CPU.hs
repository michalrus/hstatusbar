module HStatusBar.CPU
  ( cpu
  ) where

import           HStatusBar.Decl
import           HStatusBar.Types

cpu :: Decl
cpu = cpu_ <$> (decl "cpu" *> (unpack <$> arg))

cpu_ :: FilePath -> Module
cpu_ tempPath chan =
  forever $ do
    temp :: Int <-
      round . (/ 1000) . fromMaybe (0 :: Double) . readMay <$>
      readFileUtf8 tempPath
    load :: Text <-
      fromMaybe "" . headMay . words <$> readFileUtf8 "/proc/loadavg"
    writeChan chan $ tshow temp ++ "°C " ++ load
    threadDelay $ 5200 * 1000
