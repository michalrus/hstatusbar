module HStatusBar.Common
  ( processByLine
  , humanSI
  , customFormat
  ) where

import           Control.Monad.Loops (whileM_)
import           Data.List           (iterate)
import           Data.Text.IO
import           HStatusBar.Types
import           Numeric             (showFFloat)
import           System.IO           (hIsEOF)
import           System.Process

processByLine :: CreateProcess -> (Text -> Module) -> Module
processByLine spec submodule chan =
  forever $ do
    (Nothing, Just hOut, Nothing, hProc) <-
      createProcess $
      spec {std_in = NoStream, std_out = CreatePipe, std_err = Inherit}
    whileM_ (not <$> hIsEOF hOut) $ do
      line <- hGetLine hOut
      submodule line chan
    terminateProcess hProc
    threadDelay 1000000

humanSI :: Int -> Integer -> Text
humanSI decimals num = pack . loop $ units
  where
    loop (h:t) =
      if num >= fst h
        then showFFloat
               (Just decimals)
               ((fromIntegral num / fromIntegral (fst h)) :: Double) $
             snd h
        else loop t
    loop [] = show num ++ "B"
    units =
      reverse $
      iterate (* 1024) (1024 :: Integer) `zip` ["K", "M", "G", "T", "P", "E"]

customFormat :: Map Char Text -> Text -> Text
customFormat mapping fmt = pack $ go (unpack <$> mapping) (unpack fmt)
  where
    go :: Map Char String -> String -> String
    go _ [] = []
    go smapping ('%':c:sfmt) =
      findWithDefault ['%', c] c smapping ++ go smapping sfmt
    go smapping (c:sfmt) = c : go smapping sfmt
