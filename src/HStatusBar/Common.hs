module HStatusBar.Common
  ( processByLine
  , humanSI
  ) where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Loops (whileM_)
import           HStatusBar.Types
import           Numeric             (showFFloat)
import           System.IO
import           System.Process

processByLine :: CreateProcess -> (String -> Module) -> Module
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

humanSI :: Int -> Integer -> String
humanSI decimals num = loop units
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
