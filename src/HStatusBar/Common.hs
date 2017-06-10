module HStatusBar.Common
  ( processByLine
  , humanSI
  , customFormat
  ) where

import           Control.Monad.Loops (whileM_)
import           Data.List           (iterate)
import qualified Data.Map            as M
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

customFormat :: M.Map Char String -> String -> String
customFormat _ [] = []
customFormat mapping ('%':c:fmt) =
  M.findWithDefault ['%', c] c mapping ++ customFormat mapping fmt
customFormat mapping (c:fmt) = c : customFormat mapping fmt
