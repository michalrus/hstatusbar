module Main
  ( main
  ) where

import           Control.Concurrent      (forkIO)
import           Control.Concurrent.Chan
import           Control.Monad
import qualified HStatusBar.Time
import           HStatusBar.Types

plainText :: String -> Module
plainText = flip writeChan

modules :: [Module]
modules =
  [ plainText "time is "
  , HStatusBar.Time.local "%a %d %b %H:%M:%S %Z"
  , plainText " • "
  , HStatusBar.Time.universal "%H:%M %Z"
  ]

main :: IO ()
main = do
  commonChan :: Chan (Int, String) <- newChan
  forM_ ([0 ..] `zip` modules) $ \(idx, mdl) -> do
    chan <- newChan
    void $ forkIO $ mdl chan
    forkIO $
      forever $ do
        val <- readChan chan
        writeChan commonChan (idx, val)
  let loop :: [String] -> IO ()
      loop vals = do
        (idx, val) <- readChan commonChan
        let newVals = editNth vals idx val
        when (newVals /= vals) $ putStrLn $ join newVals
        loop newVals
  loop $ const "" <$> modules

editNth :: [a] -> Int -> a -> [a]
editNth xs n x =
  if 0 <= n && n < Prelude.length xs
    then Prelude.take n xs ++ [x] ++ Prelude.drop (n + 1) xs
    else xs
