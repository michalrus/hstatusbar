module Main
  ( main
  ) where

import           Control.Concurrent      (forkIO)
import qualified GHC.IO.Handle           as IO
import qualified GHC.IO.Handle.FD        as IO
import           HStatusBar.ModuleParser
import qualified System.Environment      as IO

main :: IO ()
main = do
  IO.hSetBuffering IO.stdout IO.LineBuffering
  args <- (pack <$>) <$> IO.getArgs
  let modules =
        case parseModules (unwords args) of
          Left err -> error err
          Right mods -> mods
  commonChan :: Chan (Int, Text) <- newChan
  forM_ ([0 ..] `zip` modules) $ \(idx, mdl) -> do
    chan <- newChan
    void $ forkIO $ mdl chan
    void $
      forkIO $
      forever $ do
        val <- readChan chan
        writeChan commonChan (idx, val)
  let loop :: [Text] -> IO ()
      loop vals = do
        (idx, val) <- readChan commonChan
        let newVals = editNth vals idx val
        when (newVals /= vals) $ putStrLn $ concat newVals
        loop newVals
  loop $ const "" <$> modules

editNth :: [a] -> Int -> a -> [a]
editNth xs n x =
  if 0 <= n && n < length xs
    then take n xs ++ [x] ++ drop (n + 1) xs
    else xs
