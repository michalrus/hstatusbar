module HStatusBar.Common
  ( processByLine
  , humanSI
  , customFormat
  , blink
  ) where

import           Control.Monad.Loops (whileM_)
import           Data.List           (iterate)
import qualified Data.Map            as M
import           Data.Text.IO
import           HStatusBar.Types
import           Numeric             (showFFloat)
import           System.IO           (hIsEOF)
import           System.Process

-- | Set buffering to line-based for stderr & stdout, and to none for stdin for the child process.
--
-- FIXME: Now it just wraps the command in `stdbuf -i0 -o0 -e0 -- …`. Find something better?
--
-- * https://mail.haskell.org/pipermail/haskell-cafe/2014-August/115399.html
-- * https://mail.haskell.org/pipermail/haskell-cafe/2014-August/115401.html
noBuffering :: CreateProcess -> CreateProcess
noBuffering cp = cp {cmdspec = updSpec $ cmdspec cp} -- FIXME: lens?
  where
    updSpec (RawCommand cmd args) =
      RawCommand "stdbuf" $ ["-i0", "-oL", "-eL", "--", cmd] ++ args
    updSpec (ShellCommand _) = error "Please, don’t use ShellCommand…" -- FIXME

processByLine :: CreateProcess -> (Text -> Module) -> Module
processByLine spec submodule chan =
  forever $ do
    (Nothing, Just hOut, Nothing, hProc) <-
      createProcess $
      noBuffering $
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

blink :: (Text -> IO ()) -> [Text] -> Text -> IO ()
blink display fmts text =
  forM_ fmts $ \fmt -> do
    display $ customFormat (M.fromList [('s', text)]) fmt
    threadDelay $ 600 * 1000
