module HStatusBar.Decl
  ( Decl
  , decl
  , arg
  , argInt
  , processByLine
  , humanSI
  ) where

import           Control.Applicative
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Loops (whileM_)
import           HStatusBar.Types
import           Numeric             (showFFloat)
import           System.IO
import           System.Process
import           Text.Megaparsec

type Decl = Parsec Dec String Module

decl :: String -> Parsec Dec String ()
decl nme = string nme *> pure ()

arg :: Parsec Dec String String
arg = arg' stringLiteral

argInt :: Parsec Dec String Int
argInt = arg' intLiteral

arg' :: Parsec Dec String a -> Parsec Dec String a
arg' literal = (skipSome spaceChar *> literal) <?> "argument"

stringLiteral :: Parsec Dec String String
stringLiteral =
  between
    (string "\"")
    (string "\"")
    (many ((string "\\\"" *> pure '"') <|> noneOf "\"")) <?>
  "string literal"

intLiteral :: Parsec Dec String Int
intLiteral = (read <$> some digitChar) <?> "integer literal"

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
