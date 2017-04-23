module HStatusBar.Decl
  ( Decl
  , decl
  , arg
  , processByLine
  ) where

import           Control.Applicative
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Loops (whileM_)
import           HStatusBar.Types
import           System.IO
import           System.Process
import           Text.Megaparsec

type Decl = Parsec Dec String Module

decl :: String -> Parsec Dec String ()
decl nme = string nme *> pure ()

arg :: Parsec Dec String String
arg = (skipSome spaceChar *> stringLiteral) <?> "argument"

stringLiteral :: Parsec Dec String String
stringLiteral =
  between (string "\"") (string "\"") $
  many ((string "\\\"" *> pure '"') <|> noneOf "\"")

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
