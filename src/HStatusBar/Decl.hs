module HStatusBar.Decl
  ( Decl
  , decl
  , arg
  ) where

import           Control.Applicative
import           HStatusBar.Types
import           Text.Megaparsec

type Decl = Parsec Dec String Module

decl :: String -> Parsec Dec String ()
decl nme = string nme *> pure ()

arg :: Parsec Dec String String
arg = (skipSome spaceChar *> stringLiteral) <?> "argument"

stringLiteral :: Parsec Dec String String
stringLiteral =
  between (string "\"") (string "\"") $
  many (noneOf "\"" <|> (string "\\\"" *> pure '"'))
