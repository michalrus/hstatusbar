module HStatusBar.Decl
  ( Decl
  , decl
  , arg
  , argInt
  ) where

-- FIXME: fromJust
import           Data.Maybe       (fromJust)
import           HStatusBar.Types
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
    (many ((string "\\\"" *> pure '"') <|> noneOf ("\"" :: String))) <?>
  "string literal"

intLiteral :: Parsec Dec String Int
intLiteral = (fromJust . readMay <$> some digitChar) <?> "integer literal"
