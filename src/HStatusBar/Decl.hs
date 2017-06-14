module HStatusBar.Decl
  ( Decl
  , decl
  , arg
  , argInt
  ) where

import           HStatusBar.Types
import           Text.Megaparsec      hiding (string)
import qualified Text.Megaparsec      as MP
import           Text.Megaparsec.Text

type Decl = Parser Module

string :: Text -> Parser Text
string literal = pack <$> MP.string (unpack literal)

decl :: Text -> Parser ()
decl nme = string nme *> pure ()

arg :: Parser Text
arg = arg' stringLiteral

argInt :: Parser Int
argInt = arg' intLiteral

arg' :: Parser a -> Parser a
arg' literal = (skipSome spaceChar *> literal) <?> "argument"

stringLiteral :: Parser Text
stringLiteral =
  between
    (string "\"")
    (string "\"")
    (pack <$> many ((string "\\\"" *> pure '"') <|> noneOf ("\"" :: String)) :: Parser Text) <?>
  "string literal"

intLiteral :: Parser Int
intLiteral = (read <$> some digitChar) <?> "integer literal"
  where
    read = fromMaybe 0 . readMay -- 0 won’t happen
