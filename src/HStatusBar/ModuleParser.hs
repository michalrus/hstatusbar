module HStatusBar.ModuleParser
  ( parseModules
  ) where

import           Control.Applicative
import           Text.Megaparsec

--import HStatusBar.Types
import qualified Data.Bifunctor      as Bi

data Module
  = MText String
  | MFun String
         [String]
  deriving (Show, Eq)

parseModules :: String -> Either String [Module]
parseModules input = Bi.first parseErrorPretty $ parse parser "<argv>" input

parser :: Parsec Dec String [Module]
parser = many (ptext <|> pfun)
  where
    ptext = MText <$> some (noneOf "$" <|> (string "$$" *> pure '$'))
    pfun = char '$' *> between (string "(") (string ")") funs'

funs' :: Parsec Dec String Module
funs' =
  case try <$> funs of
    h:t -> foldl (<|>) h t
    _ -> error "No modules available." -- FIXME: use NEL?

funs :: [Parsec Dec String Module]
funs =
  [ fun 1 "time.local"
  , fun 1 "time.universal"
  , fun 2 "bspwm"
  , fun 3 "disk"
  , fun 5 "memory"
  ]

fun :: Int -> String -> Parsec Dec String Module
fun numArgs name =
  MFun <$> string name <*>
  count numArgs ((skipSome spaceChar *> stringLiteral) <?> "argument") <*
  space

stringLiteral :: Parsec Dec String String
stringLiteral =
  between (string "\"") (string "\"") $
  many (noneOf "\"" <|> (string "\\\"" *> pure '"'))
