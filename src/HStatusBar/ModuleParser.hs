module HStatusBar.ModuleParser
  ( parseModules
  ) where

import qualified Data.Bifunctor     as Bi
import qualified HStatusBar.Bspwm
import qualified HStatusBar.CPU
import qualified HStatusBar.Disk
import qualified HStatusBar.Memory
import qualified HStatusBar.Time
import           HStatusBar.Types
import qualified HStatusBar.Xtitle
import           Text.Megaparsec    as MP

parseModules :: String -> Either String [Module]
parseModules input = Bi.first parseErrorPretty $ parse parser "<argv>" input

parser :: Parsec Dec String [Module]
parser = many (textP <|> funcP)
  where
    textP =
      plainText <$> some (noneOf ("$" :: String) <|> (string "$$" *> pure '$'))
    funcP = char '$' *> between (string "(") (string ")") (funs' <* space)

plainText :: String -> Module
plainText = flip writeChan

funs' :: Parsec Dec String Module
funs' =
  case MP.try <$> funs of
    h:t -> foldl' (<|>) h t
    _ -> error "No modules available." -- FIXME: use NEL?

funs :: [Parsec Dec String Module]
funs =
  [ HStatusBar.Time.local
  , HStatusBar.Time.universal
  , HStatusBar.Bspwm.bspwm
  , HStatusBar.Xtitle.xtitle
  , HStatusBar.Memory.memory
  , HStatusBar.CPU.cpu
  , HStatusBar.Disk.disk
  ]
