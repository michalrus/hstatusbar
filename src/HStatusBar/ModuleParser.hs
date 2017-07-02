module HStatusBar.ModuleParser
  ( parseModules
  ) where

import qualified Data.Bifunctor       as Bi
import qualified HStatusBar.Alsa
import qualified HStatusBar.Battery
import qualified HStatusBar.Bspwm
import qualified HStatusBar.CPU
import qualified HStatusBar.Disk
import qualified HStatusBar.Memory
import qualified HStatusBar.Time
import           HStatusBar.Types
import qualified HStatusBar.Xtitle
import           Text.Megaparsec      as MP
import           Text.Megaparsec.Text

parseModules :: Text -> Either String [Module]
parseModules input = Bi.first parseErrorPretty $ parse parser "<argv>" input

parser :: Parser [Module]
parser = many (textP <|> funcP)
  where
    textP =
      plainText <$>
      (pack <$> some (noneOf ("$" :: String) <|> (string "$$" *> pure '$')))
    funcP = char '$' *> between (string "(") (string ")") (funs' <* space)

plainText :: Text -> Module
plainText = flip writeChan

funs' :: Parser Module
funs' =
  case MP.try <$> funs of
    h:t -> foldl' (<|>) h t
    _ -> error "No modules available." -- FIXME: use NEL?

funs :: [Parser Module]
funs =
  [ HStatusBar.Time.local
  , HStatusBar.Time.universal
  , HStatusBar.Bspwm.bspwm
  , HStatusBar.Xtitle.xtitle
  , HStatusBar.Memory.memory
  , HStatusBar.CPU.cpu
  , HStatusBar.Disk.disk
  , HStatusBar.Battery.battery
  , HStatusBar.Alsa.volume
  ]
