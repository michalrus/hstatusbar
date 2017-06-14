module HStatusBar.Bspwm
  ( bspwm
  ) where

import qualified Data.Map             as Map (fromList)
import           HStatusBar.Common
import           HStatusBar.Decl
import           HStatusBar.Types
import           System.Process       (proc)
import           Text.Megaparsec      as MP
import           Text.Megaparsec.Text

bspwm :: Decl
bspwm = bspwm_ <$> (decl "bspwm" *> arg) <*> arg <*> arg <*> many arg

-- FIXME: this is terrible, string madness.
bspwm_ :: Text -> Text -> Text -> [Text] -> Module
bspwm_ normalF selectedF urgentF icons =
  processByLine (proc "bspc" ["subscribe"]) $ \line ch ->
    case parseMaybe parseLine line of
      Just (BspLine _ wspaces _) ->
        writeChan ch $
        concat $ showWspace <$> (wspaces `zip` (icons ++ repeat ""))
      _ -> pure ()
  where
    showWspace :: (Workspace, Text) -> Text
    showWspace (wspace, icon) =
      case tpe . state $ wspace of
        WUrgent -> format urgentF
        _
          | isSelected . state $ wspace -> format selectedF
        WOccupied -> format normalF
        _ -> "" -- TODO: maybe add `unoccupiedF`?
      where
        format =
          customFormat $ Map.fromList [('i', nameIcon), ('n', name wspace)]
        nameIcon =
          if icon == ""
            then name wspace
            else icon

data WType
  = WOccupied
  | WFree
  | WUrgent

data WState = WState
  { tpe :: WType
  , isSelected :: Bool
  }

data Workspace = Workspace
  { state :: WState
  , name :: Text
  }

data BspLine =
  BspLine Text
          [Workspace]
          [Text]

parseLine :: Parser BspLine
parseLine =
  BspLine <$> (pack <$> many (noneOf (":" :: String))) <*> many (MP.try wspace) <*>
  many rest
  where
    wspace :: Parser Workspace
    wspace =
      Workspace <$> (char ':' *> wstate) <*>
      (pack <$> some (noneOf (":" :: String)))
    wstate :: Parser WState
    wstate =
      (char 'O' *> pure (WState WOccupied True)) <|>
      (char 'o' *> pure (WState WOccupied False)) <|>
      (char 'F' *> pure (WState WFree True)) <|>
      (char 'f' *> pure (WState WFree False)) <|>
      (char 'U' *> pure (WState WUrgent True)) <|>
      (char 'u' *> pure (WState WUrgent False))
    rest :: Parser Text
    rest = char ':' *> (pack <$> many (noneOf (":" :: String)))
