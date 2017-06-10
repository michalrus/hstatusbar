module HStatusBar.Bspwm
  ( bspwm
  ) where

import           ClassyPrelude     hiding (try)
import           Control.Monad     (join)
import qualified Data.Map          as M
import           HStatusBar.Common
import           HStatusBar.Decl
import           HStatusBar.Types
import           System.Process    (proc)
import           Text.Megaparsec

bspwm :: Decl
bspwm = bspwm_ <$> (decl "bspwm" *> arg) <*> arg <*> arg <*> many arg

-- FIXME: this is terrible, String madness.
bspwm_ :: String -> String -> String -> [String] -> Module
bspwm_ normalF selectedF urgentF icons =
  processByLine (proc "bspc" ["subscribe"]) $ \line ch ->
    case parseMaybe parseLine line of
      Just (BspLine _ wspaces _) ->
        writeChan ch $
        join $ showWspace <$> (wspaces `zip` (icons ++ repeat ""))
      _ -> pure ()
  where
    showWspace :: (Workspace, String) -> String
    showWspace (wspace, icon) =
      case tpe . state $ wspace of
        WUrgent -> format urgentF
        _
          | isSelected . state $ wspace -> format selectedF
        WOccupied -> format normalF
        _ -> "" -- TODO: maybe add `unoccupiedF`?
      where
        format = customFormat $ M.fromList [('i', nameIcon), ('n', name wspace)]
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
  , name :: String
  }

data BspLine =
  BspLine String
          [Workspace]
          [String]

parseLine :: Parsec Dec String BspLine
parseLine =
  BspLine <$> many (noneOf (":" :: String)) <*> many (try wspace) <*> many rest
  where
    wspace :: Parsec Dec String Workspace
    wspace =
      Workspace <$> (char ':' *> wstate) <*> some (noneOf (":" :: String))
    wstate :: Parsec Dec String WState
    wstate =
      (char 'O' *> pure (WState WOccupied True)) <|>
      (char 'o' *> pure (WState WOccupied False)) <|>
      (char 'F' *> pure (WState WFree True)) <|>
      (char 'f' *> pure (WState WFree False)) <|>
      (char 'U' *> pure (WState WUrgent True)) <|>
      (char 'u' *> pure (WState WUrgent False))
    rest :: Parsec Dec String String
    rest = char ':' *> many (noneOf (":" :: String))
