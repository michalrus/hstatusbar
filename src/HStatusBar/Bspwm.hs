module HStatusBar.Bspwm
  ( bspwm
  ) where

import           Control.Concurrent
import           Control.Monad      (join)
import           HStatusBar.Decl
import           HStatusBar.Types
import           System.Process     (proc)
import           Text.Megaparsec

bspwm :: Decl
bspwm =
  bspwm_ <$> (decl "bspwm" *> arg) <*> arg <*> arg <*> arg <*> arg <*> arg <*>
  many arg

-- FIXME: this is terrible, String madness.
bspwm_ :: String
       -> String
       -> String
       -> String
       -> String
       -> String
       -> [String]
       -> Module
bspwm_ normalPre normalPost selectedPre selectedPost urgentPre urgentPost icons =
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
        WUrgent -> urgentPre ++ nameIcon ++ urgentPost
        _
          | isSelected . state $ wspace ->
            selectedPre ++ nameIcon ++ selectedPost
        WOccupied -> normalPre ++ nameIcon ++ normalPost
        _ -> ""
      where
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
parseLine = BspLine <$> many (noneOf ":") <*> many (try wspace) <*> many rest
  where
    wspace :: Parsec Dec String Workspace
    wspace = Workspace <$> (char ':' *> wstate) <*> some (noneOf ":")
    wstate :: Parsec Dec String WState
    wstate =
      (char 'O' *> pure (WState WOccupied True)) <|>
      (char 'o' *> pure (WState WOccupied False)) <|>
      (char 'F' *> pure (WState WFree True)) <|>
      (char 'f' *> pure (WState WFree False)) <|>
      (char 'U' *> pure (WState WUrgent True)) <|>
      (char 'u' *> pure (WState WUrgent False))
    rest :: Parsec Dec String String
    rest = char ':' *> many (noneOf ":")
