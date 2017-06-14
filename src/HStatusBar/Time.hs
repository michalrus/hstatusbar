module HStatusBar.Time
  ( local
  , universal
  ) where

import           Data.Time
import           HStatusBar.Decl
import           HStatusBar.Types

local :: Decl
local = common True <$> (decl "time.local" *> arg)

universal :: Decl
universal = common False <$> (decl "time.universal" *> arg)

-- TODO: make the µs algo more correct, now it’s +1.5 ms off
common :: Bool -> Text -> Module
common zoned fmt ch =
  forever $ do
    now <- getCurrentTime
    let µs =
          (diffTimeToPicoseconds (utctDayTime now) `mod` (1000000 * 1000000)) `div`
          1000000
    output :: Text <-
      pack <$>
      let fmt' = unpack fmt
      in if zoned
           then formatTime defaultTimeLocale fmt' <$> utcToLocalZonedTime now
           else return $ formatTime defaultTimeLocale fmt' now
    writeChan ch output
    threadDelay $ 1000000 - fromInteger µs
