module HStatusBar.Time
  ( local
  , universal
  ) where

import           Control.Concurrent
import           Control.Monad
import           Data.Time
import           HStatusBar.Decl
import           HStatusBar.Types

local :: Decl
local = common getZonedTime <$> (decl "time.local" *> arg)

universal :: Decl
universal = common getCurrentTime <$> (decl "time.universal" *> arg)

-- TODO: make sure to sync µs to 0 every few minutes
common
  :: FormatTime t
  => IO t -> String -> Module
common src fmt ch =
  forever $ do
    now <- src
    writeChan ch (formatTime defaultTimeLocale fmt now)
    threadDelay $ 1000 * 1000
