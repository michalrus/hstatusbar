module HStatusBar.Time
  ( local
  , universal
  ) where

import           Control.Concurrent
import           Control.Monad
import           Data.Time
import           HStatusBar.Types

local :: String -> Module
local = common getZonedTime

universal :: String -> Module
universal = common getCurrentTime

-- TODO: make sure to sync µs to 0 every few minutes
common
  :: FormatTime t
  => IO t -> String -> Module
common src fmt ch =
  forever $ do
    now <- src
    writeChan ch (formatTime defaultTimeLocale fmt now)
    threadDelay $ 1000 * 1000
