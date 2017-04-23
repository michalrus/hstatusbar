module HStatusBar.Disk
  ( disk
  ) where

import           Control.Concurrent
import           Control.Monad        (forM, forever)
import           HStatusBar.Decl
import           HStatusBar.Types
import           System.Posix.StatVFS
import qualified Text.Megaparsec      as P

disk :: Decl
disk = disk_ <$> (decl "disk" *> P.many arg)

disk_ :: [FilePath] -> Module
disk_ paths chan =
  forever $ do
    stats <- forM paths statVFS
    let avail s =
          fromIntegral (statVFS_bsize s) * fromIntegral (statVFS_bavail s)
    let addLabels as = (\(p, a) -> p ++ "=" ++ a) <$> paths `zip` as
    writeChan chan $ unwords $ addLabels $ humanSI 0 . avail <$> stats
    threadDelay $ 10700 * 1000
