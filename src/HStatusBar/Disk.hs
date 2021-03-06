module HStatusBar.Disk
  ( disk
  ) where

import           HStatusBar.Common
import           HStatusBar.Decl
import           HStatusBar.Types
import           System.Posix.StatVFS
import qualified Text.Megaparsec      as MP

disk :: Decl
disk = disk_ <$> (decl "disk" *> ((unpack <$>) <$> MP.many arg))

disk_ :: [FilePath] -> Module
disk_ paths chan =
  forever $ do
    stats <- forM paths statVFS
    let avail s =
          fromIntegral (statVFS_bsize s) * fromIntegral (statVFS_bavail s)
    let tpaths :: [Text] = pack <$> paths
    let addLabels as = (\(p, a) -> p ++ "=" ++ a) <$> tpaths `zip` as
    writeChan chan $ unwords $ addLabels $ humanSI 0 . avail <$> stats
    threadDelay $ 10700 * 1000
