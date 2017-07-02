module HStatusBar.Battery
  ( battery
  ) where

import           Control.Monad        (forever)
import qualified Data.Map             as M
import           HStatusBar.Common
import           HStatusBar.Decl
import           HStatusBar.Types
import           Text.Megaparsec
import           Text.Megaparsec.Text

battery :: Decl
battery =
  battery_ <$> (decl "battery" *> (unpack <$> arg)) <*> arg <*> arg <*> arg <*>
  argInt <*>
  arg <*>
  arg

-- FIXME: make warnFmts configurable globally (i.e. Module is not just `-> IO ()`)
battery_ :: FilePath -> Text -> Text -> Text -> Int -> Text -> Text -> Module
battery_ uevent iconCharging iconFull iconDischarging warnCapacity warnFmt1 warnFmt2 chan =
  forever $ do
    contents <- parseUevent <$> readFileUtf8 uevent
    let readD :: Text -> Double
        readD sym = fromMaybe 0.0 $ readMay =<< M.lookup sym contents
        timeLeft :: Double -> Double -> Text
        timeLeft charge current' =
          let hours = charge / current'
              hours' :: Integer = floor hours
              mins' :: Integer = round $ 60 * (hours - fromIntegral hours')
          in (if hours' /= 0
                then tshow hours' <> "h"
                else "") <>
             tshow mins' <>
             "m"
        capacity = readD "POWER_SUPPLY_CAPACITY"
        current = readD "POWER_SUPPLY_CURRENT_NOW"
        displayNormal :: Text -> IO ()
        displayNormal t = writeChan chan t >> threadDelay (4100 * 1000)
        displayWarn :: Text -> IO ()
        displayWarn = blink (writeChan chan) [warnFmt1, warnFmt2]
        render :: Text -> Maybe Double -> Text
        render icon charge =
          let time = maybe "" (\chr -> " " <> timeLeft chr current) charge
          in icon <> " " <> tshow (round capacity :: Integer) <> "%" <> time
    case toUpper <$> M.lookup "POWER_SUPPLY_STATUS" contents of
      Just "DISCHARGING" -> do
        let text =
              render iconDischarging $ Just $ readD "POWER_SUPPLY_CHARGE_NOW"
        (if capacity <= fromIntegral warnCapacity
           then displayWarn
           else displayNormal)
          text
      Just "CHARGING" -> do
        let chargeLeft =
              readD "POWER_SUPPLY_CHARGE_FULL" - readD "POWER_SUPPLY_CHARGE_NOW"
        displayNormal $ render iconCharging $ Just chargeLeft
      _ -> displayNormal $ render iconFull Nothing

parseUevent :: Text -> Map Text Text
parseUevent raw = M.fromList $ catMaybes $ parseMaybe line <$> lines raw
  where
    line :: Parser (Text, Text)
    line =
      (,) <$> (pack <$> some (noneOf ("=" :: String))) <* optional space <*
      char '=' <*
      optional space <*>
      (pack <$> many anyChar <*) eof
