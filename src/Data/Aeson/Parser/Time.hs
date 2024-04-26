module Data.Aeson.Parser.Time (
  run,
  FT.parseDay,
  FT.parseMonth,
  FT.parseQuarterOfYear,
  FT.parseLocalTime,
  FT.parseTimeOfDay,
  FT.parseUTCTime,
  FT.parseZonedTime,
) where

import qualified Data.Aeson.Types.Internal as Aeson
import Data.Text (Text)
import qualified Data.Time.FromText as FT

type Parser a = Text -> Either String a

-- | Run a @text-iso8601@ parser as an aeson parser.
run :: Parser a -> Text -> Aeson.Parser a
run f t = case f t of
  Left err -> fail $ "could not parse date: " ++ err
  Right r -> return r
{-# INLINE run #-}
