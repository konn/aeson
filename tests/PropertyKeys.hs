{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module PropertyKeys (keysTests) where

import Control.Applicative (Const)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Time.Calendar (DayOfWeek)
import Data.Time.Calendar.Month (Month)
import Data.Time.Calendar.Quarter (Quarter, QuarterOfYear)
import Data.Time.Compat (Day, LocalTime, TimeOfDay, UTCTime)
import qualified Data.UUID.Types as UUID
import Data.Version (Version)
import Instances ()
import Numeric.Natural (Natural)
import Prelude.Compat
import PropUtils
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Types

keysTests :: TestTree
keysTests =
  testGroup
    "roundTrip Key"
    [ testProperty "Bool" $ roundTripKey @Bool
    , testProperty "Text" $ roundTripKey @T.Text
    , testProperty "String" $ roundTripKey @String
    , testProperty "Int" $ roundTripKey @Int
    , testProperty "[Text]" $ roundTripKey @(LogScaled [T.Text])
    , testProperty "(Int,Char)" $ roundTripKey @(Int, Char)
    , testProperty "Integer" $ roundTripKey @Integer
    , testProperty "Natural" $ roundTripKey @Natural
    , testProperty "Float" $ roundTripKey @Float
    , testProperty "Double" $ roundTripKey @Double
    , testProperty "Day" $ roundTripKey @Day
    , testProperty "DayOfWeek" $ roundTripKey @DayOfWeek
    , testProperty "Month" $ roundTripKey @Month
    , testProperty "Quarter" $ roundTripKey @Quarter
    , testProperty "QuarterOfYear" $ roundTripKey @QuarterOfYear
    , testProperty "LocalTime" $ roundTripKey @LocalTime
    , testProperty "TimeOfDay" $ roundTripKey @TimeOfDay
    , testProperty "UTCTime" $ roundTripKey @UTCTime
    , testProperty "Version" $ roundTripKey @Version
    , testProperty "Lazy Text" $ roundTripKey @LT.Text
    , testProperty "UUID" $ roundTripKey @UUID.UUID
    , testProperty "Const Text" $ roundTripKey @(Const T.Text ())
    ]
