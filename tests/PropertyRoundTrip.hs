{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module PropertyRoundTrip (roundTripTests) where

import Control.Applicative (Const)
import Data.Aeson.Types
import Data.DList (DList)
import qualified Data.Fix as F
import Data.Int (Int8)
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import qualified Data.Monoid as Monoid
import Data.Proxy (Proxy)
import Data.Ratio (Ratio)
import Data.Sequence (Seq)
import qualified Data.Strict as S
import Data.Tagged (Tagged)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Short as ST
import Data.These (These (..))
import Data.Time (Day, DiffTime, LocalTime, NominalDiffTime, TimeOfDay, UTCTime, ZonedTime)
import Data.Time.Calendar (CalendarDiffDays, DayOfWeek)
import Data.Time.Calendar.Month (Month)
import Data.Time.Calendar.Quarter (Quarter, QuarterOfYear)
import Data.Time.Clock.System (SystemTime)
import Data.Time.LocalTime (CalendarDiffTime)
import Data.Tuple.Solo (Solo)
import qualified Data.UUID.Types as UUID
import Data.Version (Version)
import Instances ()
import Network.URI (URI)
import Numeric.Natural (Natural)
import Prelude.Compat
import PropUtils
import PropertyRTFunctors
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Types

roundTripTests :: TestTree
roundTripTests =
  testGroup
    "roundTrip"
    [ testProperty "()" $ roundTripEq @()
    , testProperty "Value" $ roundTripEq @Value
    , testProperty "Bool" $ roundTripEq @Bool
    , testProperty "Double" $ roundTripEq @(Approx Double)
    , testProperty "Int" $ roundTripEq @Int
    , testProperty "NonEmpty Char" $ roundTripEq @(NonEmpty Char)
    , testProperty "Integer" $ roundTripEq @Integer
    , testProperty "String" $ roundTripEq @String
    , testProperty "Text" $ roundTripEq @T.Text
    , testProperty "Lazy Text" $ roundTripEq @LT.Text
    , testProperty "Foo" $ roundTripEq @Foo
    , testProperty "Day" $ roundTripEq @Day
    , testProperty "Month" $ roundTripEq @Month
    , testProperty "Quarter" $ roundTripEq @Quarter
    , testProperty "QuarterOfYear" $ roundTripEq @QuarterOfYear
    , testProperty "BCE Day" $ roundTripEq @BCEDay
    , testProperty "DotNetTime" $ roundTripEq @(Approx DotNetTime)
    , testProperty "LocalTime" $ roundTripEq @LocalTime
    , testProperty "TimeOfDay" $ roundTripEq @TimeOfDay
    , testProperty "UTCTime" $ roundTripEq @UTCTime
    , testProperty "ZonedTime" $ roundTripEq @ZonedTime
    , testProperty "NominalDiffTime" $ roundTripEq @NominalDiffTime
    , testProperty "DiffTime" $ roundTripEq @DiffTime
    , testProperty "DayOfWeek" $ roundTripEq @DayOfWeek
    , testProperty "SystemTime" $ roundTripEq @SystemTime
    , testProperty "CalendarDiffTime" $ roundTripEq @CalendarDiffTime
    , testProperty "CalendarDiffDays" $ roundTripEq @CalendarDiffDays
    , testProperty "Version" $ roundTripEq @Version
    , testProperty "Natural" $ roundTripEq @Natural
    , testProperty "Proxy" $ roundTripEq @(Proxy Int)
    , testProperty "Tagged" $ roundTripEq @(Tagged Int Char)
    , testProperty "Const" $ roundTripEq @(Const Int Char)
    , testProperty "DList" $ roundTripEq @(DList Int)
    , testProperty "Seq" $ roundTripEq @(Seq Int)
    , testProperty "Rational" $ roundTripEq @Rational
    , testProperty "Ratio Int" $ roundTripEq @(Ratio Int)
    , testProperty "UUID" $ roundTripEq @UUID.UUID
    , testProperty "These" $ roundTripEq @(These Char Bool)
    , testProperty "Fix" $ roundTripEq @(F.Fix (These Char))
    , testProperty "Mu" $ roundTripEq @(F.Mu (These Char))
    , testProperty "Nu" $ roundTripEq @(F.Nu (These Char))
    , testProperty "Maybe" $ roundTripEq @(Maybe Int)
    , testProperty "Monoid.First" $ roundTripEq @(Monoid.First Int)
    , testProperty "Strict Pair" $ roundTripEq @(S.Pair Int Char)
    , testProperty "Strict Either" $ roundTripEq @(S.Either Int Char)
    , testProperty "Strict These" $ roundTripEq @(S.These Int Char)
    , testProperty "Strict Maybe" $ roundTripEq @(S.Maybe Int)
    , testProperty "Solo Int" $ roundTripEq @(Solo Int)
    , testProperty "ShortText" $ roundTripEq @ST.ShortText
    , testProperty "URI" $ roundTripEq @URI
    , roundTripFunctorsTests
    , testGroup
        "ghcGenerics"
        [ testProperty "OneConstructor" $ roundTripEq OneConstructor
        , testProperty "Product2" $ roundTripEq @(Product2 Int Bool)
        , testProperty "Product6" $ roundTripEq @(Product6 Int Bool String (Approx Double) (Int, Approx Double) ())
        , testProperty "Sum4" $ roundTripEq @(Sum4 Int8 ZonedTime T.Text (Map String Int))
        ]
    ]
