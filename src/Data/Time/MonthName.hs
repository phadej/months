{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE Trustworthy        #-}
-- | 'Month' data type.
module Data.Time.MonthName (
    -- * Types
    MonthName (..),
    -- * Conversion with Day
    dayToYearMonthName,
    firstDayOfYearMonthName,
    lastDayOfYearMonthName,
#ifdef MIN_VERSION_intervals
    yearMonthNameInterval,
#endif
    -- * Conversions with Text
    monthNameToText,
    parseMonthName,
    yearMonthNameToText,
    parseYearMonthName,
) where

import Control.Applicative ((<|>))
import Control.DeepSeq     (NFData (..))
import Data.Bits           ((.&.))
import Data.Char           (ord)
import Data.Hashable       (Hashable (..))
import Data.Text           (Text)
import Data.Time.Compat
       (Year, Day, fromGregorian, gregorianMonthLength, toGregorian)
import Data.Typeable       (Typeable)
import GHC.Generics        (Generic)
import Test.QuickCheck     (Arbitrary (..), arbitraryBoundedEnum)

import qualified Data.Attoparsec.Text     as AT
import qualified Data.Text                as T

#ifdef MIN_VERSION_aeson
import Data.Aeson
       (FromJSON (..), FromJSONKey (..), ToJSON (..), ToJSONKey (..), withText)
import Data.Aeson.Types (FromJSONKeyFunction (..), toJSONKeyText)
#endif

#ifdef MIN_VERSION_intervals
import Numeric.Interval.NonEmpty (Interval, (...))
#endif

-------------------------------------------------------------------------------
-- Month
-------------------------------------------------------------------------------

-- | We explicitly enumerate month names. Using an 'Int' is unsafe.
data MonthName
    = January
    | February
    | March
    | April
    | May
    | June
    | July
    | August
    | September
    | October
    | November
    | December
  deriving (Eq, Ord, Show, Read, Generic, Typeable, Bounded)

instance Hashable MonthName where
    hashWithSalt salt m = hashWithSalt salt (fromEnum m)

instance NFData MonthName where
    rnf m = m `seq` ()

instance Enum MonthName where
    fromEnum January   = 1
    fromEnum February  = 2
    fromEnum March     = 3
    fromEnum April     = 4
    fromEnum May       = 5
    fromEnum June      = 6
    fromEnum July      = 7
    fromEnum August    = 8
    fromEnum September = 9
    fromEnum October   = 10
    fromEnum November  = 11
    fromEnum December  = 12

    toEnum 1  = January
    toEnum 2  = February
    toEnum 3  = March
    toEnum 4  = April
    toEnum 5  = May
    toEnum 6  = June
    toEnum 7  = July
    toEnum 8  = August
    toEnum 9  = September
    toEnum 10 = October
    toEnum 11 = November
    toEnum 12 = December
    toEnum _  = error "toEnum @MonthName: out-of-range"

instance Arbitrary MonthName where
    arbitrary = arbitraryBoundedEnum
    shrink January = []
    shrink m       = [January .. pred m]

instance ToJSON MonthName where
    toJSON = toJSON . monthNameToText
    toEncoding = toEncoding . monthNameToText

instance FromJSON MonthName where
    parseJSON = withText "MonthName" $
        either fail return . parseMonthName

instance ToJSONKey MonthName where
    toJSONKey = toJSONKeyText monthNameToText

instance FromJSONKey MonthName where
    fromJSONKey = FromJSONKeyTextParser $
        either fail return . parseMonthName

-------------------------------------------------------------------------------
-- functions
-------------------------------------------------------------------------------

-- | Extract 'Month' from 'Day'
--
-- >>> dayToYearMonthName (read "2017-02-03")
-- (2017,February)
--
dayToYearMonthName :: Day -> (Year, MonthName)
dayToYearMonthName d =
    let (y, m, _) = toGregorian d
    in (y, toEnum m)

-- | First day of the month.
--
-- >>> firstDayOfYearMonthName (2017, February)
-- 2017-02-01
--
firstDayOfYearMonthName :: (Year, MonthName) -> Day
firstDayOfYearMonthName (y, m) = fromGregorian y (fromEnum m) 1

-- | Last day of the month
--
-- >>> lastDayOfYearMonthName (2017, February)
-- 2017-02-28
--
-- >>> lastDayOfYearMonthName (2016, February)
-- 2016-02-29
--
lastDayOfYearMonthName :: (Year, MonthName) -> Day
lastDayOfYearMonthName (y, m) = fromGregorian y m' (gregorianMonthLength y m')
  where
    m' = fromEnum m

parseYearMonthName :: Text -> Either String (Year, MonthName)
parseYearMonthName =  AT.parseOnly $ do
    s <- negate <$ AT.char '-' <|> id <$ AT.char '+' <|> return id
    y <- AT.decimal
    _ <- AT.char '-'
    m <- twoDigits
    if 1 <= m && m <= 12
    then return (s y, toEnum m)
    else fail "Invalid month"
  where
    twoDigits = do
        a <- AT.digit
        b <- AT.digit
        let c2d c = ord c .&. 15
        return $! c2d a * 10 + c2d b

parseMonthName :: Text -> Either String MonthName
parseMonthName "jan" = Right January
parseMonthName "feb" = Right February
parseMonthName "mar" = Right March
parseMonthName "apr" = Right April
parseMonthName "may" = Right May
parseMonthName "jun" = Right June
parseMonthName "jul" = Right July
parseMonthName "aug" = Right August
parseMonthName "sep" = Right September
parseMonthName "oct" = Right October
parseMonthName "nov" = Right November
parseMonthName "dec" = Right December
parseMonthName _ = Left "Invalid MonthName"

#ifdef MIN_VERSION_intervals
-- | Day interval of month
--
-- >>> yearMonthNameInterval (2017, February)
-- 2017-02-01 ... 2017-02-28
yearMonthNameInterval :: (Year, MonthName) -> Interval Day
yearMonthNameInterval m = firstDayOfYearMonthName m ... lastDayOfYearMonthName m
#endif

-------------------------------------------------------------------------------
-- Internals
-------------------------------------------------------------------------------

monthNameToString :: MonthName -> String
monthNameToString January   = "jan"
monthNameToString February  = "feb"
monthNameToString March     = "mar"
monthNameToString April     = "apr"
monthNameToString May       = "may"
monthNameToString June      = "jun"
monthNameToString July      = "jul"
monthNameToString August    = "aug"
monthNameToString September = "sep"
monthNameToString October   = "oct"
monthNameToString November  = "nov"
monthNameToString December  = "dec"

monthNameToText :: MonthName -> Text
monthNameToText = T.pack . monthNameToString

yearMonthNameToString :: (Year, MonthName) -> String
yearMonthNameToString (y, October)  = show y ++ "-10"
yearMonthNameToString (y, November) = show y ++ "-11"
yearMonthNameToString (y, December) = show y ++ "-12"
yearMonthNameToString (y, m)        = show y ++ "-0" ++ show (fromEnum m)

yearMonthNameToText :: (Year, MonthName) -> Text
yearMonthNameToText = T.pack . yearMonthNameToString
