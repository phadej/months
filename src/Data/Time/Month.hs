{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE Trustworthy        #-}
-- | 'Month' data type.
module Data.Time.Month (
    -- * Types
    Month (..),
    YearMonth (..),
    -- * Conversion with Day
    dayToYearMonth,
    firstDayOfYearMonth,
    lastDayOfYearMonth,
#ifdef MIN_VERSION_intervals
    yearMonthInterval,
#endif
    -- * Conversions with Text
    yearMonthToText,
    parseYearMonth,
    ) where

import Control.Applicative ((<|>))
import Control.DeepSeq     (NFData (..))
import Data.Bits           ((.&.))
import Data.Char           (ord)
import Data.Hashable       (Hashable)
import Data.String         (fromString)
import Data.Text           (Text)
import Data.Time.Compat
       (Day, fromGregorian, gregorianMonthLength, toGregorian)
import Data.Typeable       (Typeable)
import GHC.Generics        (Generic)
import Prelude ()
import Prelude.Compat
import Test.QuickCheck     (Arbitrary (..), arbitraryBoundedEnum)

import qualified Data.Attoparsec.Text     as AT
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as TE
import qualified Data.Text.Encoding.Error as TE

#ifdef MIN_VERSION_aeson
import Data.Aeson
       (FromJSON (..), FromJSONKey (..), ToJSON (..), ToJSONKey (..), withText)
import Data.Aeson.Types (FromJSONKeyFunction (..), ToJSONKeyFunction (..))

import qualified Data.Aeson.Encoding as Aeson.Encoding
#endif

#ifdef MIN_VERSION_cassava
import qualified Data.Csv as Csv
#endif

#ifdef MIN_VERSION_http_api_data
import Web.HttpApiData (FromHttpApiData (..), ToHttpApiData (..))
#endif

#ifdef MIN_VERSION_intervals
import Numeric.Interval.NonEmpty (Interval, (...))
#endif

#ifdef MIN_VERSION_lucid
import Lucid (ToHtml (..))
#endif

#ifdef MIN_VERSION_swagger2
import Control.Lens ((&), (.~), (?~))
import Data.Swagger (ToParamSchema (..), ToSchema (..))

import qualified Data.Swagger as Swagger
#endif

#if defined(MIN_VERSION_cassava) || defined(MIN_VERSION_http_api_data)
import Data.Bifunctor (first)
#endif

-------------------------------------------------------------------------------
-- Month
-------------------------------------------------------------------------------

-- | We explicitly enumerate month names. Using an 'Int' is unsafe.
data Month
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

instance Hashable Month
instance NFData Month

instance Enum Month where
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
    toEnum _  = error "toEnum @Month: out-of-range"

instance Arbitrary Month where
    arbitrary = arbitraryBoundedEnum
    shrink January = []
    shrink m       = [January .. pred m]

-------------------------------------------------------------------------------
-- Month
-------------------------------------------------------------------------------

-- | A month in Julian/Gregorian calendar.
data YearMonth = YearMonth { monthYear :: !Integer, monthName :: !Month }
  deriving (Eq, Ord, Generic, Typeable)

-- | Doesn't print field names.
instance Show YearMonth where
    showsPrec d (YearMonth y n) = showParen (d > 10)
        $ showString "YearMonth "
        . showsPrec 11 y
        . showChar ' '
        . showsPrec 11 n

-- TODO write Read instance to match above Show instance

instance Hashable YearMonth

instance NFData YearMonth where rnf (YearMonth _ _) = ()

instance Enum YearMonth where
    succ (YearMonth y December) = YearMonth (y + 1) January
    succ (YearMonth y m)        = YearMonth y (succ m)

    pred (YearMonth y January) = YearMonth (y - 1) December
    pred (YearMonth y m)       = YearMonth y (pred m)

    fromEnum (YearMonth y m) = fromIntegral y * 12 + fromEnum m - 1
    toEnum i =
        let (y, m) = divMod i 12
        in YearMonth (fromIntegral y) (toEnum $ m + 1)

#ifdef MIN_VERSION_cassava
instance Csv.ToField YearMonth where
    toField = Csv.toField . yearMonthToString

instance Csv.FromField YearMonth where
    parseField field =
        let monthtext = TE.decodeUtf8With TE.lenientDecode field
            month = first T.pack (parseYearMonth monthtext)
        in case month of
                Left err -> fail $ T.unpack err
                Right m -> pure m
#endif

#ifdef MIN_VERSION_aeson
-- | TODO: use builder if we really want speed
instance ToJSON YearMonth where
    toJSON = fromString . yearMonthToString
    toEncoding = Aeson.Encoding.string . yearMonthToString

instance FromJSON YearMonth where
    parseJSON = withText "YearMonth" $
        either fail pure . parseYearMonth

instance ToJSONKey YearMonth where
    toJSONKey = ToJSONKeyText
        (fromString . yearMonthToString)
        (Aeson.Encoding.string . yearMonthToString)

instance FromJSONKey YearMonth where
    fromJSONKey = FromJSONKeyTextParser $
        either fail pure . parseYearMonth
#endif

#ifdef MIN_VERSION_swagger2
instance ToSchema YearMonth where
    declareNamedSchema _ = pure $ Swagger.NamedSchema (Just "YearMonth") $ mempty
        & Swagger.type_ ?~ Swagger.SwaggerString
        & Swagger.format ?~ "month"

-- | Format @"month"@ corresponds to @yyyy-mm@ format.
instance ToParamSchema YearMonth where
  toParamSchema _ = mempty
      & Swagger.type_  ?~ Swagger.SwaggerString
      & Swagger.format ?~ "month"
#endif

#ifdef MIN_VERSION_http_api_data
instance ToHttpApiData YearMonth where
    toUrlPiece = fromString . yearMonthToString

instance FromHttpApiData YearMonth where
    parseUrlPiece = first T.pack . parseYearMonth
#endif

#ifdef MIN_VERSION_lucid
instance ToHtml YearMonth where
    toHtmlRaw = toHtml
    toHtml = toHtml . yearMonthToText
#endif

instance Arbitrary YearMonth where
    arbitrary = mk <$> arbitrary <*> arbitrary
      where
        mk y m = YearMonth (y + 2019) m

    shrink (YearMonth y m) =
        [ YearMonth (y' + 2019) m | y' <- shrink (y - 2019) ] ++
        [ YearMonth y m' | m' <- shrink m ]

-------------------------------------------------------------------------------
-- functions
-------------------------------------------------------------------------------

-- | Extract 'Month' from 'Day'
--
-- >>> dayToYearMonth (read "2017-02-03")
-- YearMonth 2017 February
--
dayToYearMonth :: Day -> YearMonth
dayToYearMonth d =
    let (y, m, _) = toGregorian d
    in mkYearMonth (y, m)

-- | First day of the month.
--
-- >>> firstDayOfYearMonth $ YearMonth 2017 February
-- 2017-02-01
--
firstDayOfYearMonth :: YearMonth -> Day
firstDayOfYearMonth (YearMonth y m) = fromGregorian y (fromEnum m) 1

-- | Last day of the month
--
-- >>> lastDayOfYearMonth $ YearMonth 2017 February
-- 2017-02-28
--
-- >>> lastDayOfYearMonth $ YearMonth 2016 February
-- 2016-02-29
--
lastDayOfYearMonth :: YearMonth -> Day
lastDayOfYearMonth (YearMonth y m) = fromGregorian y m' (gregorianMonthLength y m')
  where
    m' = fromEnum m

parseYearMonth :: Text -> Either String YearMonth
parseYearMonth =  AT.parseOnly $ do
    s <- negate <$ AT.char '-' <|> id <$ AT.char '+' <|> return id
    y <- AT.decimal
    _ <- AT.char '-'
    m <- twoDigits
    if 1 <= m && m <= 12
    then return $ YearMonth y (toEnum m)
    else fail "Invalid month"
  where
    twoDigits = do
        a <- AT.digit
        b <- AT.digit
        let c2d c = ord c .&. 15
        return $! c2d a * 10 + c2d b

#ifdef MIN_VERSION_intervals
-- | Day interval of month
--
-- >>> yearMonthInterval $ YearMonth 2017 February
-- 2017-02-01 ... 2017-02-28
yearMonthInterval :: YearMonth -> Interval Day
yearMonthInterval m = firstDayOfYearMonth m ... lastDayOfYearMonth m
#endif

-------------------------------------------------------------------------------
-- Internals
-------------------------------------------------------------------------------

mkYearMonth :: (Integer, Int) -> YearMonth
mkYearMonth (y, m) = YearMonth y (toEnum m)

yearMonthToString :: YearMonth -> String
yearMonthToString (YearMonth y October)  = show y ++ "-10"
yearMonthToString (YearMonth y November) = show y ++ "-11"
yearMonthToString (YearMonth y December) = show y ++ "-12"
yearMonthToString (YearMonth y m)        = show y ++ "-0" ++ show (fromEnum m)

yearMonthToText :: YearMonth -> Text
yearMonthToText = T.pack . yearMonthToString
