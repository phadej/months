{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE Trustworthy        #-}
-- | 'Quarter' data type.
module Data.Time.Quarter (
    -- * Types
    Quarter (..),
    YearQuarter (..),
    -- * Functions
    dayToYearQuarter,
    firstDayOfYearQuarter,
    lastDayOfYearQuarter,
    yearQuarterToText,
    ) where

import Control.Applicative ((<|>))
import Control.DeepSeq     (NFData (..))
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
import Data.Bifunctor  (first)
import Web.HttpApiData (FromHttpApiData (..), ToHttpApiData (..))
#endif

#ifdef MIN_VERSION_lucid
import Lucid (ToHtml (..))
#endif

#ifdef MIN_VERSION_swagger2
import Control.Lens ((&), (.~), (?~))
import Data.Swagger (ToParamSchema (..), ToSchema (..))

import qualified Data.Swagger as Swagger
#endif

-------------------------------------------------------------------------------
-- Quarter
-------------------------------------------------------------------------------

-- | We explicitly enumerate quarter names. Using an 'Int' is unsafe.
data Quarter
    = Q1
    | Q2
    | Q3
    | Q4
  deriving (Eq, Ord, Show, Read, Generic, Typeable, Bounded)

instance Hashable Quarter
instance NFData Quarter

instance Enum Quarter where
    fromEnum Q1 = 1
    fromEnum Q2 = 2
    fromEnum Q3 = 3
    fromEnum Q4 = 4

    toEnum 1  = Q1
    toEnum 2  = Q2
    toEnum 3  = Q3
    toEnum 4  = Q4
    toEnum _  = error "toEnum @Quarter: out-of-range"

instance Arbitrary Quarter where
    arbitrary = arbitraryBoundedEnum
    shrink Q1 = []
    shrink m  = [Q1 .. pred m]

-------------------------------------------------------------------------------
-- Quarter
-------------------------------------------------------------------------------

-- | A quarter in Julian/Gregorian calendar.
data YearQuarter = YearQuarter { quarterYear :: !Integer, quarterName :: !Quarter }
  deriving (Eq, Ord, Generic, Typeable)

-- | Doesn't print field names.
instance Show YearQuarter where
    showsPrec d (YearQuarter y n) = showParen (d > 10)
        $ showString "YearQuarter "
        . showsPrec 11 y
        . showChar ' '
        . showsPrec 11 n

-- TODO write Read instance to match above Show instance

instance Hashable YearQuarter

instance NFData YearQuarter where rnf (YearQuarter _ _) = ()

instance Enum YearQuarter where
    succ (YearQuarter y Q4) = YearQuarter (y + 1) Q1
    succ (YearQuarter y m)  = YearQuarter y (succ m)

    pred (YearQuarter y Q1) = YearQuarter (y - 1) Q4
    pred (YearQuarter y m)  = YearQuarter y (pred m)

    fromEnum (YearQuarter y m) = fromIntegral y * 4 + fromEnum m - 1
    toEnum i =
        let (y, m) = divMod i 4
        in YearQuarter (fromIntegral y) (toEnum $ m + 1)

#ifdef MIN_VERSION_cassava
instance Csv.ToField YearQuarter where
    toField = Csv.toField . yearQuarterToString

instance Csv.FromField YearQuarter where
    parseField field =
        let quartertext = TE.decodeUtf8With TE.lenientDecode field
            quarter = first T.pack (parseYearQuarter quartertext)
        in case quarter of
                Left err -> fail $ T.unpack err
                Right m -> pure m
#endif

#ifdef MIN_VERSION_aeson
-- | TODO: use builder if we really want speed
instance ToJSON YearQuarter where
    toJSON = fromString . yearQuarterToString
    toEncoding = Aeson.Encoding.string . yearQuarterToString

instance FromJSON YearQuarter where
    parseJSON = withText "YearQuarter" $
        either fail pure . parseYearQuarter

instance ToJSONKey YearQuarter where
    toJSONKey = ToJSONKeyText
        (fromString . yearQuarterToString)
        (Aeson.Encoding.string . yearQuarterToString)

instance FromJSONKey YearQuarter where
    fromJSONKey = FromJSONKeyTextParser $
        either fail pure . parseYearQuarter
#endif

#ifdef MIN_VERSION_swagger2
instance ToSchema YearQuarter where
    declareNamedSchema _ = pure $ Swagger.NamedSchema (Just "YearQuarter") $ mempty
        & Swagger.type_ ?~ Swagger.SwaggerString
        & Swagger.format ?~ "quarter"

-- | Format @"quarter"@ corresponds to @yyyy-mm@ format.
instance ToParamSchema YearQuarter where
  toParamSchema _ = mempty
      & Swagger.type_  ?~ Swagger.SwaggerString
      & Swagger.format ?~ "quarter"
#endif

#ifdef MIN_VERSION_http_api_data
instance ToHttpApiData YearQuarter where
    toUrlPiece = fromString . yearQuarterToString

instance FromHttpApiData YearQuarter where
    parseUrlPiece = first T.pack . parseYearQuarter
#endif

#ifdef MIN_VERSION_lucid
instance ToHtml YearQuarter where
    toHtmlRaw = toHtml
    toHtml = toHtml . yearQuarterToText
#endif

instance Arbitrary YearQuarter where
    arbitrary = mk <$> arbitrary <*> arbitrary
      where
        mk y m = YearQuarter (y + 2019) m

    shrink (YearQuarter y m) =
        [ YearQuarter (y' + 2019) m | y' <- shrink (y - 2019) ] ++
        [ YearQuarter y m' | m' <- shrink m ]

-------------------------------------------------------------------------------
-- functions
-------------------------------------------------------------------------------

-- | Extract 'Quarter' from 'Day'
--
-- >>> dayToYearQuarter (read "2017-02-03")
-- YearQuarter 2017 Q1
--
dayToYearQuarter :: Day -> YearQuarter
dayToYearQuarter d =
    let (y, m, _) = toGregorian d
    in mkYearQuarter (y, succ (pred m `div` 3))

-- | First day of the quarter.
--
-- >>> firstDayOfYearQuarter $ YearQuarter 2017 Q3
-- 2017-07-01
--
firstDayOfYearQuarter :: YearQuarter -> Day
firstDayOfYearQuarter (YearQuarter y m) = fromGregorian y m' 1
  where
    m' = 3 * fromEnum m - 2

-- | Last day of the quarter
--
-- >>> lastDayOfYearQuarter $ YearQuarter 2017 Q1
-- 2017-03-31
--
-- >>> lastDayOfYearQuarter $ YearQuarter 2016 Q2 
-- 2016-06-30
--
lastDayOfYearQuarter :: YearQuarter -> Day
lastDayOfYearQuarter (YearQuarter y m) = fromGregorian y m' (gregorianMonthLength y m')
  where
    m' = 3 * fromEnum m

parseYearQuarter :: Text -> Either String YearQuarter
parseYearQuarter =  AT.parseOnly $ do
      s <- negate <$ AT.char '-' <|> id <$ AT.char '+' <|> return id
      y <- AT.decimal
      _ <- AT.char '-'
      _ <- AT.char 'Q'
      q <- Q1 <$ AT.char '1'
          <|> Q2 <$ AT.char '2'
          <|> Q3 <$ AT.char '3'
          <|> Q4 <$ AT.char '4'
      return (YearQuarter y q)

-------------------------------------------------------------------------------
-- Internals
-------------------------------------------------------------------------------

mkYearQuarter :: (Integer, Int) -> YearQuarter
mkYearQuarter (y, m) = YearQuarter y (toEnum m)

yearQuarterToString :: YearQuarter -> String
yearQuarterToString (YearQuarter y Q1)  = show y ++ "-Q1"
yearQuarterToString (YearQuarter y Q2)  = show y ++ "-Q2"
yearQuarterToString (YearQuarter y Q3)  = show y ++ "-Q3"
yearQuarterToString (YearQuarter y Q4)  = show y ++ "-Q4"

yearQuarterToText :: YearQuarter -> Text
yearQuarterToText = T.pack . yearQuarterToString
