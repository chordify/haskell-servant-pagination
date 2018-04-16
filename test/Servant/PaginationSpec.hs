{-# OPTIONS_GHC -fno-warn-orphans #-}

module Servant.PaginationSpec
  ( spec
  ) where


import           Data.Either               (isLeft)
import           Data.Proxy                (Proxy (..))
import           Data.Text                 (Text)
import           Servant                   (FromHttpApiData (..), ToHttpApiData (..))
import           Test.Hspec                (Spec, describe, it, shouldBe)
import           Test.QuickCheck           (Arbitrary (..), property, withMaxSuccess)
import           Test.QuickCheck.Gen       (Gen, choose, oneof, scale, sized, vectorOf)
import           Test.QuickCheck.Modifiers (Positive (..))

import           Servant.Pagination


spec :: Spec
spec = do
  describe "round-up properties" $ do
    it "parseUrlPiece . toUrlPiece = pure" $ withMaxSuccess 10000 $ property $
      \x -> (fmap extractA . parseUrlPiece . toUrlPiece) x == (pure . extractA) x

    it "parseUrlPiece . toUrlPiece = pure" $ withMaxSuccess 10000 $ property $
      \x -> (fmap extractB . parseUrlPiece . toUrlPiece) x == (pure . extractB) x

  describe "try-out ranges" $ do
    let r0 = getDefaultRange (Proxy @Resource)  :: Range "fieldA" Int

    it "Range: fieldA" $
      let
        Right r = parseUrlPiece "fieldA"
        r' = r0
      in
        extractA r `shouldBe` pure r'

    it "Range: fieldA 14; limit 42" $
      let
        Right r = parseUrlPiece "fieldA 14; limit 42"
        r' =  r0 { rangeValue = Just 14, rangeLimit = 42 }
      in
        extractA r `shouldBe` pure r'

    it "Range: fieldA; order asc; offset 2" $
      let
        Right r = parseUrlPiece "fieldA; order asc; offset 42"
        r' =  r0 { rangeOffset = 42, rangeOrder = RangeAsc }
      in
        extractA r `shouldBe` pure r'

    it "Range: fieldA xxx" $
      isLeft (parseUrlPiece "fieldA xxx" :: Either Text (Ranges '["fieldA", "fieldB"] Resource))

    it "Range: fieldC" $
      isLeft (parseUrlPiece "fieldC" :: Either Text (Ranges '["fieldA", "fieldB"] Resource))

    it "Range: fieldB" $
      isLeft (parseUrlPiece "fieldB" :: Either Text (Ranges '["fieldA"] Resource))
 where
  extractA :: Ranges '["fieldA", "fieldB"] Resource -> Maybe (Range "fieldA" Int)
  extractA = extractRange

  extractB :: Ranges '["fieldA", "fieldB"] Resource -> Maybe (Range "fieldB" SimpleString)
  extractB = extractRange


data Resource = Resource
  { fieldA :: Int
  , fieldB :: SimpleString
  } deriving (Show, Eq)

newtype SimpleString = SimpleString
  { getSimpleString :: String
  } deriving (Show, Eq, Ord)

instance Arbitrary SimpleString where
  arbitrary =
    SimpleString <$> scale (+1) (sized $ flip vectorOf $ choose ('a', 'z'))

instance FromHttpApiData SimpleString where
  parseUrlPiece =
    fmap SimpleString . parseUrlPiece

instance ToHttpApiData SimpleString where
  toUrlPiece =
    toUrlPiece . getSimpleString

instance HasPagination Resource "fieldA" where
  type RangeType Resource "fieldA" = Int
  getFieldValue _ = fieldA

instance HasPagination Resource "fieldB" where
  type RangeType Resource "fieldB" = SimpleString
  getFieldValue _ = fieldB

instance Arbitrary (Ranges '["fieldA", "fieldB"] Resource) where
  arbitrary = oneof
    [ putRange <$> (arbitrary :: Gen (Range "fieldA" Int))
    , putRange <$> (arbitrary :: Gen (Range "fieldB" SimpleString))
    ]

instance (IsRangeType a, Arbitrary a) => Arbitrary (Range "fieldA" a) where
  arbitrary = Range
    <$> arbitrary
    <*> fmap getPositive arbitrary
    <*> fmap getPositive arbitrary
    <*> oneof [pure RangeAsc, pure RangeDesc]
    <*> pure Proxy

instance (IsRangeType a, Arbitrary a) => Arbitrary (Range "fieldB" a) where
  arbitrary = Range
    <$> arbitrary
    <*> fmap getPositive arbitrary
    <*> fmap getPositive arbitrary
    <*> oneof [pure RangeAsc, pure RangeDesc]
    <*> pure Proxy
