{-# LANGUAGE TypeFamilies #-}

module Servant.Pagination
  (
  -- * Types
    Range(..)
  , RangeOrder(..)
  , AcceptRanges (..)
  , ContentRange (..)
  , PageHeaders
  , TotalCount

  -- * Declare Ranges
  , FromRange(..)
  , FromHttpApiData(..)
  , FromRangeOptions(..)
  , defaultOptions
  , defaultRange

  -- * Use Ranges
  , HasPagination(..)
  , applyRange
  , returnPage
  , returnPage_
  ) where

import           Data.List                   (filter, find)
import           Data.Maybe                  (listToMaybe)
import           Data.Proxy                  (Proxy (..))
import           Data.Semigroup              ((<>))
import           Data.Text                   (Text)
import           GHC.Generics                (Generic)
import           GHC.TypeLits                (KnownSymbol, Symbol, symbolVal)
import           Numeric.Natural             (Natural)
import           Servant

import qualified Data.List                   as List
import qualified Data.Text                   as Text
import qualified Safe

--
-- TYPES
--

-- An actual Range parsed from a `Range` header. A Range
data Range (fields :: [Symbol]) typ =
  forall field. (KnownSymbol field, HasPagination typ field, ToHttpApiData (RangeType typ field), Ord (RangeType typ field)) => Range
  { rangeValue  :: Maybe (RangeType typ field)   -- ^ The value of that field, beginning of the range
  , rangeLimit  :: Int         -- ^ Maximum number of resources to return
  , rangeOffset :: Int         -- ^ Offset, number of resources to skip after the starting value
  , rangeOrder  :: RangeOrder  -- ^ The order of sorting (ascending or descending)
  , rangeField  :: Proxy field
  }

-- | Lift a concrete range to a range over more fields
liftRange :: Range fields typ -> Range (field ': fields) typ
liftRange Range{..} = Range
  { rangeValue  = rangeValue
  , rangeLimit  = rangeLimit
  , rangeOffset = rangeOffset
  , rangeOrder  = rangeOrder
  , rangeField  = rangeField
  }

instance ToHttpApiData (Range fields typ) where
  toUrlPiece Range{..} =
    Text.pack (symbolVal rangeField)
    <> maybe "" (\v -> " " <> toUrlPiece v) rangeValue
    <> ";limit "  <> toUrlPiece rangeLimit
    <> ";offset " <> toUrlPiece rangeOffset
    <> ";order "  <> toUrlPiece rangeOrder

-- | Define the sorting order of the paginated resources (ascending or descending)
data RangeOrder
  = RangeAsc
  | RangeDesc
  deriving (Eq, Show, Ord, Generic)

instance ToHttpApiData RangeOrder where
  toUrlPiece order =
    case order of
      RangeAsc  -> "asc"
      RangeDesc -> "desc"

instance FromHttpApiData RangeOrder where
  parseUrlPiece txt =
    case txt of
      "asc"  -> pure RangeAsc
      "desc" -> pure RangeDesc
      _      -> Left "Invalid Range Order"


-- | Accepted Ranges in the `Accept-Ranges` response's header
data AcceptRanges (fields :: [Symbol]) = AcceptRanges

instance (KnownSymbol field) => ToHttpApiData (AcceptRanges '[field]) where
  toUrlPiece AcceptRanges =
    Text.pack (symbolVal (Proxy :: Proxy field))

instance (ToHttpApiData (AcceptRanges (f ': fs)), KnownSymbol field) => ToHttpApiData (AcceptRanges (field ': f ': fs)) where
  toUrlPiece AcceptRanges =
    Text.pack (symbolVal (Proxy :: Proxy field)) <> "," <> toUrlPiece (AcceptRanges :: AcceptRanges (f ': fs))


-- | Actual range returned, in the `Content-Range` response's header
data ContentRange (fields :: [Symbol]) typ =
  forall field. (KnownSymbol field, ToHttpApiData (RangeType typ field)) => ContentRange
  { contentRangeStart :: RangeType typ field
  , contentRangeEnd   :: RangeType typ field
  , contentRangeField :: Proxy field
  }

instance ToHttpApiData (ContentRange fields res) where
  toUrlPiece (ContentRange start end field) =
    Text.pack (symbolVal field)
    <> " "  <> (toUrlPiece start)
    <> ".." <> (toUrlPiece end)

-- | Type alias to declare response headers related to pagination
type PageHeaders (fields :: [Symbol]) typ =
  '[ Header "Accept-Ranges" (AcceptRanges fields)
   , Header "Content-Range" (ContentRange fields typ)
   , Header "Next-Range"    (Range fields typ)
   , Header "Total-Count"   Natural
   ]

--
-- DECLARE RANGES
--

-- | Default values to apply when parsing a Range
data FromRangeOptions  = FromRangeOptions
  { defaultRangeLimit  :: Int
  , defaultRangeOffset :: Int
  , defaultRangeOrder  :: RangeOrder
  } deriving (Eq, Show)


-- | Some default options of default values for a Range (limit 100; offset 0; order desc)
defaultOptions :: FromRangeOptions
defaultOptions =
  FromRangeOptions 100 0 RangeDesc


-- | Some default range based on the default options
defaultRange :: forall field fields typ. (HasPagination typ field, ToHttpApiData (RangeType typ field), Ord (RangeType typ field)) =>
                Maybe (RangeType typ field) -> Range (field ': fields) typ
defaultRange val =
  let
    FromRangeOptions lim off ord = rangeOptions (Proxy :: Proxy typ) (Proxy :: Proxy field)
  in
    Range val lim off ord (Proxy :: Proxy field)

-- | Parse a Range object from a `Range` request's header. Any `Range field typ` and combinations
-- of any `Range field typ` provide instance of this class. It is a signature similar to
-- `parseUrlPiece` from the `FromHttpApiData` class and can be used as a drop-in replacement to
-- define instance of this class.
--
-- > type MyRange = Range "created_at" UTCTime
-- >
-- > instance FromHttpApiData UTCTime => FromHttpApiData MyRange where
-- >     parseUrlPiece =
-- >         fromRange defaultOptions
class FromRange a where
  parseRange :: FromRangeOptions -> Text -> Either Text a

instance FromHttpApiData (Range '[] typ) where
  parseUrlPiece _ = Left "Invalid Range"
  
instance (FromHttpApiData (Range fields typ),
          FromHttpApiData (RangeType typ field),
          ToHttpApiData (RangeType typ field),
          Ord (RangeType typ field),
          KnownSymbol field,
          HasPagination typ field) =>
         FromHttpApiData (Range (field ': fields) typ) where
  parseUrlPiece txt =
    let
      rOpts = rangeOptions (Proxy :: Proxy typ) (Proxy :: Proxy field)
      
      toTuples =
        filter (/= "") . Text.splitOn (Text.singleton ' ')

      args =
        map toTuples $ Text.splitOn (Text.singleton ';') txt

      field =
        Text.pack $ symbolVal (Proxy :: Proxy field)
    in
      case args of
        (field' : value) : rest | field == field' -> do
          opts <-
            traverse parseOpt rest

          Range
            <$> sequence (fmap parseQueryParam (listToMaybe value))
            <*> ifOpt "limit"  (defaultRangeLimit  rOpts) opts
            <*> ifOpt "offset" (defaultRangeOffset rOpts) opts
            <*> ifOpt "order"  (defaultRangeOrder  rOpts) opts
            <*> pure (Proxy :: Proxy field)

        _ -> -- recurse to other fields
          liftRange <$> parseUrlPiece txt
    where
      parseOpt :: [Text] -> Either Text (Text, Text)
      parseOpt piece =
        case piece of
          [opt, arg] ->
            pure (opt, arg)

          _ ->
            Left "Invalid Range Options"

      ifOpt :: FromHttpApiData o => Text -> o -> [(Text, Text)] -> Either Text o
      ifOpt opt def =
        maybe (pure def) (parseQueryParam . snd) . find ((== opt) . fst)

type TotalCount =
  Maybe Natural

--
-- USE RANGES
--

-- | In addition to the `FromHttpApiData` instance, one can provide an instance for this
-- type-class to easily lift a list of response to a Servant handler.
-- By providing a getter to retrieve the value of an actual range from a resource, the
-- `HasPagination` class provides `returnPage` to handle the plumbering of declaring
-- response headers related to pagination.
class KnownSymbol field => HasPagination typ field where
  type RangeType typ field :: *

  getRangeField :: Proxy field -> typ -> RangeType typ field

  rangeOptions :: Proxy typ -> Proxy field -> FromRangeOptions
  rangeOptions _ _ = defaultOptions

-- | Lift a list of result into a Monad (e.g. @Servant.Handler@) obtained from the given range
returnPage_ :: (Monad m, ToHttpApiData (AcceptRanges fields)) =>
              (Range fields typ) -> [typ] -> m (Headers (PageHeaders fields typ) [typ])
returnPage_ = returnPage Nothing

-- | Lift a list of result into a Monad (e.g. @Servant.Handler@) obtained from the given range, including a total count
returnPage :: (Monad m, ToHttpApiData (AcceptRanges fields)) =>
              TotalCount -> (Range fields typ) -> [typ] -> m (Headers (PageHeaders fields typ) [typ])
returnPage count Range{..} rs = do
  let boundaries = (,)
        <$> fmap (getRangeField rangeField) (Safe.headMay rs)
        <*> fmap (getRangeField rangeField) (Safe.lastMay rs)

  let totalCount =
        maybe noHeader addHeader count

  case boundaries of
    Nothing ->
      return $ addHeader AcceptRanges $ noHeader $ noHeader $ totalCount rs

    Just (start, end) -> do
      let nextOffset | rangeValue `compare` Just end == EQ = rangeOffset + length rs
                     | otherwise = length $ takeWhile (\r -> getRangeField rangeField r `compare` end == EQ) $ reverse rs

      let nextRange = Range
            { rangeValue  = Just end
            , rangeLimit  = rangeLimit
            , rangeOffset = nextOffset
            , rangeOrder  = rangeOrder
            , rangeField  = rangeField
            }

      let contentRange = 
            ContentRange
            { contentRangeStart = start
            , contentRangeEnd   = end
            , contentRangeField = rangeField
            }

      return $ addHeader AcceptRanges $ addHeader contentRange $ addHeader nextRange $ totalCount rs

applyRange :: forall ranges res. Range ranges res -> [res] -> [res]
applyRange Range{..} =
  let
    sortRel =
      case rangeOrder of
        RangeDesc ->
          \a b -> compare (getRangeField rangeField b) (getRangeField rangeField a)

        RangeAsc ->
          \a b -> compare (getRangeField rangeField a) (getRangeField rangeField b)

    dropRel =
      case (rangeValue, rangeOrder) of
        (Nothing, _) ->
          const False

        (Just a, RangeDesc) ->
          (> a) . (getRangeField rangeField)

        (Just a, RangeAsc) ->
          (< a) . (getRangeField rangeField)
  in
      List.take rangeLimit
    . List.drop rangeOffset
    . List.dropWhile dropRel
    . List.sortBy sortRel
