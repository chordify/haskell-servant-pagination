{-# LANGUAGE TypeFamilies #-}

module Servant.Pagination
  (
  -- * Types
    Range(..)
  , RangeOrder(..)
  , AcceptRanges (..)
  , ContentRange (..)
  , NextRange (..)
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

  -- * Combine Ranges
  , (:|:)(..)
  ) where

import           Data.List                   (filter, find)
import           Data.Maybe                  (fromMaybe, listToMaybe)
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

import           Servant.Pagination.Internal


--
-- TYPES
--

-- An actual Range parsed from a `Range` header. A Range
data Range (field :: Symbol) typ = Range
  { rangeValue  :: Maybe typ   -- ^ The value of that field, beginning of the range
  , rangeLimit  :: Int         -- ^ Maximum number of resources to return
  , rangeOffset :: Int         -- ^ Offset, number of resources to skip after the starting value
  , rangeOrder  :: RangeOrder  -- ^ The order of sorting (ascending or descending)
  } deriving (Eq, Show, Generic)

instance Functor (Range field) where
  fmap f r =
    r { rangeValue = f <$> rangeValue r }

instance (ToHttpApiData typ, KnownSymbol field) => ToHttpApiData (Range field typ) where
  toUrlPiece Range{..} =
    Text.pack (symbolVal (Proxy :: Proxy field))
    <> maybe "" (\v -> " " <> toUrlPiece v) rangeValue
    <> ";limit "  <> toUrlPiece rangeLimit
    <> ";offset " <> toUrlPiece rangeOffset
    <> ";order "  <> toUrlPiece rangeOrder

instance (KnownSymbol field) => ToAcceptRanges (Range field typ) where
  toAcceptRanges _ =
    Text.pack (symbolVal (Proxy :: Proxy field))


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
data AcceptRanges range = AcceptRanges

instance (ToAcceptRanges a) => ToHttpApiData (AcceptRanges a) where
  toUrlPiece _ =
    toAcceptRanges (Proxy :: Proxy a)


-- | Actual range returned, in the `Content-Range` response's header
data ContentRange range = ContentRange
  { contentRangeStart :: range
  , contentRangeEnd   :: range
  }

instance (ToHttpApiData typ, KnownSymbol field) => ToHttpApiData (ContentRange (Range field typ)) where
  toUrlPiece (ContentRange start end) =
    Text.pack (symbolVal (Proxy :: Proxy field))
    <> " "  <> (fromMaybe "" (toUrlPiece <$> rangeValue start))
    <> ".." <> (fromMaybe "" (toUrlPiece <$> rangeValue end))

instance (ToHttpApiData (ContentRange a), ToHttpApiData (ContentRange b)) => ToHttpApiData (ContentRange (a :|: b)) where
  toUrlPiece (ContentRange (InL sa) (InL ea)) =
    toUrlPiece (ContentRange sa ea)

  toUrlPiece (ContentRange (InR sb) (InR eb)) =
    toUrlPiece (ContentRange sb eb)

  toUrlPiece _ =
    error "impossible"


-- | Range to provide to retrieve the next batch of resource, in the `Next-Range` response's header
data NextRange range = NextRange range

instance (ToHttpApiData typ, KnownSymbol field) => ToHttpApiData (NextRange (Range field typ)) where
  toUrlPiece (NextRange r) =
    toUrlPiece r

instance (ToHttpApiData (NextRange a), ToHttpApiData (NextRange b)) => ToHttpApiData (NextRange (a :|: b)) where
  toUrlPiece (NextRange (InL a)) =
    toUrlPiece (NextRange a)

  toUrlPiece (NextRange (InR b)) =
    toUrlPiece (NextRange b)


-- | Type alias to declare response headers related to pagination
type PageHeaders range =
  '[ Header "Accept-Ranges" (AcceptRanges range)
   , Header "Content-Range" (ContentRange range)
   , Header "Next-Range"    (NextRange range)
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
defaultRange :: Maybe a -> FromRangeOptions -> Range field a
defaultRange val opts =
  let
    (FromRangeOptions lim off ord) =
      opts
  in
    Range val lim off ord


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

instance (FromHttpApiData typ, KnownSymbol field) => FromRange (Range field typ) where
  parseRange FromRangeOptions{..} txt =
    let
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
            <*> ifOpt "limit" defaultRangeLimit opts
            <*> ifOpt "offset"  defaultRangeOffset opts
            <*> ifOpt "order" defaultRangeOrder opts

        _ ->
          Left "Invalid Range"
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

instance {-# Overlappable #-} (FromHttpApiData typ, KnownSymbol field) => FromHttpApiData (Range field typ) where
  parseUrlPiece =
    parseRange defaultOptions


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
class KnownSymbol field => HasPagination resource field where
  type RangeType resource field :: *

  getRangeField :: Proxy field -> resource -> RangeType resource field

  returnPage_ :: forall m ranges.
    ( Monad m
    , (Range field (RangeType resource field)) :<: ranges
    , ToAcceptRanges ranges
    , ToHttpApiData (ContentRange ranges)
    , ToHttpApiData (NextRange ranges)
    , Ord (RangeType resource field)
    ) => (Range field (RangeType resource field)) -> [resource] -> m (Headers (PageHeaders ranges) [resource])
  returnPage_ =
    returnPage Nothing
  {-# INLINE returnPage_ #-}

  returnPage :: forall m ranges.
    ( Monad m
    , (Range field (RangeType resource field)) :<: ranges
    , ToAcceptRanges ranges
    , ToHttpApiData (ContentRange ranges)
    , ToHttpApiData (NextRange ranges)
    , Ord (RangeType resource field)
    ) => TotalCount -> (Range field (RangeType resource field)) -> [resource] -> m (Headers (PageHeaders ranges) [resource])
  returnPage count range rs = do
    let field =
          Proxy :: Proxy field

    let boundaries = (,)
          <$> fmap (getRangeField field) (Safe.headMay rs)
          <*> fmap (getRangeField field) (Safe.lastMay rs)

    let acceptRanges =
          addHeader (AcceptRanges :: AcceptRanges ranges)

    let totalCount =
          maybe noHeader addHeader count

    case boundaries of
      Nothing ->
        return $
          acceptRanges $ noHeader $ noHeader $ totalCount rs

      Just (start, end) -> do
        let rangeStart =
              liftRange $ (range { rangeValue = Just start } :: Range field (RangeType resource field))

        let rangeEnd =
              liftRange $ (range { rangeValue = Just end } :: Range field (RangeType resource field))

        let nextOffset | rangeValue range `compare` Just end == EQ = rangeOffset range + length rs
                       | otherwise = length $ takeWhile (\r -> getRangeField field r `compare` end == EQ) $ reverse rs
        
        let rangeNext =
              liftRange $ (range { rangeValue = Just end, rangeOffset = nextOffset } :: Range field (RangeType resource field))

        let contentRange =
              addHeader $ ContentRange
                { contentRangeStart = rangeStart
                , contentRangeEnd   = rangeEnd
                }

        let nextRange =
              addHeader $ NextRange $ rangeNext

        return
          $ acceptRanges $ contentRange $ nextRange $ totalCount rs


-- | Apply a range to a list of element
applyRange :: forall b field. (HasPagination b field, Ord (RangeType b field)) => Range field (RangeType b field) -> [b] -> [b]
applyRange Range{..} =
  let
    field =
      Proxy :: Proxy (field :: Symbol)

    sortRel =
      case rangeOrder of
        RangeDesc ->
          \a b -> compare (getRangeField field b) (getRangeField field a)

        RangeAsc ->
          \a b -> compare (getRangeField field a) (getRangeField field b)

    dropRel =
      case (rangeValue, rangeOrder) of
        (Nothing, _) ->
          const False

        (Just a, RangeDesc) ->
          (> a) . (getRangeField field)

        (Just a, RangeAsc) ->
          (< a) . (getRangeField field)
  in
      List.take rangeLimit
    . List.drop rangeOffset
    . List.dropWhile dropRel
    . List.sortBy sortRel
