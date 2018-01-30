{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies           #-}

module Servant.Pagination
  (
  -- * Overview
  --
  -- This module offers opinionated helpers to declare a type-safe and a flexible pagination
  -- mecanism for Servant APIs. This design, inspired by [Heroku's API](https://devcenter.heroku.com/articles/platform-api-reference#ranges),
  -- provides a small framework to communicate about a possible pagination feature of an endpoint,
  -- enabling a client to consume the API in different fashions (pagination with offset / limit,
  -- endless scroll using last referenced resources, ascending and descending ordering, etc.)
  --
  -- Therefore, client may provide a `Range` header with their request with the following format:
  --
  -- - @Range: <field> [<value>][; offset <o>][; limit <l>][; order <asc|desc>]
  --
  -- For example: @Range: createdAt 2017-01-15T23:14:67.000Z; offset 5; order desc@ indicates that
  -- the client is willing to retrieve the next batch of document in descending order that were
  -- created after the fifteenth of January, skipping the first 5.
  --
  -- As a response, the server may return the list of corresponding document, and augment the
  -- response with 3 or 4 headers:
  --
  -- - @Accept-Ranges@: A comma-separated list of field upon which a range can be defined
  -- - @Content-Range@: Actual range corresponding to the content being returned
  -- - @Next-Range@: Indicate what should be the next @Range@ header in order to retrieve the next range
  -- - @Total-Count@: Optional, but if present, refers to the total number of resources in the collection
  --
  -- For example:
  --
  -- - @Accept-Ranges: createdAt, modifiedAt@
  -- - @Content-Range@: createdAt 2017-01-15T23:14:51.000Z..2017-02-18T06:10:23.000Z
  -- - @Next-Range@: createdAt 2017-02-19T12:56:28.000Z; offset 0; limit 100; order desc
  -- - @Total-Count@: 1442
  --
  -- * Getting Starting
  --
  -- Code-wise, the integration is rather seamless and requires to declare a `Range` type on
  -- on a given field and to provide an instance of `HasPagination` and `FromHttpApiData` for
  -- this type. The `getRangeField` method from `HasPagination` is merely a getter to retrieve
  -- a range's field value from a resource. Note that `Range` (and all combinations of `Range`)
  -- provides an instance of a `FromRange` class that can be leveraged to easily define instances
  -- for the `FromHttpApiData` class. For example:
  --
  -- > data MyResource = MyResource
  -- >   { anything  :: String
  -- >   , createdAt :: Int
  -- >   }
  -- >
  -- > type MyRange = Range "createdAt" UTCTime
  -- > type MyHeaders = PageHeaders MyRange
  -- >
  -- > instance FromHttpApiData MyRange where
  -- >   parseUrlPiece = parseRange defaultOptions
  -- >
  -- > instance HasPagination MyResource "createdAt" UTCTime where
  -- >   getRangeField _ = createdAt
  --
  -- That's it, the range is ready to use and to be declared in the Servant API. Additionally,
  -- this library provides a small type alias helper `PageHeaders` to derive response headers from
  -- a range. For example:
  --
  -- > type API =
  -- >   "resource"
  -- >   :> Header "Range" MyRange
  -- >   :> GetPartialContent '[JSON] (Headers MyHeaders [MyResource])
  --
  -- The range is then provided to the corresponding handler as a @Maybe MyRange@ type and can be
  -- used by the backend service to actually apply the given range and fetch the resources demanded
  -- by the client. To send the response, one can leverage the `returnPage` defined in the
  -- `HasPagination` type-class from the `getRangeField` getter. For example:
  --
  -- > server :: Maybe MyRange -> Handler (Headers MyHeaders [MyResource])
  -- > server mrange =
  -- >   case mrange of
  -- >     Just range -> do
  -- >       -- ...
  -- >       returnPage (Just count) range xs
  -- >
  -- >     Nothing ->
  -- >       -- ...
  --
  -- * Multiple Ranges
  --
  -- TODO

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

  -- * Use Ranges
  , HasPagination(..)

  -- * Combine Ranges
  , (:|:)(..)
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
    <> " "  <> toUrlPiece start
    <> ".." <> toUrlPiece end

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
    Text.pack (symbolVal (Proxy :: Proxy field)) <> " " <> toUrlPiece r

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
  { defaultRangeLimit :: Int
  , defaultRangeSkip  :: Int
  , defaultRangeOrder :: RangeOrder
  } deriving (Eq, Show)


-- | Some default options of default values for a Range (limit 100; skip 0; order desc)
defaultOptions :: FromRangeOptions
defaultOptions =
  FromRangeOptions 100 0 RangeDesc


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
            <*> ifOpt "skip"  defaultRangeSkip opts
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
class (KnownSymbol field, ToHttpApiData typ) => HasPagination resource field typ | resource field -> typ where
  (^.) :: resource -> Proxy field -> typ
  default (^.) :: resource -> Proxy field -> typ
  resource ^. field = getRangeField field resource

  getRangeField :: Proxy field -> resource -> typ
  default getRangeField :: Proxy field -> resource -> typ
  getRangeField field resource = resource ^. field

  returnPage_ :: forall m ranges.
    ( Monad m
    , (Range field typ) :<: ranges
    , ToAcceptRanges ranges
    , ToHttpApiData (ContentRange ranges)
    , ToHttpApiData (NextRange ranges)
    ) => (Range field typ) -> [resource] -> m (Headers (PageHeaders ranges) [resource])
  returnPage_ =
    returnPage Nothing
  {-# INLINE returnPage_ #-}

  returnPage :: forall m ranges.
    ( Monad m
    , (Range field typ) :<: ranges
    , ToAcceptRanges ranges
    , ToHttpApiData (ContentRange ranges)
    , ToHttpApiData (NextRange ranges)
    ) => TotalCount -> (Range field typ) -> [resource] -> m (Headers (PageHeaders ranges) [resource])
  returnPage count range rs = do
    let field =
          Proxy :: Proxy field

    let boundaries = (,)
          <$> fmap (^. field) (Safe.headMay rs)
          <*> fmap (^. field) (Safe.lastMay rs)

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
              liftRange $ (range { rangeValue = Just start } :: Range field typ)

        let rangeEnd =
              liftRange $ (range { rangeValue = Just end } :: Range field typ)

        let rangeNext =
              liftRange $ (range { rangeValue = Just end, rangeOffset = 0 } :: Range field typ)

        let contentRange =
              addHeader $ ContentRange
                { contentRangeStart = rangeStart
                , contentRangeEnd   = rangeEnd
                }

        let nextRange =
              addHeader $ NextRange $ rangeNext

        return
          $ acceptRanges $ contentRange $ nextRange $ totalCount rs
