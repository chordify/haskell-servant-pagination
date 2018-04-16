-- | Opinionated Pagination Helpers for Servant APIs
--
--
-- Client can provide a `Range` header with their request with the following format
--
-- > Range: <field> [<value>][; offset <o>][; limit <l>][; order <asc|desc>]
--
-- Available ranges are declared using type-level list of accepted fields, bound to a given
-- resource and type using the 'HasPagination' type-class. The library provides unobtrusive
-- types and abstract away all the plumbing to hook that on an existing Servant API.
--
-- The 'IsRangeType' constraints summarize all constraints that must apply to a possible field
-- and heavily rely on the 'Web.Internal.FromHttpApiData' and 'Web.Internal.ToHttpApiData'.
--
-- > $ curl -v http://localhost:1337/colors -H 'Range: name; limit 10'
-- >
-- > > GET /colors HTTP/1.1
-- > > Host: localhost:1337
-- > > User-Agent: curl/7.47.0
-- > > Accept: */*
-- > >
-- > < HTTP/1.1 206 Partial Content
-- > < Transfer-Encoding: chunked
-- > < Date: Tue, 30 Jan 2018 12:45:17 GMT
-- > < Server: Warp/3.2.13
-- > < Content-Type: application/json;charset=utf-8
-- > < Accept-Ranges: name
-- > < Content-Range: name Yellow..Purple
-- > < Next-Range: name Purple;limit 10;offset 1;order desc
--
-- The 'Range' header is totally optional, but when provided, it indicates to the server what
-- parts of the collection is requested. As a reponse and in addition to the data, the server may
-- provide 3 headers to the client:
--
-- - @Accept-Ranges@: A comma-separated list of field upon which a range can be defined
-- - @Content-Range@: Actual range corresponding to the content being returned
-- - @Next-Range@: Indicate what should be the next `Range` header in order to retrieve the next range
--
-- This allows the client to work in a very _dumb_ mode where it simply consumes data from the server
-- using the value of the 'Next-Range' header to fetch each new batch of data. The 'Accept-Ranges'
-- comes in handy to self-document the API telling the client about the available filtering and sorting options
-- of a resource.
--
-- Here's a minimal example used to obtained the previous behavior; Most of the magic happens in the
-- 'returnRange' function which lift a collection of resources into a Servant handler, computing the
-- corresponding ranges from the range used to retrieve the resources.
--
-- @
-- -- Resource Type
--
-- data Color = Color
--   { name :: 'String'
--   , rgb  :: ['Int']
--   , hex  :: 'String'
--   } deriving ('Eq', 'Show', 'GHC.Generics.Generic')
--
-- colors :: [Color]
-- colors = [ {- ... -} ]
--
-- -- Ranges definitions
--
-- instance 'HasPagination' Color "name" where
--   type 'RangeType' Color "name" = 'String'
--   'getFieldValue' _ = name
--
--
-- -- API
--
-- type API =
--   "colors"
--     :> 'Servant.Header' \"Range\" ('Ranges' '["name"] Color)
--     :> 'Servant.GetPartialContent' '['Servant.JSON'] ('Servant.Headers' ('PageHeaders' '["name"] Color) [Color])
--
--
-- -- Application
--
-- defaultRange :: 'Range' "name" 'String'
-- defaultRange =
--   'getDefaultRange' ('Data.Proxy.Proxy' @Color)
--
-- server :: 'Servant.Server.Server' API
-- server mrange = do
--   let range =
--         'Data.Maybe.fromMaybe' defaultRange (mrange >>= 'extractRange')
--
--   'returnRange' range ('applyRange' range colors)
--
-- main :: 'IO' ()
-- main =
--   'Network.Wai.Handler.Warp.run' 1337 ('Servant.Server.serve' ('Data.Proxy.Proxy' @API) server)
-- @
module Servant.Pagination
  (
  -- * Types
  Ranges
  , Range(..)
  , RangeOrder(..)
  , AcceptRanges (..)
  , ContentRange (..)
  , PageHeaders
  , IsRangeType

  -- * Declare Ranges
  , HasPagination(..)
  , RangeOptions(..)
  , defaultOptions

  -- * Use Ranges
  , extractRange
  , putRange
  , returnRange
  , applyRange
  ) where

import           Data.List      (filter, find, intercalate)
import           Data.Maybe     (listToMaybe)
import           Data.Proxy     (Proxy (..))
import           Data.Semigroup ((<>))
import           Data.Text      (Text)
import           GHC.Generics   (Generic)
import           GHC.TypeLits   (KnownSymbol, Symbol, symbolVal)
import           Servant

import qualified Data.List      as List
import qualified Data.Text      as Text
import qualified Safe


--
-- TYPES
--

-- | Set of constraints that must apply to every type target of a 'Range'
type IsRangeType a =
  ( Show a
  , Ord a
  , Eq a
  , FromHttpApiData a
  , ToHttpApiData a
  )

-- | A type to specify accepted Ranges via the @Range@ HTTP Header. For example:
--
-- @
-- type API = "resources"
--   :> 'Servant.Header' \"Range\" ('Ranges' '["created_at"] Resource)
--   :> 'Servant.Get' '['Servant.JSON'] [Resource]
-- @
data Ranges :: [Symbol] -> * -> * where
  Lift   :: Ranges fields resource -> Ranges (y ': fields) resource
  Ranges
    :: HasPagination resource field
    => Range field (RangeType resource field)
    -> Ranges (field ': fields) resource

instance (Show (Ranges '[] res)) where
  showsPrec _ _ = flip mappend "Ranges"

instance (Show (Ranges fields res)) => Show (Ranges (field ': fields) res) where
  showsPrec prec (Lift r)   s = showsPrec prec r s
  showsPrec prec (Ranges r) s =
    let
      inner = "Ranges@" ++ showsPrec 11 r s
    in
      if prec > 10 then "(" ++ inner ++ ")" else inner


-- | An actual 'Range' instance obtained from parsing / to generate a @Range@ HTTP Header.
data Range (field :: Symbol) (a :: *) =
  (KnownSymbol field, IsRangeType a) => Range
  { rangeValue  :: Maybe a     -- ^ The value of that field, beginning of the range
  , rangeLimit  :: Int         -- ^ Maximum number of resources to return
  , rangeOffset :: Int         -- ^ Offset, number of resources to skip after the starting value
  , rangeOrder  :: RangeOrder  -- ^ The order of sorting (ascending or descending)
  , rangeField  :: Proxy field -- ^ Actual field this Range actually refers to
  }

instance Eq (Range field a) where
  (Range val0 lim0 off0 ord0 _) == (Range val1 lim1 off1 ord1 _) =
       val0 == val1
    && lim0 == lim1
    && off0 == off1
    && ord0 == ord1

instance Show (Range field a) where
  showsPrec prec Range{..} =
    let
      inner = "Range {" ++ args ++ "}"
      args  = intercalate ", "
        [ "rangeValue = "  ++ show rangeValue
        , "rangeLimit = "  ++ show rangeLimit
        , "rangeOffset = " ++ show rangeOffset
        , "rangeOrder = "  ++ show rangeOrder
        , "rangeField = "  ++ "\"" ++ symbolVal rangeField ++ "\""
        ]
    in
      flip mappend $ if prec > 10 then
        "(" ++ inner ++ ")"
      else
        inner


-- | Extract a 'Range' from a 'Ranges'
class ExtractRange (fields :: [Symbol]) (field :: Symbol) where
  -- | Extract a 'Range' from a 'Ranges'. Works like a safe 'read', trying to coerce a 'Range' instance to
  -- an expected type. Type annotation are most likely necessary to remove ambiguity. Note that a 'Range'
  -- can only be extracted to a type bound by the allowed 'fields' on a given 'resource'.
  --
  -- @
  -- extractDateRange :: 'Ranges' '["created_at", "name"] Resource -> 'Range' "created_at" 'Data.Time.Clock.UTCTime'
  -- extractDateRange =
  --   'extractRange'
  -- @
  extractRange
    :: HasPagination resource field
    => Ranges fields resource                         -- ^ A list of accepted Ranges for the API
    -> Maybe (Range field (RangeType resource field)) -- ^ A Range instance of the expected type, if it matches

instance ExtractRange (field ': fields) field where
  extractRange (Ranges r) = Just r
  extractRange (Lift _)   = Nothing
  {-# INLINE extractRange #-}

instance {-# OVERLAPPABLE #-} ExtractRange fields field => ExtractRange (y ': fields) field where
  extractRange (Ranges _) = Nothing
  extractRange (Lift r)   = extractRange r
  {-# INLINE extractRange #-}


-- | Put a 'Range' in a 'Ranges'
class PutRange (fields :: [Symbol]) (field :: Symbol) where
  putRange
    :: HasPagination resource field
    => Range field (RangeType resource field)
    -> Ranges fields resource

instance PutRange (field ': fields) field where
  putRange = Ranges
  {-# INLINE putRange #-}

instance {-# OVERLAPPABLE #-} (PutRange fields field) => PutRange (y ': fields) field where
  putRange = Lift . putRange
  {-# INLINE putRange #-}


instance ToHttpApiData (Ranges fields resource) where
  toUrlPiece (Lift range) =
    toUrlPiece range

  toUrlPiece (Ranges Range{..}) =
    Text.pack (symbolVal rangeField)
    <> maybe "" (\v -> " " <> toUrlPiece v) rangeValue
    <> ";limit "  <> toUrlPiece rangeLimit
    <> ";offset " <> toUrlPiece rangeOffset
    <> ";order "  <> toUrlPiece rangeOrder


instance FromHttpApiData (Ranges '[] resource) where
  parseUrlPiece _ =
    Left "Invalid Range"

instance
  ( FromHttpApiData (Ranges fields resource)
  , HasPagination resource field
  , KnownSymbol field
  , IsRangeType (RangeType resource field)
  ) => FromHttpApiData (Ranges (field ': fields) resource) where
  parseUrlPiece txt =
    let
      RangeOptions{..} = getRangeOptions (Proxy @field) (Proxy @resource)

      toTuples =
        filter (/= "") . Text.splitOn (Text.singleton ' ')

      args =
        map toTuples $ Text.splitOn (Text.singleton ';') txt

      field =
        Text.pack $ symbolVal (Proxy @field)
    in
      case args of
        (field' : value) : rest | field == field' -> do
          opts <-
            traverse parseOpt rest

          range <- Range
            <$> sequence (fmap parseQueryParam (listToMaybe value))
            <*> ifOpt "limit"  defaultRangeLimit opts
            <*> ifOpt "offset" defaultRangeOffset opts
            <*> ifOpt "order"  defaultRangeOrder opts
            <*> pure (Proxy @field)

          pure $ Ranges range

        _ ->
          Lift <$> (parseUrlPiece txt :: Either Text (Ranges fields resource))
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


-- | Type alias to declare response headers related to pagination
--
-- @
-- type MyHeaders =
--   'PageHeaders' '["created_at"] Resource
--
-- type API = "resources"
--   :> 'Servant.Header' \"Range\" ('Ranges' '["created_at"] Resource)
--   :> 'Servant.Get' '['Servant.JSON'] ('Servant.Headers' MyHeaders [Resource])
-- @
type PageHeaders (fields :: [Symbol]) (resource :: *) =
  '[ Header "Accept-Ranges" (AcceptRanges fields)
   , Header "Content-Range" (ContentRange fields resource)
   , Header "Next-Range"    (Ranges fields resource)
   ]

-- | Accepted Ranges in the `Accept-Ranges` response's header
data AcceptRanges (fields :: [Symbol]) = AcceptRanges

instance (KnownSymbol field) => ToHttpApiData (AcceptRanges '[field]) where
  toUrlPiece AcceptRanges =
    Text.pack (symbolVal (Proxy @field))

instance (ToHttpApiData (AcceptRanges (f ': fs)), KnownSymbol field) => ToHttpApiData (AcceptRanges (field ': f ': fs)) where
  toUrlPiece AcceptRanges =
    Text.pack (symbolVal (Proxy @field)) <> "," <> toUrlPiece (AcceptRanges @(f ': fs))


-- | Actual range returned, in the `Content-Range` response's header
data ContentRange (fields :: [Symbol]) resource =
  forall field. (KnownSymbol field, ToHttpApiData (RangeType resource field)) => ContentRange
  { contentRangeStart :: RangeType resource field
  , contentRangeEnd   :: RangeType resource field
  , contentRangeField :: Proxy field
  }

instance ToHttpApiData (ContentRange fields res) where
  toUrlPiece (ContentRange start end field) =
    Text.pack (symbolVal field) <> " " <> toUrlPiece start <> ".." <> toUrlPiece end


--
-- USE RANGES
--

-- | Available 'Range' on a given @resource@ must implements the 'HasPagination' type-class.
-- This class defines how the library can interact with a given @resource@ to access the value
-- to which a @field@ refers.
class KnownSymbol field => HasPagination resource field where
  type RangeType resource field :: *

  -- | Get the corressponding value of a Resource
  getFieldValue :: Proxy field -> resource -> RangeType resource field

  -- | Get parsing options for the 'Range' defined on this 'field'
  getRangeOptions :: Proxy field -> Proxy resource -> RangeOptions
  getRangeOptions _ _ = defaultOptions

  -- | Create a default 'Range' from a value and default 'RangeOptions'. Typical use-case
  -- is for when no or an invalid 'Range' header was provided.
  getDefaultRange
    :: IsRangeType (RangeType resource field)
    => Proxy resource
    -> Range field (RangeType resource field)
  getDefaultRange _ =
    let
      RangeOptions{..} = getRangeOptions (Proxy @field) (Proxy @resource)
    in Range
      { rangeValue  = Nothing @(RangeType resource field)
      , rangeLimit  = defaultRangeLimit
      , rangeOffset = defaultRangeOffset
      , rangeOrder  = defaultRangeOrder
      , rangeField  = Proxy @field
      }

-- | Lift an API response in a 'Monad', typically a 'Servant.Server.Handler'. 'Ranges' headers can be quite cumbersome to
-- declare and can be deduced from the resources returned and the previous 'Range'. This is exactly what this function
-- does.
--
-- @
-- myHandler
--  :: 'Maybe' ('Ranges' '["created_at"] Resource)
--  -> 'Servant.Server.Handler' ('Servant.Headers' ('PageHeaders' '["created_at"] Resource) [Resource])
-- myHandler mrange =
--  let range =
--        'Data.Maybe.fromMaybe' ('getDefaultRange' ('Data.Proxy.Proxy' @Resource)) (mrange >>= 'extractRange')
--
--  'returnRange' range ('applyRange' range resources)
-- @
returnRange
  :: ( Monad m
     , ToHttpApiData (AcceptRanges fields)
     , KnownSymbol field
     , HasPagination resource field
     , IsRangeType (RangeType resource field)
     , PutRange fields field
     )
  => Range field (RangeType resource field)               -- ^ Actual 'Range' used to retrieve the results
  -> [resource]                                           -- ^ Resources to return, fetched from a db or a local store
  -> m (Headers (PageHeaders fields resource) [resource]) -- ^ Resources embedded in a given 'Monad' (typically a 'Servant.Server.Handler', with pagination headers)
returnRange Range{..} rs = do
  let boundaries = (,)
        <$> fmap (getFieldValue rangeField) (Safe.headMay rs)
        <*> fmap (getFieldValue rangeField) (Safe.lastMay rs)

  case boundaries of
    Nothing ->
      return $ addHeader AcceptRanges $ noHeader $ noHeader rs

    Just (start, end) -> do
      let nextOffset | rangeValue == Just end = rangeOffset + length rs
                     | otherwise              = length $ takeWhile ((==) end . getFieldValue rangeField) (reverse rs)

      let nextRange = putRange Range
            { rangeValue  = Just end
            , rangeLimit  = rangeLimit
            , rangeOffset = nextOffset
            , rangeOrder  = rangeOrder
            , rangeField  = rangeField
            }

      let contentRange = ContentRange
            { contentRangeStart = start
            , contentRangeEnd   = end
            , contentRangeField = rangeField
            }

      return $ addHeader AcceptRanges $ addHeader contentRange $ addHeader nextRange rs

-- | Default values to apply when parsing a 'Range'
data RangeOptions  = RangeOptions
  { defaultRangeLimit  :: Int         -- ^ Default limit if not provided, default to @100@
  , defaultRangeOffset :: Int         -- ^ Default offset if not provided, default to @0@
  , defaultRangeOrder  :: RangeOrder  -- ^ Default order if not provided, default to 'RangeDesc'
  } deriving (Eq, Show)


-- | Some default options of default values for a Range (limit 100; offset 0; order desc)
defaultOptions :: RangeOptions
defaultOptions =
  RangeOptions 100 0 RangeDesc


-- | Helper to apply a 'Range' to a list of values. Most likely useless in practice
-- as results may come more realistically from a database, but useful for debugging or
-- testing.
applyRange
  :: HasPagination resource field
  => Range field (RangeType resource field) -- ^ A 'Range' instance on a given @resource@
  -> [resource]                             -- ^ A full-list of @resource@
  -> [resource]                             -- ^ The sublist obtained by applying the 'Range'
applyRange Range{..} =
  let
    sortRel =
      case rangeOrder of
        RangeDesc ->
          \a b -> compare (getFieldValue rangeField b) (getFieldValue rangeField a)

        RangeAsc ->
          \a b -> compare (getFieldValue rangeField a) (getFieldValue rangeField b)

    dropRel =
      case (rangeValue, rangeOrder) of
        (Nothing, _) ->
          const False

        (Just a, RangeDesc) ->
          (> a) . getFieldValue rangeField

        (Just a, RangeAsc) ->
          (< a) . getFieldValue rangeField
  in
      List.take rangeLimit
    . List.drop rangeOffset
    . List.dropWhile dropRel
    . List.sortBy sortRel
