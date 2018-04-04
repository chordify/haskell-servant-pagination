{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Servant.Pagination where
--  (
--  -- * Types
--    Range(..)
--  , IsRangeType
--  , RangeOrder(..)
--  , AcceptRanges (..)
--  , ContentRange (..)
--  , PageHeaders
--  , mkRange
--
--  -- * Declare Ranges
--  , HasPagination(..)
--  , RangeOptions(..)
--  , defaultOptions
--
--  -- * Use Ranges
--  , returnPage
--  , applyRange
--  ) where

import           Data.List      (filter, find)
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

-- | Set of constraints that apply to each types that can be target for a Range Header
type IsRangeType r =
  ( Show r
  , Ord r
  , Eq r
  , FromHttpApiData r
  , ToHttpApiData r
  )

-- An actual Range parsed from a `Range` header. A Range
data Ranges :: [Symbol] -> * -> * where
  Lift   :: Ranges fields typ -> Ranges (y ': fields) typ
  Ranges ::
   ( KnownSymbol field
   , HasPagination typ field
   , IsRangeType (RangeType typ field)
   ) => Range field (RangeType typ field) -> Ranges (field ': fields) typ

data Range (field :: Symbol) (a :: *) = Range
  { rangeValue  :: Maybe a     -- ^ The value of that field, beginning of the range
  , rangeLimit  :: Int         -- ^ Maximum number of resources to return
  , rangeOffset :: Int         -- ^ Offset, number of resources to skip after the starting value
  , rangeOrder  :: RangeOrder  -- ^ The order of sorting (ascending or descending)
  , rangeField  :: Proxy field -- ^ Actual field this Range actually refers to
  }

class GetRange (fields :: [Symbol]) (field :: Symbol) where
  getRange
    :: (r ~ RangeType typ field, KnownSymbol field, HasPagination typ field, IsRangeType r)
    => Ranges fields typ
    -> Maybe (Range field r)

instance GetRange (field ': fields) field where
  getRange (Ranges r) = Just r
  getRange (Lift _)   = Nothing

instance {-# OVERLAPPABLE #-} GetRange fields field => GetRange (y ': fields) field where
  getRange (Ranges _) = Nothing
  getRange (Lift r)   = getRange r


class PutRange (fields :: [Symbol]) (field :: Symbol) where
  putRange
    :: (r ~ RangeType typ field, KnownSymbol field, HasPagination typ field, IsRangeType r)
    => Range field r
    -> Ranges fields typ

instance PutRange (field ': fields) field where
  putRange = Ranges

instance {-# OVERLAPPABLE #-} (PutRange fields field) => PutRange (y ': fields) field where
  putRange = Lift . putRange


instance ToHttpApiData (Ranges fields typ) where
  toUrlPiece (Lift range) =
    toUrlPiece range

  toUrlPiece (Ranges Range{..}) =
    Text.pack (symbolVal rangeField)
    <> maybe "" (\v -> " " <> toUrlPiece v) rangeValue
    <> ";limit "  <> toUrlPiece rangeLimit
    <> ";offset " <> toUrlPiece rangeOffset
    <> ";order "  <> toUrlPiece rangeOrder

instance FromHttpApiData (Ranges '[] typ) where
  parseUrlPiece _ =
    Left "Invalid Range"

instance
  ( FromHttpApiData (Ranges fields typ)
  , KnownSymbol field
  , HasPagination typ field
  , IsRangeType (RangeType typ field)
  ) => FromHttpApiData (Ranges (field ': fields) typ) where
  parseUrlPiece txt =
    let
      RangeOptions{..} = getRangeOptions (Proxy @typ) (Proxy @field)

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

        _ -> -- recurse to other fields
          Lift <$> (parseUrlPiece txt :: Either Text (Ranges fields typ))
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
type PageHeaders (fields :: [Symbol]) typ =
  '[ Header "Accept-Ranges" (AcceptRanges fields)
   , Header "Content-Range" (ContentRange fields typ)
   , Header "Next-Range"    (Ranges fields typ)
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
data ContentRange (fields :: [Symbol]) typ =
  forall field. (KnownSymbol field, ToHttpApiData (RangeType typ field)) => ContentRange
  { contentRangeStart :: RangeType typ field
  , contentRangeEnd   :: RangeType typ field
  , contentRangeField :: Proxy field
  }

instance ToHttpApiData (ContentRange fields res) where
  toUrlPiece (ContentRange start end field) =
    Text.pack (symbolVal field) <> " " <> toUrlPiece start <> ".." <> toUrlPiece end


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

  getRangeOptions :: Proxy typ -> Proxy field -> RangeOptions
  getRangeOptions _ _ = defaultOptions

  getDefaultRange :: Proxy typ -> Maybe (RangeType typ field) -> Range field (RangeType typ field)
  getDefaultRange _ val =
    let
      RangeOptions{..} = getRangeOptions (Proxy @typ) (Proxy @field)
    in Range
      { rangeValue  = val
      , rangeLimit  = defaultRangeLimit
      , rangeOffset = defaultRangeOffset
      , rangeOrder  = defaultRangeOrder
      , rangeField  = Proxy @field
      }

returnRange
  :: ( Monad m
     , ToHttpApiData (AcceptRanges fields)
     , KnownSymbol field
     , HasPagination typ field
     , IsRangeType (RangeType typ field)
     , PutRange fields field
     )
  => Range field (RangeType typ field)
  -> [typ]
  -> m (Headers (PageHeaders fields typ) [typ])
returnRange Range{..} rs = do
  let boundaries = (,)
        <$> fmap (getRangeField rangeField) (Safe.headMay rs)
        <*> fmap (getRangeField rangeField) (Safe.lastMay rs)

  case boundaries of
    Nothing ->
      return $ addHeader AcceptRanges $ noHeader $ noHeader rs

    Just (start, end) -> do
      let nextOffset | rangeValue == Just end = rangeOffset + length rs
                     | otherwise              = length $ takeWhile ((==) end . getRangeField rangeField) (reverse rs)

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

-- | Default values to apply when parsing a Range
data RangeOptions  = RangeOptions
  { defaultRangeLimit  :: Int
  , defaultRangeOffset :: Int
  , defaultRangeOrder  :: RangeOrder
  } deriving (Eq, Show)


-- | Some default options of default values for a Range (limit 100; offset 0; order desc)
defaultOptions :: RangeOptions
defaultOptions =
  RangeOptions 100 0 RangeDesc


applyRange
  :: (KnownSymbol field, HasPagination typ field, IsRangeType (RangeType typ field))
  => Range field (RangeType typ field) -> [typ] -> [typ]
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
          (> a) . getRangeField rangeField

        (Just a, RangeAsc) ->
          (< a) . getRangeField rangeField
  in
      List.take rangeLimit
    . List.drop rangeOffset
    . List.dropWhile dropRel
    . List.sortBy sortRel
