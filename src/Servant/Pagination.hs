{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}

module Servant.Pagination where

import           Data.List      (filter, find)
import           Data.Maybe     (fromMaybe, listToMaybe)
import           Data.Proxy     (Proxy (..))
import           Data.Semigroup ((<>))
import           Data.Text      (Text)
import           GHC.Generics   (Generic)
import           GHC.TypeLits   (KnownSymbol, Symbol, symbolVal)
import           Servant        (FromHttpApiData (..), Header, Headers, ToHttpApiData (..),
                                 addHeader, noHeader)

import qualified Data.List      as List
import qualified Data.Text      as Text
import qualified Safe


--
-- DECLARATION
--

-- | Set of constraints that apply to each types that can be target for a Range Header
type IsRangeType typ =
  ( Show typ
  , Ord typ
  , Eq typ
  , FromHttpApiData typ
  , ToHttpApiData typ
  )


class (IsRangeType typ) => HasPagination res typ where
  type FieldFor (res :: *) (typ :: *) :: Symbol
  getField :: res -> typ
  paginationOptions :: Proxy (res, typ) -> PaginationOptions
  paginationOptions _ = defaultOptions


-- | Default values to apply when parsing a Range
data PaginationOptions  = PaginationOptions
  { defaultRangeLimit  :: Int
  , defaultRangeOffset :: Int
  , defaultRangeOrder  :: RangeOrder
  } deriving (Eq, Show)

--
-- | Some default options of default values for a Range (limit 100; offset 0; order desc)
defaultOptions :: PaginationOptions
defaultOptions =
  PaginationOptions 100 0 RangeDesc


--
-- RANGE
--

-- An actual Range parsed from a `Range` header. A Range
data Range (ranges :: [*]) (res :: *) =
  forall typ. (IsRangeType typ, HasPagination res typ, KnownSymbol (FieldFor res typ)) => Range
  { rangeValue  :: Maybe typ   -- ^ The value of that field, beginning of the range
  , rangeLimit  :: Int         -- ^ Maximum number of resources to return
  , rangeOffset :: Int         -- ^ Offset, number of resources to skip after the starting value
  , rangeOrder  :: RangeOrder  -- ^ The order of sorting (ascending or descending)
  }

instance Show (Range ranges res) where
  showsPrec prec Range{..} rest =
    let
      inner =
        "Range " ++ showsPrec 11 rangeValue (" " ++ show rangeLimit ++ " " ++ show rangeOffset ++ " " ++ show rangeOrder) ++ rest
    in
      if prec > 10 then
        "(" ++ inner ++ ")"
      else
        inner

instance ToHttpApiData (Range ranges res) where
  toUrlPiece r@Range{..} =
    Text.pack (fieldFor r)
    <> maybe "" (\v -> " " <> toUrlPiece v) rangeValue
    <> ";limit "  <> toUrlPiece rangeLimit
    <> ";offset " <> toUrlPiece rangeOffset
    <> ";order "  <> toUrlPiece rangeOrder

instance FromHttpApiData (Range '[] res) where
  parseUrlPiece _ =
    Left "Unknown Range" -- Terminal case

instance
  ( FromHttpApiData (Range ranges res)
  , HasPagination res typ
  , field ~ FieldFor res typ
  , KnownSymbol field
  ) => FromHttpApiData (Range (typ ': ranges) res) where
  parseUrlPiece urlPiece =
    let
      field =
        Text.pack (symbolVal $ Proxy @field)

      toTuples =
        filter (/= "") . Text.splitOn (Text.singleton ' ')

      args =
        map toTuples $ Text.splitOn (Text.singleton ';') urlPiece

      rangeTyp = case args of
        (field' : value) : rest | field == field' -> do
          opts <-
            traverse parseOpt rest

          let PaginationOptions{..} =
                paginationOptions (Proxy @(res, typ))

          Range
            <$> sequence (fmap parseValue (listToMaybe value))
            <*> ifOpt "limit"  defaultRangeLimit  opts
            <*> ifOpt "offset" defaultRangeOffset opts
            <*> ifOpt "order"  defaultRangeOrder  opts

        _ ->
          Left "Invalid Range"
    in
      rangeTyp `orElse` fmap rangeNat (parseUrlPiece urlPiece :: Either Text (Range ranges res))
    where
      parseValue :: Text -> Either Text typ
      parseValue =
        parseUrlPiece

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


data RangeOrder
  = RangeAsc
  | RangeDesc
  deriving (Eq, Show, Ord, Enum, Bounded, Generic)

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


--
-- RESPONSE HEADERS
--

-- | Type alias to declare response headers related to pagination
type PageHeaders ranges res =
  '[ Header "Accept-Ranges" (AcceptRanges ranges res)
   , Header "Content-Range" (ContentRange ranges res)
   , Header "Next-Range"    (Range ranges res)
   ]


-- | Accepted Ranges in the `Accept-Ranges` response's header
data AcceptRanges (ranges :: [*]) (res :: *) = AcceptRanges


instance (SymbolVals (AcceptRanges ranges res)) => ToHttpApiData (AcceptRanges ranges res) where
  toUrlPiece _ =
    Text.pack $ List.intercalate "," $ symbolVals $ Proxy @(AcceptRanges ranges res)


-- | Actual range returned, in the `Content-Range` response's header
data ContentRange (ranges :: [*]) (res :: *) = ContentRange
  { contentRangeStart :: Range ranges res
  , contentRangeEnd   :: Range ranges res
  }


instance ToHttpApiData (ContentRange ranges res) where
  toUrlPiece (ContentRange start end) =
    let
      field =
        Text.pack $ fieldFor start

      mkRangePiece Range{..} =
        fromMaybe "" (toUrlPiece <$> rangeValue)
    in
      field <> " " <> mkRangePiece start <> ".." <> mkRangePiece end


--
-- UTILS
--

-- | Obtain the field, as string, from which the range's value refers to
fieldFor :: forall ranges res. Range ranges res -> String
fieldFor (Range (_ :: Maybe typ) _ _ _) =
  symbolVal (Proxy @(FieldFor res typ))


-- | Some default range based on the default options
mkRange :: forall typ res fields
  . (IsRangeType typ, HasPagination res typ, KnownSymbol (FieldFor res typ))
  => Maybe typ
  -> Range fields res
mkRange val =
  let
    PaginationOptions{..} =
      paginationOptions $ Proxy @(res, typ)
  in
    Range
      { rangeValue  = val
      , rangeLimit  = defaultRangeLimit
      , rangeOffset = defaultRangeOffset
      , rangeOrder  = defaultRangeOrder
      }


-- | Lift a list of result into a Monad (e.g. @Servant.Handler@) obtained from the given range
returnPage :: forall m ranges res
  . (Monad m, ToHttpApiData (AcceptRanges ranges res))
  => Range ranges res
  -> [res]
  -> m (Headers (PageHeaders ranges res) [res])
returnPage (Range (range :: Maybe typ) limit offset order) rs = do
  let boundaries = (,)
        <$> fmap getField (Safe.headMay rs)
        <*> fmap getField (Safe.lastMay rs)

  let acceptRanges =
        addHeader AcceptRanges

  case boundaries :: Maybe (typ, typ) of
    Nothing ->
      return $
        acceptRanges $ noHeader $ noHeader rs

    Just (start, end) -> do
      let nextOffset | range == Just end  = offset + length rs
                     | otherwise          = length $ takeWhile (\r -> getField r == end) $ reverse rs

      let rangeNext = Range
            { rangeValue  = Just end
            , rangeLimit  = limit
            , rangeOffset = nextOffset
            , rangeOrder  = order
            } :: Range ranges res

      let rangeStart = Range
            { rangeValue  = Just start
            , rangeLimit  = limit
            , rangeOffset = offset
            , rangeOrder  = order
            } :: Range ranges res

      let rangeEnd = Range
            { rangeValue  = Just end
            , rangeLimit  = limit
            , rangeOffset = offset
            , rangeOrder  = order
            } :: Range ranges res

      let contentRange =
            addHeader ContentRange
              { contentRangeStart = rangeStart
              , contentRangeEnd   = rangeEnd
              }

      let nextRange =
            addHeader rangeNext

      return $
        acceptRanges $ contentRange $ nextRange rs


-- | Apply a range to a list of element
applyRange :: forall ranges res. Range ranges res -> [res] -> [res]
applyRange (Range (value :: Maybe typ) limit offset order) =
  let
    sortRel =
      case order of
        RangeDesc ->
          \a b -> compare @typ (getField b) (getField a)

        RangeAsc ->
          \a b -> compare @typ (getField a) (getField b)

    dropRel =
      case (value, order) of
        (Nothing, _) ->
          const False

        (Just a, RangeDesc) ->
          (> a) . getField

        (Just a, RangeAsc) ->
          (< a) . getField
  in
      List.take limit
    . List.drop offset
    . List.dropWhile dropRel
    . List.sortBy sortRel


--
-- INTERNALS
--

-- | Natural transformation between two ranges
rangeNat :: Range ranges res -> Range (typ ': ranges) res
rangeNat Range{..} = Range
  { rangeValue  = rangeValue
  , rangeLimit  = rangeLimit
  , rangeOffset = rangeOffset
  , rangeOrder  = rangeOrder
  }

-- | Helper to execute two `Either a b` successively
orElse :: Either a b -> Either a b -> Either a b
orElse a b =
  either (const b) (const a) a
{-# INLINE orElse #-}


class SymbolVals a where
  symbolVals :: Proxy a -> [String]

instance SymbolVals (AcceptRanges '[] res) where
  symbolVals _ = []

instance (SymbolVals (AcceptRanges ranges res), HasPagination res typ, field ~ FieldFor res typ, KnownSymbol field) => SymbolVals (AcceptRanges (typ ': ranges) res) where
  symbolVals _ =
    symbolVal (Proxy @field) : symbolVals (Proxy @(AcceptRanges ranges res))
