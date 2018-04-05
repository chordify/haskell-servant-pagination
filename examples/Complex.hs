{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies     #-}

module Main where

import           Control.Applicative      ((<|>))
import           Data.Maybe               (fromMaybe)
import           Data.Proxy               (Proxy (..))
import           Servant
import           Servant.Pagination

import qualified Data.Char                as Char
import qualified Network.Wai.Handler.Warp as Warp

import           Color


--  Ranges definitions

-- By default, a Range relies on `defaultOptions` but any instance can define its own options
instance HasPagination Color "name" where
  type RangeType Color "name" = String
  getFieldValue _  = name
  getRangeOptions _ _ = defaultOptions
    { defaultRangeLimit = 5
    , defaultRangeOrder = RangeAsc
    }

-- We can declare more than one range on a given type if they use different symbol field
instance HasPagination Color "hex" where
  type RangeType Color "hex" = String
  getFieldValue _ = map Char.toUpper . hex

instance HasPagination Color "rgb" where
  type RangeType Color "rgb" = Int
  getFieldValue _ = sum . rgb


-- API

type API =
  "colors"
    :> Header "Range" (Ranges '["name", "rgb", "hex"] Color)
    :> GetPartialContent '[JSON] (Headers MyHeaders [Color])

-- PageHeaders fields resource ~ '[Header h typ], thus we can add extra headers
-- as we desire.
type MyHeaders =
  Header "Total-Count" Int ': PageHeaders '["name", "rgb", "hex"] Color


-- Application

defaultRange :: Range "name" String
defaultRange =
  getDefaultRange (Proxy @Color) Nothing

server :: Server API
server mrange =
  addHeader (length colors) <$> handler mrange
 where
  -- 'extractRange' tries to extract a range if it has the right type, and yields 'Nothing'
  -- otherwise. We can use the '<|>' alternative combinator to try handlers one after
  -- the other
  handler r =
    fromMaybe (returnNameRange defaultRange) $
          fmap returnNameRange (r >>= extractRange)
      <|> fmap returnRGBRange  (r >>= extractRange)
      <|> fmap returnHexRange  (r >>= extractRange)

  -- Handlers below are very simple, in practice, they're likely to trigger different functions
  -- or database calls.
  returnNameRange (range :: Range "name" String) =
    returnRange range (applyRange range colors)

  returnRGBRange (range :: Range "rgb" Int) =
    returnRange range (applyRange range colors)

  returnHexRange (range :: Range "hex" String) =
    returnRange range (applyRange range colors)


main :: IO ()
main =
  Warp.run 1337 (serve (Proxy @API) server)


-- Examples

-- $ curl -v http://localhost:1337/colors
--
-- > GET /colors HTTP/1.1
--
-- < HTTP/1.1 206 Partial Content
-- < Content-Type: application/json;charset=utf-8
-- < Accept-Ranges: name,rgb
-- < Content-Range: name Aqua..CadetBlue
-- < Next-Range: name CadetBlue;limit 5;offset 1;order asc
-- < Total-Count: 59


-- $ curl -v http://localhost:1337/colors --header 'Range: rgb'
--
-- > GET /colors HTTP/1.1
-- > Range: rgb
--
-- < HTTP/1.1 206 Partial Content
-- < Content-Type: application/json;charset=utf-8
-- < Accept-Ranges: name,rgb
-- < Content-Range: rgb 765..0
-- < Next-Range: rgb 0;limit 100;offset 1;order desc
-- < Total-Count: 59


-- $ curl -v http://localhost:1337/colors --header 'Range: name Green; limit 10; order desc'
--
-- > Get /colors HTTP/1.1
-- > Range: name; limit 10; order desc
-- >
-- < HTTP/1.1 206 Partial Content
-- < Content-Type: application/json;charset=utf-8
-- < Accept-Ranges: name,rgb
-- < Content-Range: name Green..DarkMagenta
-- < Next-Range: name DarkMagenta;limit 10;offset 1;order desc
-- < Total-Count: 59
