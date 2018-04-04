{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies     #-}

module Main where

import           Control.Applicative      ((<|>))
import           Data.Maybe               (fromMaybe)
import           Data.Proxy               (Proxy (..))
import           Servant
import           Servant.Pagination

import qualified Network.Wai.Handler.Warp as Warp

import           Color


--  Ranges definitions

instance HasPagination Color "name" where
  type RangeType Color "name" = String
  getRangeField _  = name
  getRangeOptions _ _ = defaultOptions
    { defaultRangeLimit = 5
    , defaultRangeOrder = RangeAsc
    }

instance HasPagination Color "rgb" where
  type RangeType Color "rgb" = Int
  getRangeField _ = sum . rgb


-- API

type API =
  "colors"
    :> Header "Range" (Ranges '["name", "rgb"] Color)
    :> GetPartialContent '[JSON] (Headers MyHeaders [Color])

type MyHeaders =
  PageHeaders '["name", "rgb"] Color


-- Application

defaultRange :: Range "name" String
defaultRange =
  getDefaultRange (Proxy @Color) Nothing

server :: Server API
server mrange =
  fromMaybe (returnNameRange defaultRange) handler
 where
  handler =
        fmap returnNameRange (mrange >>= getRange)
    <|> fmap returnRGBRange  (mrange >>= getRange)

  returnNameRange (range :: Range "name" String) =
    returnRange range (applyRange range colors)

  returnRGBRange (range :: Range "rgb" Int) =
    returnRange range (applyRange range colors)


main :: IO ()
main =
  Warp.run 1337 (serve (Proxy :: Proxy API) server)


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
