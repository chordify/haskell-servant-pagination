{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import           Data.Maybe               (fromMaybe)
import           Data.Proxy               (Proxy (..))
import           Servant                  ((:>), GetPartialContent, Header, Headers, JSON, Server)
import           Servant.Pagination

import qualified Network.Wai.Handler.Warp as Warp
import qualified Servant

import           Color



-- API

-- | A range on the Color's name
instance HasPagination Color String where
  type FieldFor Color String = "name"
  getField = name

instance HasPagination Color Int where
  type FieldFor Color Int = "rgb"
  getField = sum . rgb
  paginationOptions _ = PaginationOptions
    { defaultRangeLimit  = 5
    , defaultRangeOffset = 0
    , defaultRangeOrder  = RangeAsc
    }

type Paginated res =
  Headers (PageHeaders '[String, Int] Color) res

type API =
  "colors"
    :> Header "Range" (Range '[String, Int] Color)
    :> GetPartialContent '[JSON] (Paginated [Color])


-- Application

server :: Server API
server mrange = do
  let range =
        fromMaybe (defaultRange (Nothing :: Maybe String)) mrange

  returnPage range (applyRange range colors)


main :: IO ()
main =
  Warp.run 1337 (Servant.serve (Proxy @API) server)


-- Examples

-- $ curl -v http://localhost:1337/colors
--
-- > GET /colors HTTP/1.1
--
-- < HTTP/1.1 206 Partial Content
-- < Content-Type: application/json;charset=utf-8
-- < Accept-Ranges: name,rgb
-- < Content-Range: name Aqua..CadetBlue
-- < Next-Range: name CadetBlue;limit 5;offset 0;order asc
-- < Total-Count: 59


-- $ curl -v http://localhost:1337/colors --header 'Range: rgb'
--
-- > GET /colors HTTP/1.1
-- > Range: rgb
--
-- < HTTP/1.1 206 Partial Content
-- < Content-Type: application/json;charset=utf-8
-- < Accept-Ranges: name,rgb
-- < Content-Range: rgb 0..128
-- < Next-Range: rgb 128;limit 5;offset 0;order asc
-- < Total-Count: 59


-- $ curl -v http://localhost:1337/colors --header 'Range: name Green; limit 10; order desc'
--
-- > Get /colors HTTP/1.1
-- > Range: name; limit 10; order desc
-- >
-- < HTTP/1.1 206 Partial Content
-- < Content-Type: application/json;charset=utf-8
-- < Accept-Ranges: name,rgb
-- < Content-Range: name Fuchsia..DarkMagenta
-- < Next-Range: name DarkMagenta;limit 10;offset 0;order desc
-- < Total-Count: 59
