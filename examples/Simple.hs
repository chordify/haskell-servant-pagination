{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import           Data.Maybe               (fromMaybe)
import           Data.Proxy               (Proxy (..))
import           Servant                  ((:>), GetPartialContent, Header, Headers, JSON, Server)
import           Servant.Pagination       (HasPagination (..), PageHeaders, Range, applyRange,
                                           defaultRange, returnPage)

import qualified Network.Wai.Handler.Warp as Warp
import qualified Servant

import           Color


-- API

-- | A range on the Color's name
instance HasPagination Color String where
  type FieldFor Color String = "name"
  getField = name

type Paginated res =
  Headers (PageHeaders '[String] Color) res

type API =
  "colors"
    :> Header "Range" (Range '[String] Color)
    :> GetPartialContent '[JSON] (Paginated [Color])


-- Application

server :: Server API
server mrange = do
  let range =
        fromMaybe (defaultRange (Nothing :: Maybe String)) mrange

  returnPage range (applyRange range colors)

main :: IO ()
main =
  Warp.run 1337 $ Servant.serve (Proxy @API) server


-- Examples

-- $ curl -v http://localhost:1337/colors
--
-- > GET /colors HTTP/1.1
-- > Host: localhost:1337
-- > User-Agent: curl/7.47.0
-- > Accept: */*
-- >
-- < HTTP/1.1 206 Partial Content
-- < Transfer-Encoding: chunked
-- < Date: Tue, 30 Jan 2018 12:45:17 GMT
-- < Server: Warp/3.2.13
-- < Content-Type: application/json;charset=utf-8
-- < Accept-Ranges: name
-- < Content-Range: name Yellow..Aqua
-- < Next-Range: name Aqua;limit 100;offset 0;order desc
-- < Total-Count: 59


-- $ curl -v http://localhost:1337/colors --header 'Range: name; offset 59'
--
-- > GET /colors HTTP/1.1
-- > Range: name; offset 59
-- >
-- < HTTP/1.1 206 Partial Content
-- < Content-Type: application/json;charset=utf-8
-- < Accept-Ranges: name
-- < Total-Count: 59
