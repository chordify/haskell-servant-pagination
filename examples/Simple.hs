{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies     #-}

module Main where

import           Data.Maybe               (fromMaybe)
import           Data.Proxy               (Proxy (..))
import           Servant
import           Servant.Pagination

import qualified Network.Wai.Handler.Warp as Warp

import           Color


--  Ranges definitions

instance HasPagination Color "patate" where
  type RangeType Color "patate" = String
  getRangeField _ = name

-- API

type API =
  "colors"
    :> Header "Range" (Ranges '["patate"] Color)
    :> GetPartialContent '[JSON] (Headers (PageHeaders '["patate"] Color) [Color])


-- Application

defaultRange :: Range "patate" String
defaultRange =
  getDefaultRange (Proxy @Color) Nothing

server :: Server API
server mrange = do
  let range =
        fromMaybe defaultRange (mrange >>= getRange)

  returnRange range (applyRange range colors)

main :: IO ()
main =
  Warp.run 1337 (serve (Proxy :: Proxy API) server)


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
-- < Next-Range: name Aqua;limit 100;offset 1;order desc
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
