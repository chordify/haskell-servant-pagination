{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import           Data.Proxy               (Proxy (..))
import           Servant
import           Servant.Pagination

import qualified Network.Wai.Handler.Warp as Warp

import           Color


--  Ranges definitions

-- | Custom options for ranges
myOpts :: FromRangeOptions
myOpts =
  defaultOptions { defaultRangeLimit = 5, defaultRangeOrder = RangeAsc }

-- | A range on the colors' name
type NameRange =
  Range "name" String

instance FromHttpApiData NameRange where
  parseUrlPiece =
    parseRange myOpts

instance HasPagination Color "name" where
  type RangeType Color "name" = String
  getRangeField _ =
    name

-- | A range on the sum of the rgb components of a color
type RGBRange =
  Range "rgb" Int

instance FromHttpApiData RGBRange where
  parseUrlPiece =
    parseRange myOpts

instance HasPagination Color "rgb" where
  type RangeType Color "rgb" = Int

  getRangeField _ =
    sum . rgb


-- API

type API =
  "colors"
    :> Header "Range" (NameRange :|: RGBRange)
    :> GetPartialContent '[JSON] (Headers MyHeaders [Color])

type MyHeaders =
  PageHeaders (NameRange :|: RGBRange)


-- Application

server :: Server API
server mrange = do
  let range =
        defaultRange Nothing myOpts :: NameRange

  case mrange of
    Nothing ->
      returnPage (Just nColors) range (applyRange range colors)

    Just (InL nameRange) ->
      returnPage (Just nColors) nameRange (applyRange nameRange colors)

    Just (InR rgbRange) ->
      returnPage (Just nColors) rgbRange (applyRange rgbRange colors)


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
