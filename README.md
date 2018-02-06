# servant-pagination [![](https://img.shields.io/hackage/v/servant-pagination.svg)](https://hackage.haskell.org/package/servant-pagination)

## Overview

This module offers opinionated helpers to declare a type-safe and a flexible pagination
mecanism for Servant APIs. This design, inspired by [Heroku's API](https://devcenter.heroku.com/articles/platform-api-reference#ranges),
provides a small framework to communicate about a possible pagination feature of an endpoint,
enabling a client to consume the API in different fashions (pagination with offset / limit,
endless scroll using last referenced resources, ascending and descending ordering, etc.)

Therefore, client may provide a `Range` header with their request with the following format:

- `Range: <field> [<value>][; offset <o>][; limit <l>][; order <asc|desc>]`

For example: `Range: createdAt 2017-01-15T23:14:67.000Z; offset 5; order desc` indicates that
the client is willing to retrieve the next batch of document in descending order that were
created after the fifteenth of January, skipping the first 5.

As a response, the server may return the list of corresponding document, and augment the
response with 3 or 4 headers:

- `Accept-Ranges`: A comma-separated list of field upon which a range can be defined
- `Content-Range`: Actual range corresponding to the content being returned
- `Next-Range`: Indicate what should be the next `Range` header in order to retrieve the next range
- `Total-Count`: Optional, but if present, refers to the total number of resources in the collection

For example:

- `Accept-Ranges: createdAt, modifiedAt`
- `Content-Range: createdAt 2017-01-15T23:14:51.000Z..2017-02-18T06:10:23.000Z`
- `Next-Range: createdAt 2017-02-19T12:56:28.000Z; offset 0; limit 100; order desc`
- `Total-Count: 1442`


## Getting Starting

Code-wise, the integration is rather seamless and requires to declare a `Range` type on
on a given field and to provide an instance of `HasPagination` and `FromHttpApiData`. 
The `getRangeField` method from `HasPagination` is merely a getter to retrieve
a range's field value from a resource. 

```hs
data Color = Color
  { name :: String
  , rgb  :: [Int]
  , hex  :: String
  } deriving (Eq, Show, Generic)

type NameRange = Range "name" String

instance HasPagination MyResource "name" where
  type RangeType MyResource "name" = String
  getRangeField _ = name
```

That's it, the range is ready to use and to be declared in the Servant API. Additionally,
this library provides a small type alias helper `PageHeaders` to derive response headers from
a range. For example:

```hs
type API =
  "colors"
  :> Header "Range" NameRange
  :> GetPartialContent '[JSON] (Headers (PageHeaders NameRange) [Color])
```

The range is then provided to the corresponding handler as a `Maybe NameRange` type and can be
used by the backend service to actually apply the given range and fetch the resources demanded
by the client. To send the response, one can leverage the `returnPage_` defined in the
`HasPagination` type-class from the `getRangeField` getter. For example:

```hs
server :: Maybe NameRange -> Handler (Headers (PageHeaders NameRange) [Color])
server mrange = do
  let range =
        fromMaybe (defaultRange Nothing defaultOptions) mrange

  returnPage_ range (applyRange range colors)
  
  -- or, if the Total-Count is known:
  -- returnPage (Just nColors) range (applyRange range colors)
```


## Multiple Ranges

One may also define more than one range on a single endpoint. For this, `servant-pagination`
provides an operator (`:|:`) to combine ranges as well as necessary instances and plumbering 
to make it work transparently. Let's define another range on our *Color* resource:

```hs
type RGBRange = Range "rgb" Int

instance HasPagination Color "rgb" where
  type RangeType Color "rgb" = Int
  getRangeField _ = sum . rgb
```

From there, we can combine it seamlessly with our `NameRange` previously defined.

```hs
type ColorRanges = NameRange :|: RGBRange

type API =
  "colors"
  :> Header "Range" ColorRanges
  :> GetPartialContent '[JSON] (Headers (PageHeaders ColorRanges) [Color])
```

Looking up closely at the definition of `(:|:)`, we see that it provides two constructors on
which one can pattern-match to figure-out which of the two ranges was provided:

```hs
-- data a :|: b = InL a | InR b

server :: Maybe ColorRanges -> Handler (Headers (PageHeaders ColorRanges) [Color])
server mrange = do
  case mrange of
    Nothing              -> -- no range was provided
    Just (InL nameRange) -> -- a NameRange was provided
    Just (InR rgbRange)  -> -- a RGBRange was provided

  let range =
        fromMaybe (defaultRange Nothing defaultOptions) mrange

  returnPage_ range (applyRange range colors)
```

> *NOTE:* Ranges are combined left-wise (left associativity `infixl 7 :|:`), therefore, 
> to access ranges in a combination, one can pattern-match as follow:
>
> ```hs
> r  :|: r2         -- InL r        
> r1 :|: r          -- InR r      
> r  :|: r2 :|: r3  -- InL (InL r)
> r1 :|: r  :|: r3  -- InL (InR r)
> r1 :|: r2 :|: r   -- InR (InR r)
> ```
> ...and so forth


## Parsing Options

By default, `servant-pagination` provides an instance (overlappalbe) for `FromHttpApiData`
used behind the scene by `servant` in order to parse HTTP Headers. This instance defines 
default values for the range's limit, offset and order in case they're not passed by a client. 
It is possible to override these defaults by defining your own `FromHttpApiData` on a `Range`
leveraging the `FromRange` type-class defined for all ranges. The `parseRange` from the
`FromRange` class and `parseUrlPiece` from the `FromHttpApiData` have very similar signature.
The former has been designed to be used as a drop-in replacement for `parseUrlPiece` but with 
extra parsing options. 

For instance, let's say we wanted to change the default limit to `5` in our `NameRange`:

```hs
instance FromHttpApiData NameRange where
  parseUrlPiece =
    parseRange FromRangeOptions 
      { defaultRangeLimit  = 5
      , defaultRangeOffset = 0
      , defaultRangeOrder  = RangeDesc
      }
```

Alternatively, one can use the `defaultOptions :: FromRangeOptions` provided by
`servant-pagination`:


```hs
instance FromHttpApiData NameRange where
  parseUrlPiece =
    parseRange (defaultOptions { defaultRangeLimit = 5 })
```

## Changelog

[CHANGELOG.md](CHANGELOG.md)


## License

[LGPL-3 © 2018 Chordify](LICENSE)
