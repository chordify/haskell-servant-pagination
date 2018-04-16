# servant-pagination [![](https://img.shields.io/hackage/v/servant-pagination.svg)](https://hackage.haskell.org/package/servant-pagination)

## Overview

This module offers opinionated helpers to declare a type-safe and a flexible pagination
mechanism for Servant APIs. This design, inspired by [Heroku's API](https://devcenter.heroku.com/articles/platform-api-reference#ranges),
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

For example:

- `Accept-Ranges: createdAt, modifiedAt`
- `Content-Range: createdAt 2017-01-15T23:14:51.000Z..2017-02-18T06:10:23.000Z`
- `Next-Range: createdAt 2017-02-19T12:56:28.000Z; offset 0; limit 100; order desc`


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

instance ToJSON Color where
  toJSON = genericToJSON defaultOptions

instance HasPagination Color "name" where
  type RangeType Color "name" = String
  getFieldValue _ = name
```

That's it, the range is ready to use and to be declared in the Servant API. Additionally,
this library provides a small type alias helper `PageHeaders` to derive response headers from
a range. For example:

```hs
type API =
  "colors"
  :> Header "Range" (Ranges '["name"] Color)
  :> GetPartialContent '[JSON] (Headers (PageHeaders '["name"] Color) [Color])
```

The range is then provided to the corresponding handler as a `Maybe NameRange` (for Servant
<0.13) type and can be used by the backend service to actually apply the given range and 
fetch the resources demanded by the client. To send the response, one can leverage the
`returnPage` to lift a collection of resources into a Servant Handler:

```hs
defaultRange :: Range "name" String
defaultRange =
  getDefaultRange (Proxy @Color)

server :: Maybe (Ranges '["name"] Color) -> Handler (Headers (PageHeaders '["name"] Color) [Color])
server mrange = do
  let range =
        fromMaybe defaultRange (mrange >>= extractRange)

  returnRange range (applyRange range colors)
```

> See `examples/Simple.hs` for a running version of this guide.


## Multiple Ranges

As you've probably noticed, the 'Ranges' type takes a list of 'Symbol' of accepted fields. For
each of those 'Symbol', there must be a instance of `HasPagination` tighting the 'Symbol' to a
'Resource' and a given type. This enables you to define as many ranges as you want on a given
resource type. For instance, one could go for:

```hs
instance HasPagination Color "hex" where
  type RangeType Color "hex" = String
  getFieldValue _ = hex

-- to then define: Ranges '["name", "hex"] Color
```

> See `examples/Complex.hs` for more complex examples.


## Parsing Options

By default, `servant-pagination` provides an implementation of `getRangeOptions` for each 
`HasPagination` type-class. However, this can be overwritten when defining a instance of that
class to provide your own options. This options come into play when a `Range` header is
received and isn't fully specified (`limit`, `offset`, `order` are all optional) to provide 
default fallback values for those.

For instance, let's say we wanted to change the default limit to `5` in for our range on
`"name"`, we could tweak the corresponding `HasPagination` instance as follows:

```hs
instance HasPagination Color "name" where
  type RangeType Color "name" = String
  getFieldValue _ = name
  getRangeOptions _ _ = defaultOptions { defaultRangeLimit = 5 }
```


## Changelog

[CHANGELOG.md](CHANGELOG.md)


## License

[LGPL-3 © 2018 Chordify](LICENSE)
