# servant-pagination

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
on a given field and to provide an instance of `HasPagination` and `FromHttpApiData` for
this type. The `getRangeField` method from `HasPagination` is merely a getter to retrieve
a range's field value from a resource. Note that `Range` (and all combinations of `Range`)
provides an instance of a `FromRange` class that can be leveraged to easily define instances
for the `FromHttpApiData` class. For example:

```hs
data MyResource = MyResource
  { anything  :: String
  , createdAt :: Int
  }

type MyRange = Range "createdAt" UTCTime
type MyHeaders = PageHeaders MyRange

instance FromHttpApiData MyRange where
  parseUrlPiece = parseRange defaultOptions

instance HasPagination MyResource "createdAt" where
  type PaginationType MyResource "createdAt" = UTCTime
  getRangeField _ = createdAt
```

That's it, the range is ready to use and to be declared in the Servant API. Additionally,
this library provides a small type alias helper `PageHeaders` to derive response headers from
a range. For example:

```hs
type API =
  "resource"
  :> Header "Range" MyRange
  :> GetPartialContent '[JSON] (Headers MyHeaders [MyResource])
```

The range is then provided to the corresponding handler as a `Maybe MyRange` type and can be
used by the backend service to actually apply the given range and fetch the resources demanded
by the client. To send the response, one can leverage the `returnPage` defined in the
`HasPagination` type-class from the `getRangeField` getter. For example:

```hs
server :: Maybe MyRange -> Handler (Headers MyHeaders [MyResource])
server mrange =
  case mrange of
    Just range -> do
      -- ...
      returnPage (Just count) range xs

    Nothing ->
      -- ...
```

## Multiple Ranges

TODO
