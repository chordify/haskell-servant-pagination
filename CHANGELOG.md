# Changelog

## v2.3.0 (2020-03-05)

- Allow server-0.17 in executables
- Bump stack LTS to 14.25

## v2.2.2 (2019-02-28)

- (oversight) Allow server-0.16 in executables


## v2.2.1 (2019-02-28)

- Allow server-0.16


## v2.2.0 (2019-01-28)

- Use URL encoding for range values to properly support strings.


## v2.1.3 (2018-11-14)

- Fix Haddock generation


## v2.1.2 (2018-11-13)

- Allow servant-0.15


## v2.1.1 (2018-07-10)

- Allow servant-0.14


## v2.1.0 (2018-04-16) 

- Add some tests (QuickCheck round-up & control some Ranges parsing)
- Add `Show` and `Eq` instances for Ranges
- Expose `putRange` function 
- Review `getDefaultRange` signature (remove Maybe argument)


## v2.0.0 (2018-04-06)

- Review internal implementation and public API (ditch Range combinator to favor type-level
  list and more discrete footprint). 

- Remove 'Total-Count' header, can still be added on top of the range headers but isn't a Range
  header so to speak. 

- Extend haddock documentation to be more user-friendly


## v1.0.0 (2018-02-06)

- Initial release
