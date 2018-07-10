# Changelog

## v2.1.1 (2018-07-10)

- Allow servant 0.14


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
