# NEWS

## rfcip 1.0.2 (2026-04-06)

### BUG FIXES

* Fixed HTTP 500 error when using `get_sob_data()` with the `crop` parameter. The RMA server now requires zero-padded 4-digit commodity codes (e.g., `0041` instead of `41`).

## rfcip 1.0.1

### BUG FIXES

* Fixed "file name too long" error when using `get_sob_data()` with many filters (e.g., multiple years and crops). The caching system now uses MD5 hashes for long filenames while maintaining metadata to track original parameters.
* Enhanced `get_cache_info()` to display descriptions for hashed cache keys, making it easier to identify cached data.

### DEPENDENCIES

* Added `digest` package to Imports for MD5 hash generation.

## rfcip 1.0.0 (2025-08-15)

### INITIAL RELEASE

This represents the initial public release of rfcip.