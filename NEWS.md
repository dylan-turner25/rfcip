# NEWS

## rfcip 1.0.1 (development version)

### BUG FIXES

* Fixed "file name too long" error when using `get_sob_data()` with many filters (e.g., multiple years and crops). The caching system now uses MD5 hashes for long filenames while maintaining metadata to track original parameters.
* Enhanced `get_cache_info()` to display descriptions for hashed cache keys, making it easier to identify cached data.

### DEPENDENCIES

* Added `digest` package to Imports for MD5 hash generation.

## rfcip 1.0.0 (2025-08-15)

### INITIAL RELEASE

This represents the initial public release of rfcip.