# NEWS

## Development version

* Removed the extra memory caches from `get_sob_data()` and
  `get_insurance_plan_codes()`. Repeated forced calls now reach retrieval, and
  clearing SOB's disk cache cannot leave a hidden SOB result in memory.
* Insurance-plan lookup now uses ADM table A00460 from the package's GitHub
  release assets. Different plan filters share the same year/dataset cache.
  The existing output columns are retained; `commodity_year` reports the
  effective ADM reinsurance year. Pre-2011 requests use 2011 with a warning.
  Mixed names, codes, and abbreviations are supported, including `RP-HPE`;
  unmatched identifiers now error rather than being silently dropped.
* SOB and SOBTPU plan filters pass their requested years and refresh intent to
  ADM lookup. SOB resolves plans once per request before its yearly export loop.
* Successful SOB refreshes replace disk caches after all years are validated.
  Failed forced exports can return usable cached data with a detectable
  `rfcip_cache_fallback` warning. Cache-write failures return fresh data with an
  `rfcip_cache_write_warning`, preserving the previous usable cache. Invalid
  arguments and ADM lookup errors are not disguised as SOB download failures.
* Added `clear_rfcip_cache(function_name = "get_insurance_plan_codes")` for
  A00460 and legacy plan caches. Other ADM datasets are preserved.
* Crop-code memoisation and generic ADM cache behavior are unchanged. Reload
  the package after updating to activate the revised load hook.
* SOB exports now retry HTTP 429/502/503/504 and recognized transient transport
  failures, with at most four attempts per year and exponential waits with
  jitter. `Retry-After` seconds and HTTP dates are respected within a cumulative
  60-second sleep budget. Longer required waits stop retrieval; they are not
  shortened. Each transfer explicitly uses the R timeout option.
* Failed exports report the year, HTTP status when available, and attempt count.
  Forced calls retain the usable-cache fallback after retries. Invalid HTTP 200
  content cannot replace a valid cache, and user interruptions propagate.
  ADM/GitHub and SOBTPU downloads are outside the new retry policy.

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
