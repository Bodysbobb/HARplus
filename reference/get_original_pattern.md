# Retrieve the Original Dimension Pattern (Internal)

A helper function that finds the original dimension pattern name in an
SL4 or HAR dataset that matches a given pattern. Used internally in
[`get_data_by_dims()`](https://bodysbobb.github.io/HARplus/reference/get_data_by_dims.md).

## Usage

``` r
get_original_pattern(pattern, data_obj, mix_patterns = FALSE)
```

## Arguments

- pattern:

  Character. The pattern to search for in dimension structures.

- data_obj:

  An SL4 or HAR object containing dimension information.

- mix_patterns:

  Logical. If `TRUE`, allows dimension order to vary when matching
  patterns.

## Value

The original dimension pattern name as a character string, or `NULL` if
no match is found.

## Details

- Performs a case-insensitive comparison to identify matching dimension
  patterns.

- Supports flexible pattern matching when `mix_patterns = TRUE`,
  allowing dimension order to vary.

- Returns the standardized dimension pattern name as stored in the
  dataset.

## See also

[`pattern_match`](https://bodysbobb.github.io/HARplus/reference/pattern_match.md),
[`process_pattern`](https://bodysbobb.github.io/HARplus/reference/process_pattern.md),
[`get_data_by_dims`](https://bodysbobb.github.io/HARplus/reference/get_data_by_dims.md)

## Author

Pattawee Puangchit
