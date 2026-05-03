# Match Patterns with Optional Mixing (Internal)

Compares two patterns to determine if they match, with an option to
allow flexible dimension order.

## Usage

``` r
pattern_match(pattern1, pattern2, mix_patterns = FALSE)
```

## Arguments

- pattern1:

  A character string representing the first pattern.

- pattern2:

  A character string representing the second pattern.

- mix_patterns:

  Logical; if `TRUE`, allows dimension order to be ignored during
  comparison.

## Value

Logical; `TRUE` if the patterns match, `FALSE` otherwise.

## Details

- Performs case-insensitive pattern matching.

- If `mix_patterns = TRUE`, allows patterns to match even if dimensions
  are in different order.

## See also

[`get_original_pattern`](https://bodysbobb.github.io/HARplus/reference/get_original_pattern.md),
[`process_pattern`](https://bodysbobb.github.io/HARplus/reference/process_pattern.md),
[`get_data_by_dims`](https://bodysbobb.github.io/HARplus/reference/get_data_by_dims.md)

## Author

Pattawee Puangchit
