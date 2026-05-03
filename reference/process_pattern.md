# Extract and Process Pattern-Matched Variables (Internal)

A helper function that extracts and processes variables matching a
specified pattern within an SL4 or HAR data object. Used internally in
[`get_data_by_dims()`](https://bodysbobb.github.io/HARplus/reference/get_data_by_dims.md).

## Usage

``` r
process_pattern(pattern, data_obj, exp_name, pattern_mix = FALSE)
```

## Arguments

- pattern:

  Character. The pattern to match against dimension structures.

- data_obj:

  An SL4 or HAR object containing dimension information and data.

- exp_name:

  Character. The experiment name assigned to the extracted data.

- pattern_mix:

  Logical. If `TRUE`, allows pattern matching to ignore dimension order.

## Value

A data frame containing processed data for the matching pattern, or
`NULL` if no matches are found.

## Details

- Searches for variables whose dimension structures match the given
  pattern.

- Supports flexible pattern matching when `pattern_mix = TRUE`, allowing
  dimension order to vary.

- Extracted data is converted into a tidy format, preserving dimension
  structures.

- Standardizes `"Subtotal"` column naming for consistency across
  datasets.

- Ensures only non-empty extracted variables are retained.

## See also

[`get_original_pattern`](https://bodysbobb.github.io/HARplus/reference/get_original_pattern.md),
[`pattern_match`](https://bodysbobb.github.io/HARplus/reference/pattern_match.md),
[`get_data_by_dims`](https://bodysbobb.github.io/HARplus/reference/get_data_by_dims.md)

## Author

Pattawee Puangchit
