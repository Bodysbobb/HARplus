# Extract Data by Dimension Patterns from SL4 or HAR Objects

Retrieves structured data from SL4 or HAR objects based on specified
dimension patterns. Supports multiple experiments and merging datasets
while maintaining structured dimension metadata.

## Usage

``` r
get_data_by_dims(
  patterns = NULL,
  ...,
  experiment_names = NULL,
  subtotal_level = FALSE,
  rename_cols = NULL,
  merge_data = FALSE,
  pattern_mix = FALSE
)
```

## Arguments

- patterns:

  Character vector. Dimension patterns to extract. Use `"ALL"` or `NULL`
  to extract all available patterns.

- ...:

  One or more SL4 or HAR data objects loaded using
  [`load_sl4x()`](https://bodysbobb.github.io/HARplus/reference/load_sl4x.md)
  or
  [`load_harx()`](https://bodysbobb.github.io/HARplus/reference/load_harx.md).

- experiment_names:

  Character vector. Names assigned to each dataset. If `NULL`, names are
  inferred.

- subtotal_level:

  Character or logical. Determines which decomposition levels to retain:

  - `"total"`: Keeps only `"TOTAL"` values.

  - `"decomposed"`: Keeps only decomposed values (excludes `"TOTAL"`).

  - `"all"`: Keeps all rows.

  - `TRUE`: Equivalent to `"all"`, retaining both `"TOTAL"` and
    decomposed values.

  - `FALSE`: Equivalent to `"total"`, keeping only `"TOTAL"` values.

- rename_cols:

  Named vector. Column name replacements (`c("old_name" = "new_name")`).

- merge_data:

  Logical. If `TRUE`, attempts to merge data across multiple
  experiments. Default is `FALSE`.

- pattern_mix:

  Logical. If `TRUE`, allows flexible pattern matching, ignoring
  dimension order. Default is `FALSE`.

## Value

A structured list of extracted data:

- If `merge_data = FALSE`, returns a named list where each element
  corresponds to an experiment.

- If `merge_data = TRUE`, returns a named list of all merged data

## Details

- Extracts variables matching specified dimension patterns.

- Allows for flexible pattern matching (`pattern_mix = TRUE`).

- Supports merging data across multiple experiments
  (`merge_data = TRUE`).

- Provides column renaming functionality (`rename_cols`).

- Handles subtotal filtering (`subtotal_level`), controlling whether
  `"TOTAL"` or decomposed values are retained.

## See also

[`get_data_by_var`](https://bodysbobb.github.io/HARplus/reference/get_data_by_var.md),
[`group_data_by_dims`](https://bodysbobb.github.io/HARplus/reference/group_data_by_dims.md)

## Author

Pattawee Puangchit

## Examples

``` r
# Import sample data:
sl4_data <- load_sl4x(
  system.file("extdata", "TAR10.sl4", package = "HARplus")
)
sl4_data1 <- load_sl4x(
  system.file("extdata", "SUBT10.sl4", package = "HARplus")
)

# Extract data for a single dimension pattern
data_single_pattern <- get_data_by_dims(
  "comm*reg",
  sl4_data
)

# Extract multiple dimension patterns
data_multiple_patterns <- get_data_by_dims(
  c("comm*reg", "REG*ACTS"),
  sl4_data
)

# Extract all dimension patterns separately from multiple datasets
data_all_patterns <- get_data_by_dims(
  NULL,
  sl4_data, sl4_data1,
  merge_data = FALSE
)

# Merge data for identical patterns across multiple datasets
data_merged_patterns <- get_data_by_dims(
  NULL,
  sl4_data, sl4_data1,
  merge_data = TRUE
)

# Merge data while allowing interchangeable dimensions (e.g., A*B = B*A)
data_pattern_mixed <- get_data_by_dims(
  NULL,
  sl4_data, sl4_data1,
  merge_data = TRUE,
  pattern_mix = TRUE
)

# Retain only "TOTAL" values
data_total_only <- get_data_by_dims(
  "comm*reg",
  sl4_data,
  subtotal_level = "total"
)
data_total_only_alt <- get_data_by_dims(
  "comm*reg",
  sl4_data,
  subtotal_level = FALSE
)

# Retain only decomposed components
data_decomposed_only <- get_data_by_dims(
  "comm*reg",
  sl4_data,
  subtotal_level = "decomposed"
)

# Retain all value levels
data_all_decomp <- get_data_by_dims(
  "comm*reg",
  sl4_data,
  subtotal_level = "all"
)
data_all_decomp_alt <- get_data_by_dims(
  "comm*reg",
  sl4_data,
  subtotal_level = TRUE
)

# Rename specific columns
data_renamed <- get_data_by_dims(
  "comm*reg",
  sl4_data,
  rename_cols = c(REG = "Region", COMM = "Commodity")
)

# Merge data with custom experiment names
data_merged_experiments <- get_data_by_dims(
  "comm*reg",
  sl4_data, sl4_data1,
  experiment_names = c("EXP1", "EXP2"),
  merge_data = TRUE
)
```
