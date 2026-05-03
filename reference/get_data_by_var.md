# Extract Variable Data from SL4 or HAR Objects

Extracts structured data for one or more variables from SL4 or HAR
objects, transforming array-like data into a tidy format.

## Usage

``` r
get_data_by_var(
  var_names = NULL,
  ...,
  experiment_names = NULL,
  subtotal_level = FALSE,
  rename_cols = NULL,
  merge_data = FALSE
)
```

## Arguments

- var_names:

  Character vector. Variable names to extract. Use `"ALL"` or `NULL` to
  extract all available variables.

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

## Value

A list of structured data:

- If `merge_data = FALSE`, returns a named list where each element
  corresponds to an experiment.

- If `merge_data = TRUE`, returns a named list of all merged data

## Details

- Retrieves specific variables, multiple variables, or all available
  variables from SL4 or HAR datasets.

- Supports merging data from multiple experiments (`merge_data = TRUE`).

- Allows renaming of column names (`rename_cols`).

- Handles subtotal filtering (`subtotal_level`), controlling whether
  `"TOTAL"` or decomposed values are retained.

## See also

[`get_data_by_dims`](https://bodysbobb.github.io/HARplus/reference/get_data_by_dims.md),
,
[`group_data_by_dims`](https://bodysbobb.github.io/HARplus/reference/group_data_by_dims.md),
[`load_sl4x`](https://bodysbobb.github.io/HARplus/reference/load_sl4x.md),
[`load_harx`](https://bodysbobb.github.io/HARplus/reference/load_harx.md)

## Author

Pattawee Puangchit

## Examples

``` r
# Import sample data:
sl4_data <- load_sl4x(system.file("extdata", "TAR10.sl4", package = "HARplus"))
sl4_data1 <- load_sl4x(system.file("extdata", "SUBT10.sl4", package = "HARplus"))

# Extract a single variable
data_qo <- get_data_by_var("qo", sl4_data)

# Extract multiple variables
data_multiple <- get_data_by_var(c("qo", "qgdp"), sl4_data)

# Extract all variables separately from multiple datasets
data_all <- get_data_by_var(NULL, sl4_data, sl4_data1, merge_data = FALSE)

# Merge variable data across multiple datasets
data_merged <- get_data_by_var(NULL, sl4_data, sl4_data1, merge_data = TRUE)

# Retain only "TOTAL" values, removing decomposed components (subtotal_level = "total" or FALSE)
data_total_only <- get_data_by_var("qo", sl4_data, subtotal_level = "total")
data_total_only_alt <- get_data_by_var("qo", sl4_data, subtotal_level = FALSE)

# Retain only decomposed components, removing "TOTAL" (subtotal_level = "decomposed")
data_decomposed_only <- get_data_by_var("qo", sl4_data, subtotal_level = "decomposed")

# Retain all value levels (subtotal_level = "all" or TRUE)
data_all_decomp <- get_data_by_var("qo", sl4_data, subtotal_level = "all")
data_all_decomp_alt <- get_data_by_var("qo", sl4_data, subtotal_level = TRUE)

# Rename specific columns
data_renamed <- get_data_by_var("qo", sl4_data, rename_cols = c(REG = "Region", COMM = "Commodity"))

# Merge data across multiple datasets with custom experiment names
data_merged_experiments <- get_data_by_var("qo", sl4_data, sl4_data1,
experiment_names = c("EXP1", "EXP2"),
merge_data = TRUE)
```
