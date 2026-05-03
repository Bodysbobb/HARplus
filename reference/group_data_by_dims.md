# Group Data by Dimension Patterns in SL4 or HAR Objects

Groups extracted SL4 or HAR data based on specified dimension structures
and priority rules. Supports automatic renaming, merging, subtotal
filtering, and structured metadata handling.

## Usage

``` r
group_data_by_dims(
  patterns = NULL,
  ...,
  priority,
  rename_cols = NULL,
  experiment_names = NULL,
  subtotal_level = FALSE,
  auto_rename = FALSE
)
```

## Arguments

- patterns:

  Character vector. Dimension patterns to extract. Use `"ALL"` or `NULL`
  to extract all available patterns.

- ...:

  One or more SL4 or HAR objects loaded using
  [`load_sl4x()`](https://bodysbobb.github.io/HARplus/reference/load_sl4x.md)
  or
  [`load_harx()`](https://bodysbobb.github.io/HARplus/reference/load_harx.md).

- priority:

  Named list. Specifies priority dimension elements
  (`c("group_name" = c("dim1", "dim2"))`).

- rename_cols:

  Named vector. Column name replacements (`c("old_name" = "new_name")`).

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

- auto_rename:

  Logical. If `TRUE`, automatically renames dimensions for consistency.
  Default is `FALSE`.

## Value

A structured list of grouped data:

- A named list where each element corresponds to a dimension size group
  (e.g., "2D", "3D").

- Each group contains dimension-grouped data based on priority rules.

- If unmerged data exists, includes a report attribute detailing merge
  issues.

## Details

- Groups extracted variables based on dimension elements.

- Applies predefined priority rules to structure the data.

- Allows automatic renaming of dimensions (`auto_rename = TRUE`).

- Supports merging of grouped data across multiple experiments.

- Handles subtotal filtering (`subtotal_level`), controlling whether
  `"TOTAL"` or decomposed values are retained.

## See also

[`get_data_by_dims`](https://bodysbobb.github.io/HARplus/reference/get_data_by_dims.md),
[`get_data_by_var`](https://bodysbobb.github.io/HARplus/reference/get_data_by_var.md),
[`load_sl4x`](https://bodysbobb.github.io/HARplus/reference/load_sl4x.md),
[`load_harx`](https://bodysbobb.github.io/HARplus/reference/load_harx.md)

## Author

Pattawee Puangchit

## Examples

``` r
# Import sample data
sl4_data1 <- load_sl4x(system.file("extdata", "TAR10.sl4", package = "HARplus"))
sl4_data2 <- load_sl4x(system.file("extdata", "SUBT10.sl4", package = "HARplus"))

# Case 1: Multiple priority levels (Sector then Region) with auto_rename
priority_list <- list(
  "Sector" = c("COMM", "ACTS"),
  "Region" = c("REG")
)
grouped_data_multiple <- group_data_by_dims(
  patterns = "ALL",
  sl4_data1,
  priority = priority_list,
  auto_rename = TRUE
)

# Case 2: Single priority (Region only) with auto_rename
priority_list <- list("Region" = c("REG"))
grouped_data_single <- group_data_by_dims(
  patterns = "ALL",
  sl4_data1, sl4_data2,
  priority = priority_list,
  auto_rename = TRUE
)

# Case 3: Multiple priorities without auto_rename
priority_list <- list(
  "Sector" = c("COMM", "ACTS"),
  "Region" = c("REG")
)
grouped_data_no_rename <- group_data_by_dims(
  patterns = "ALL",
  sl4_data1,
  priority = priority_list,
  auto_rename = FALSE
)
```
