# Pivot Data from SL4 or HAR Objects

Transforms long-format SL4 or HAR data into wide format by pivoting
selected columns. Supports both single data frames and nested lists.

## Usage

``` r
pivot_data(data_obj, pivot_cols, name_repair = "unique")
```

## Arguments

- data_obj:

  A list or data frame. The SL4 or HAR data to pivot.

- pivot_cols:

  Character vector. Column names to use as pivot keys.

- name_repair:

  Character. Method for handling duplicate column names (`"unique"`,
  `"minimal"`, `"universal"`). Default is `"unique"`.

## Value

A transformed data object where the specified `pivot_cols` are pivoted
into wide format.

## Details

- Uses
  [`tidyr::pivot_wider()`](https://tidyr.tidyverse.org/reference/pivot_wider.html)
  internally to reshape data.

- Allows multiple columns to be pivoted simultaneously.

- Recursively processes nested lists, ensuring all data frames are
  transformed.

## See also

[`get_data_by_var`](https://bodysbobb.github.io/HARplus/reference/get_data_by_var.md),
[`get_data_by_dims`](https://bodysbobb.github.io/HARplus/reference/get_data_by_dims.md)

## Author

Pattawee Puangchit

## Examples

``` r
# Import sample data:
sl4_data <- load_sl4x(system.file("extdata", "TAR10.sl4", package = "HARplus"))

# Extract multiple variables
data_multiple <- get_data_by_var(c("qo", "qxs"), sl4_data)

# Pivot a single column
pivoted_data <- pivot_data(data_multiple, pivot_cols = "REG")

# Pivot multiple columns
pivoted_data_multi <- pivot_data(data_multiple, pivot_cols = c("REG", "COMM"))
```
