# Process Decomposition Levels in Data Frames (Internal)

A helper function that filters data based on decomposition levels in the
`"Subtotal"` column. Used internally in
[`get_data_by_var()`](https://bodysbobb.github.io/HARplus/reference/get_data_by_var.md),
[`get_data_by_dims()`](https://bodysbobb.github.io/HARplus/reference/get_data_by_dims.md),
and
[`group_data_by_dims()`](https://bodysbobb.github.io/HARplus/reference/group_data_by_dims.md).

## Usage

``` r
process_decomp_level(df, subtotal_level)
```

## Arguments

- df:

  A data frame containing a `"Subtotal"` column.

- subtotal_level:

  Character or logical. Determines which values to retain:

  - `"total"`: Keeps only `"TOTAL"` values.

  - `"decomposed"`: Keeps only decomposed values (excludes `"TOTAL"`).

  - `"all"`: Keeps all rows.

  - `TRUE`: Equivalent to `"all"` (keeps both `"TOTAL"` and decomposed
    values).

  - `FALSE`: Equivalent to `"total"` (keeps only `"TOTAL"` values,
    removing decomposed components).

## Value

A filtered data frame based on the specified decomposition level.

## Details

- If `subtotal_level = "total"`, **keeps only `"TOTAL"` values**,
  removing all decomposed components.

- If `subtotal_level = "decomposed"`, **keeps only decomposed
  components**, removing `"TOTAL"`.

- If `subtotal_level = "all"`, **keeps both `"TOTAL"` and decomposed
  values** (no filtering).

- If `subtotal_level = TRUE` (logical), it is **equivalent to `"all"`**,
  meaning all values are kept.

- If `subtotal_level = FALSE` (logical), it is **equivalent to
  `"total"`**, meaning only `"TOTAL"` values are kept, and decomposed
  components are removed.

- Filtering is applied **only when both `"TOTAL"` and decomposed values
  exist**.

## See also

[`get_data_by_var`](https://bodysbobb.github.io/HARplus/reference/get_data_by_var.md),
[`get_data_by_dims`](https://bodysbobb.github.io/HARplus/reference/get_data_by_dims.md),
[`group_data_by_dims`](https://bodysbobb.github.io/HARplus/reference/group_data_by_dims.md)

## Author

Pattawee Puangchit
