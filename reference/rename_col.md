# Rename Columns in a Data Frame (Internal)

A helper function that renames columns in a data frame based on a
specified mapping. Used internally in
[`get_var_structure()`](https://bodysbobb.github.io/HARplus/reference/get_var_structure.md),
[`get_data_by_dims()`](https://bodysbobb.github.io/HARplus/reference/get_data_by_dims.md),
and
[`rename_dims()`](https://bodysbobb.github.io/HARplus/reference/rename_dims.md).

## Usage

``` r
rename_col(df, rename_cols)
```

## Arguments

- df:

  A data frame containing columns to be renamed.

- rename_cols:

  A named vector where names are existing column names, and values are
  the corresponding new names.

## Value

A modified data frame with renamed columns.

## Details

- Replaces column names according to the provided `rename_cols` mapping.

- Ensures no duplicate column names by appending numerical suffixes when
  necessary.

- Helps standardize column names across SL4 and HAR datasets.

## See also

[`get_var_structure`](https://bodysbobb.github.io/HARplus/reference/get_var_structure.md),
[`get_data_by_dims`](https://bodysbobb.github.io/HARplus/reference/get_data_by_dims.md),
[`rename_dims`](https://bodysbobb.github.io/HARplus/reference/rename_dims.md)

## Author

Pattawee Puangchit
