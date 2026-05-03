# Extract and Organize Dimension Metadata (Internal)

A helper function that extracts and structures dimension-related
metadata from a given dimension structure. Used internally in
[`get_var_structure()`](https://bodysbobb.github.io/HARplus/reference/get_var_structure.md)
and
[`compare_var_structure()`](https://bodysbobb.github.io/HARplus/reference/compare_var_structure.md).

## Usage

``` r
get_dim_info(dim_info)
```

## Arguments

- dim_info:

  A list containing dimension metadata, including:

  - `dimension_string`: A textual representation of dimensions (e.g.,
    `"REG*COMM*YEAR"`).

  - `dimension_names`: A character vector of dimension names.

  - `dimension_sizes`: A numeric vector indicating the size of each
    dimension.

## Value

A structured list containing:

- `dimension_string`: The original dimension string.

- `dim_size`: The number of dimensions.

- `data_shape`: A formatted string representing the data shape (e.g.,
  `"10x20x30"`).

- `col_size`: The product of all dimension sizes except the first,
  representing column count.

- `n_obs`: The first dimension size, typically representing the number
  of observations.

## Details

- Retrieves structured metadata for variables in SL4 and HAR datasets.

- Computes data shape and ensures consistency in dimension structures.

- Helps determine observation counts and column sizes for variable
  summaries.

## See also

[`get_var_structure`](https://bodysbobb.github.io/HARplus/reference/get_var_structure.md),
[`compare_var_structure`](https://bodysbobb.github.io/HARplus/reference/compare_var_structure.md)

## Author

Pattawee Puangchit
