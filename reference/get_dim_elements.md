# Get Dimension Elements from SL4 and HAR Objects

Extracts and lists unique dimension elements (e.g., `REG`, `COMM`,
`ACTS`) from one or more datasets.

## Usage

``` r
get_dim_elements(..., keep_unique = FALSE)
```

## Arguments

- ...:

  One or more structured SL4 or HAR objects containing dimension
  information.

- keep_unique:

  Logical. If `TRUE`, returns only unique dimension elements across
  inputs. Default is `FALSE`.

## Value

A data frame containing unique dimension elements.

## See also

[`get_dim_patterns`](https://bodysbobb.github.io/HARplus/reference/get_dim_patterns.md),
[`get_var_structure`](https://bodysbobb.github.io/HARplus/reference/get_var_structure.md)

## Author

Pattawee Puangchit

## Examples

``` r
# Import sample data:
sl4_data1 <- load_sl4x(system.file("extdata", "TAR10.sl4", package = "HARplus"))
sl4_data2 <- load_sl4x(system.file("extdata", "SUBT10.sl4", package = "HARplus"))


# Extract dimension elements from a single dataset
get_dim_elements(sl4_data1)
#>     DimName
#> 1      COMM
#> 2       REG
#> 3      ACTS
#> 4      ENDW
#> 5      MARG
#> 6    ENDWMS
#> 7     ENDWF
#> 8     ENDWC
#> 9     GDPEX
#> 10      DIR
#> 11     CTAX
#> 12 TECHTYPE

# Extract dimension elements from multiple datasets
get_dim_elements(sl4_data1, sl4_data2)
#>     DimName
#> 1      COMM
#> 2       REG
#> 3      ACTS
#> 4      ENDW
#> 5      MARG
#> 6    ENDWMS
#> 7     ENDWF
#> 8     ENDWC
#> 9     GDPEX
#> 10      DIR
#> 11     CTAX
#> 12 TECHTYPE
#> 13     COMM
#> 14      REG
#> 15     ACTS
#> 16     ENDW
#> 17     MARG
#> 18   ENDWMS
#> 19    ENDWF
#> 20    ENDWC
#> 21    GDPEX
#> 22      DIR
#> 23     CTAX
#> 24 TECHTYPE

# Extract unique dimension elements across datasets
get_dim_elements(sl4_data1, sl4_data2, keep_unique = TRUE)
#>     DimName
#> 1      COMM
#> 2       REG
#> 3      ACTS
#> 4      ENDW
#> 5      MARG
#> 6    ENDWMS
#> 7     ENDWF
#> 8     ENDWC
#> 9     GDPEX
#> 10      DIR
#> 11     CTAX
#> 12 TECHTYPE
```
