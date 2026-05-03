# Create Calculation Configuration

Defines calculation settings for generating shock values, including
variable mapping, timeline sequence, and exclusion criteria. Used as
input for both
[`shock_calculate`](https://bodysbobb.github.io/HARplus/reference/shock_calculate.md)
and
[`shock_calculate_uniform`](https://bodysbobb.github.io/HARplus/reference/shock_calculate_uniform.md).

## Usage

``` r
create_calc_config(
  column_mapping = NULL,
  timeline = 1,
  exclude_self_trade = FALSE,
  exclusion_values = NULL
)
```

## Arguments

- column_mapping:

  Optional named vector mapping initial and target columns (e.g.,
  `c(COMM = "COMM", REG = "REG", REG.1 = "REG.1")`).

- timeline:

  Numeric or character range defining simulation periods.

  - Example: "1-5" expands to 1:5.

- exclude_self_trade:

  Logical; if TRUE, removes intra-region records. Default is FALSE.

- exclusion_values:

  Optional named list of elements to exclude from calculation.

## Value

A list containing:

- `column_mapping`: variable mapping between datasets

- `timeline`: expanded numeric sequence of simulation periods

- `exclude_self_trade`: logical flag

- `exclusion_values`: exclusion list by dimension

## Details

- Controls column alignment between initial and target datasets

- Supports numeric or range-based timeline definitions (e.g., "1-10")

- Excludes self-trade or specified region/sector pairs if configured

- Provides core metadata for shock calculation functions

## See also

[`create_initial_config`](https://bodysbobb.github.io/HARplus/reference/create_initial_config.md),
[`create_target_config`](https://bodysbobb.github.io/HARplus/reference/create_target_config.md),
[`shock_calculate`](https://bodysbobb.github.io/HARplus/reference/shock_calculate.md),
[`shock_calculate_uniform`](https://bodysbobb.github.io/HARplus/reference/shock_calculate_uniform.md)

## Author

Pattawee Puangchit

## Examples

``` r
# Example: Define Calculation Configuration
calc <- create_calc_config(
  column_mapping      = c(COMM = "COMM", REG = "REG", REG.1 = "REG.1"),
  timeline            = "1-5",
  exclude_self_trade  = TRUE
)
```
