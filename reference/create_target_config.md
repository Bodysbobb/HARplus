# Create Target Value Configuration

Defines the configuration for loading the target dataset, which
represents post-adjustment or comparative rate values. Supports multiple
file formats.

## Usage

``` r
create_target_config(
  path = NULL,
  type = NULL,
  header = NULL,
  value_col = "Value"
)
```

## Arguments

- path:

  Optional path to the target data file.

- type:

  Optional file type for the target dataset ("har", "sl4", "csv", or
  "xlsx").

- header:

  Optional header name within the HAR or SL4 file to extract.

- value_col:

  Column name containing numeric target values. Default is "Value".

## Value

A list containing:

- `path`: file path to target data

- `type`: file format (lowercase)

- `header`: header name in HAR/SL4 file

- `value_col`: column name for target values

## Details

- Supports `HAR`, `SL4`, `CSV`, and `XLSX` file formats

- Can also represent a uniform numeric target value when no file path is
  provided

- Used in combination with
  [`create_initial_config`](https://bodysbobb.github.io/HARplus/reference/create_initial_config.md)
  for shock computation

## See also

[`create_initial_config`](https://bodysbobb.github.io/HARplus/reference/create_initial_config.md),
[`create_calc_config`](https://bodysbobb.github.io/HARplus/reference/create_calc_config.md),
[`shock_calculate`](https://bodysbobb.github.io/HARplus/reference/shock_calculate.md),
[`shock_calculate_uniform`](https://bodysbobb.github.io/HARplus/reference/shock_calculate_uniform.md)

## Author

Pattawee Puangchit

## Examples

``` r
# Example: Define Target Configuration
target <- create_target_config(
  path   = "D:/Data/taxrates_2019.har",
  type   = "har",
  header = "rTMS"
)
```
