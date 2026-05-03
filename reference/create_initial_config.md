# Create Initial Value Configuration

Defines the configuration for loading the initial dataset, including
path, format, variable header, and value column name. Used as input for
[`shock_calculate`](https://bodysbobb.github.io/HARplus/reference/shock_calculate.md)
and
[`shock_calculate_uniform`](https://bodysbobb.github.io/HARplus/reference/shock_calculate_uniform.md).

## Usage

``` r
create_initial_config(path, format, header, value_col = "Value")
```

## Arguments

- path:

  Path to the initial data file.

- format:

  File format of the initial dataset. Must be "har" or "sl4".

- header:

  Header name within the HAR or SL4 file to extract.

- value_col:

  Name of the column containing numeric values. Default is "Value".

## Value

A list containing:

- `path`: input file path

- `format`: file format ("har" or "sl4")

- `header`: target header name

- `value_col`: column name for numeric values

## Details

- Supports `HAR` and `SL4` file formats

- Specifies the header name to extract from the dataset

- Allows custom naming for the value column (`Value` by default)

## See also

[`create_target_config`](https://bodysbobb.github.io/HARplus/reference/create_target_config.md),
[`create_calc_config`](https://bodysbobb.github.io/HARplus/reference/create_calc_config.md),
[`shock_calculate`](https://bodysbobb.github.io/HARplus/reference/shock_calculate.md),
[`shock_calculate_uniform`](https://bodysbobb.github.io/HARplus/reference/shock_calculate_uniform.md)

## Author

Pattawee Puangchit

## Examples

``` r
# Example: Define Initial Configuration
initial <- create_initial_config(
  path   = "D:/Data/taxrates_2017.har",
  format = "har",
  header = "rTMS"
)
```
