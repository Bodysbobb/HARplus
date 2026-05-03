# Calculate Shocks with Uniform Adjustment

Generates GEMPACK-style percentage shocks using a uniform proportional
adjustment applied to all values in the initial dataset. The function
supports additive or multiplicative adjustments, single or multi-period
configurations, and direct HAR export.

## Usage

``` r
shock_calculate_uniform(
  initial_config,
  adjustment_value,
  calculation_method = "*",
  calc_config,
  output_path,
  long_desc = "Uniform shock adjustment",
  dim_order = NULL,
  lowercase = FALSE,
  new_baseline = FALSE
)
```

## Arguments

- initial_config:

  A list created by
  [`create_initial_config`](https://bodysbobb.github.io/HARplus/reference/create_initial_config.md),
  defining:

  - Input file path, format, and variable header

  - Column name for initial rate values ("Value_ini")

- adjustment_value:

  Numeric scalar specifying the uniform adjustment to apply.

  - Interpreted according to `calculation_method`

  - For example, 0.5 with method "\*" halves the base rate

- calculation_method:

  Operator defining the adjustment method:

  - "\*" multiply (default)

  - "/" divide

  - "+" add

  - "-" subtract

- calc_config:

  A list created by
  [`create_calc_config`](https://bodysbobb.github.io/HARplus/reference/create_calc_config.md),
  specifying:

  - `timeline`: sequence of simulation periods (e.g., "1-10")

  - `exclude_self_trade`: logical, whether to omit intra-region pairs

  - `exclusion_values`: optional list defining excluded elements

- output_path:

  Path to the output HAR file where calculated shocks will be written.

- long_desc:

  Optional text for header description. Default is "Uniform shock
  adjustment".

- dim_order:

  Optional dimension ordering specification. Can be:

  - NULL (default): alphabetical A-Z ordering

  - a named list defining preferred order per dimension

  - a data frame or path to Excel/CSV with explicit order definitions

- lowercase:

  Logical; if TRUE, converts all dimension elements to lowercase.
  Default is FALSE.

- new_baseline:

  Logical; if TRUE, generates an additional HAR file with the new
  baseline rates (target values where available, initial values
  otherwise). The output file will have "\_baseline" appended to the
  filename. Default is FALSE.

## Value

Invisibly returns a list containing summary metadata:

- `n_observations`: total records processed

- `n_included`: records included in shock computation

- `n_excluded`: records excluded by configuration

- `output_path`: normalized path to the generated HAR file

- `baseline_path`: normalized path to baseline file (if new_baseline =
  TRUE)

## Details

- Applies uniform adjustment to all base rates across defined dimensions

- Supports additive ("+", "-") and proportional ("\*", "/") adjustments

- Computes compounded shocks following the "power of tax" formulation

- Handles multiple time periods as defined by `timeline` in
  `calc_config`

- Excludes self-trade or specified region/sector pairs if configured

- Outputs results as multi-header HAR file (one per timeline period)

- Optionally generates new baseline rate file showing post-adjustment
  values

## See also

[`shock_calculate`](https://bodysbobb.github.io/HARplus/reference/shock_calculate.md),
[`create_initial_config`](https://bodysbobb.github.io/HARplus/reference/create_initial_config.md),
[`create_calc_config`](https://bodysbobb.github.io/HARplus/reference/create_calc_config.md),
[`save_har`](https://bodysbobb.github.io/HARplus/reference/save_har.md)

## Author

Pattawee Puangchit

## Examples

``` r
# Example 1: Uniform Shock (50% Reduction) with New Baseline
har_path <- system.file("extdata", "baserate.har", package = "HARplus")

# Sorting Column
mapping <- list(
  REG = c("USA", "EU", "ROW")
)

# Initial File
initial <- create_initial_config(
  path   = har_path,
  format = "har",
  header = "rTMS"
)

# Calculation Setup
calc <- create_calc_config(
  timeline           = "1-10",
  exclude_self_trade = TRUE
)

# Compute Uniform 50% Reduction (Value_tar = Value_ini * 0.5)
# Also generate new baseline file showing the adjusted rates
shock_calculate_uniform(
  initial_config     = initial,
  adjustment_value   = 0.5,
  calculation_method = "*",
  calc_config        = calc,
  output_path        = file.path(tempdir(), "output_uniform.har"),
  dim_order          = mapping,
  new_baseline       = TRUE
)
#> ONEY with maxsize 2e+06
#> TWOY with maxsize 2e+06
#> THRY with maxsize 2e+06
#> FOUR with maxsize 2e+06
#> FIVE with maxsize 2e+06
#> SIXY with maxsize 2e+06
#> SEVN with maxsize 2e+06
#> EIGT with maxsize 2e+06
#> NINE with maxsize 2e+06
#> TENY with maxsize 2e+06
#> 
#> Successfully wrote 12 header(s) to HAR file
#>   Set headers (1C type): 2
#>   Mapping headers (1C type): 0
#>   Data headers (RE/2I type): 10
#> 
#> Dimension ordering applied:
#>   REG: 3 prioritized values, remaining A-Z
#> 
#> Output file: /tmp/Rtmpp9aRk7/output_uniform.har
#> File size: 6,254 bytes
#> 
#> RTMS with maxsize 2e+06
#> 
#> Successfully wrote 3 header(s) to HAR file
#>   Set headers (1C type): 2
#>   Mapping headers (1C type): 0
#>   Data headers (RE/2I type): 1
#> 
#> Dimension ordering applied:
#>   REG: 3 prioritized values, remaining A-Z
#> 
#> Output file: /tmp/Rtmpp9aRk7/output_uniform_baseline.har
#> File size: 935 bytes
#> 
#> 
#> Generated new baseline file: /tmp/Rtmpp9aRk7/output_uniform_baseline.har
```
