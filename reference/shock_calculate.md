# Calculate Shocks from Initial and Target Values

Computes compounded GEMPACK-style percentage shocks between initial and
target values, producing multi-period shock series for dynamic
simulation models. The function automatically aligns dimensions across
datasets and exports results in HAR format.

## Usage

``` r
shock_calculate(
  initial_config,
  target_config,
  calc_config,
  output_path,
  long_desc = "Calculated shock values",
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

  - Input path, file format, and variable header for the initial dataset

  - Column name of the initial value field ("Value_ini")

- target_config:

  A list created by
  [`create_target_config`](https://bodysbobb.github.io/HARplus/reference/create_target_config.md),
  defining:

  - Path, format, and variable header for the target dataset

  - Target value column ("Value_tar") or numeric target for uniform
    shock

- calc_config:

  A list created by
  [`create_calc_config`](https://bodysbobb.github.io/HARplus/reference/create_calc_config.md),
  specifying:

  - `column_mapping`: mapping between initial and target columns

  - `timeline`: sequence of years or periods (e.g., "1-5")

  - `exclude_self_trade`: logical, whether to drop self-pairs

  - `exclusion_values`: list of region/sector values to omit

- output_path:

  Path to the output HAR file where calculated shocks will be written.

- long_desc:

  Optional text for header description. Default is "Calculated shock
  values".

- dim_order:

  Optional dimension ordering specification. Can be:

  - NULL (default): alphabetical A-Z ordering

  - a named list defining order for each dimension (e.g., REG, COMM)

  - a data frame or path to Excel/CSV file containing order definitions

- lowercase:

  Logical; if TRUE, converts dimension elements to lowercase. Default is
  FALSE.

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

- `output_path`: normalized path to output HAR file

- `baseline_path`: normalized path to baseline file (if new_baseline =
  TRUE)

## Details

- Computes percentage shocks using compounded "power of tax" formula

- Supports multiple periods defined via `timeline` configuration

- Compatible with HAR, SL4, CSV, or XLSX input formats

- Excludes self-trade or specified region-sector pairs when configured

- Exports results as multi-header HAR file (one header per timeline
  period)

- Optionally generates new baseline rate file showing post-adjustment
  values

## See also

[`shock_calculate_uniform`](https://bodysbobb.github.io/HARplus/reference/shock_calculate_uniform.md),
[`create_initial_config`](https://bodysbobb.github.io/HARplus/reference/create_initial_config.md),
[`create_target_config`](https://bodysbobb.github.io/HARplus/reference/create_target_config.md),
[`create_calc_config`](https://bodysbobb.github.io/HARplus/reference/create_calc_config.md),
[`save_har`](https://bodysbobb.github.io/HARplus/reference/save_har.md)

## Author

Pattawee Puangchit

## Examples

``` r
# Example 1: Target-Based Shock Calculation with New Baseline
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

# Target File
target <- create_target_config(
  path   = har_path,
  type   = "har",
  header = "rTMS"
)

# Calculation Setup with Column Mapping
calc <- create_calc_config(
  column_mapping      = c(TRAD_COMM = "TRAD_COMM", REG = "REG", REG.1 = "REG.1"),
  timeline            = "1-5",
  exclude_self_trade  = TRUE
)

# Compute Shock Based on Initial and Target Values
# Also generate new baseline file
shock_calculate(
  initial_config = initial,
  target_config  = target,
  calc_config    = calc,
  output_path    = file.path(tempdir(), "output_target.har"),
  dim_order      = mapping,
  new_baseline   = TRUE
)
#> Doing sparse: ONEY
#> ONEY with maxsize 2e+06
#> Doing sparse: TWOY
#> TWOY with maxsize 2e+06
#> Doing sparse: THRY
#> THRY with maxsize 2e+06
#> Doing sparse: FOUR
#> FOUR with maxsize 2e+06
#> Doing sparse: FIVE
#> FIVE with maxsize 2e+06
#> 
#> Successfully wrote 7 header(s) to HAR file
#>   Set headers (1C type): 2
#>   Mapping headers (1C type): 0
#>   Data headers (RE/2I type): 5
#> 
#> Dimension ordering applied:
#>   REG: 3 prioritized values, remaining A-Z
#> 
#> Output file: /tmp/Rtmpp9aRk7/output_target.har
#> File size: 2,719 bytes
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
#> Output file: /tmp/Rtmpp9aRk7/output_target_baseline.har
#> File size: 935 bytes
#> 
#> 
#> Generated new baseline file: /tmp/Rtmpp9aRk7/output_target_baseline.har
```
