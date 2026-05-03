# Save Data to GEMPACK HAR Format

Writes a named list of R objects to a GEMPACK-compatible HAR file,
including character sets, mapping vectors, integer matrices, numeric
arrays, sparse numeric arrays, and data frames reshaped to arrays.

## Usage

``` r
save_har(
  data_list,
  file_path,
  dimensions = NULL,
  value_cols = NULL,
  header_type = NULL,
  mappings = NULL,
  long_desc = NULL,
  coefficients = NULL,
  export_sets = TRUE,
  lowercase = TRUE,
  dim_order = NULL,
  dim_rename = NULL,
  force_sparse = NULL,
  max_chunk = 2e+06
)
```

## Arguments

- data_list:

  Named list of objects to write. List names are used as HAR header
  names after conversion to uppercase and truncation to four characters.

- file_path:

  Character string giving the output HAR file path.

- dimensions:

  Optional named list. For data-frame inputs, each element gives the
  columns used as array dimensions.

- value_cols:

  Optional named list or named character vector. For data-frame inputs,
  each element gives the numeric value column. Defaults to `"Value"`
  when omitted.

- header_type:

  Optional named list or named character vector giving explicit header
  roles. Accepted values are `"auto"`, `"set"`, `"mapping"`, `"real"`,
  `"sparse"`, and `"integer"`.

- mappings:

  Optional named list. Each element must be
  `c(source_set, destination_set)` for the corresponding mapping header.

- long_desc:

  Optional named list or named character vector of long header
  descriptions.

- coefficients:

  Optional named list or named character vector of coefficient names for
  numeric headers.

- export_sets:

  Logical. If `TRUE`, dimension sets from numeric arrays are written as
  character headers unless already supplied. Default is `TRUE`.

- lowercase:

  Logical. If `TRUE`, character elements and dimension values are
  converted to lowercase during data-frame and ordering processing.
  Default is `TRUE`.

- dim_order:

  Optional dimension-ordering specification. Can be `NULL`, a data
  frame, a named list, or a path to a CSV or Excel file.

- dim_rename:

  Optional named list for renaming array dimensions in the HAR output.

- force_sparse:

  Optional character vector of headers to write in sparse numeric
  format.

- max_chunk:

  Integer. Maximum number of elements per dense numeric data chunk.
  Default is `2e6`.

## Value

Invisibly returns a list containing the output path, written headers,
and counts of set, data, and mapping headers.

## See also

[`load_harx`](https://bodysbobb.github.io/HARplus/reference/load_harx.md),
[`load_sl4x`](https://bodysbobb.github.io/HARplus/reference/load_sl4x.md)

## Author

Pattawee Puangchit

## Examples

``` r
# Example 1: Save one numeric data frame
REG <- c("USA", "EU", "ROW")
COLUMN <- c("alloc_A1", "tot_E1")
WELF <- expand.grid(REG = REG, COLUMN = COLUMN, stringsAsFactors = FALSE)
WELF$Value <- seq_len(nrow(WELF))

save_har(
  data_list = list(WELF = WELF),
  file_path = file.path(tempdir(), "output_single.har"),
  dimensions = list(WELF = c("REG", "COLUMN")),
  value_cols = list(WELF = "Value"),
  long_desc = list(WELF = "Welfare Decomposition"),
  coefficients = list(WELF = "WELF"),
  export_sets = TRUE,
  lowercase = FALSE
)
#> WELF with maxsize 2e+06
#> 
#> Successfully wrote 3 header(s) to HAR file
#>   Set headers (1C type): 2
#>   Mapping headers (1C type): 0
#>   Data headers (RE/2I type): 1
#> 
#> All dimensions sorted A-Z (no custom mapping provided)
#> 
#> Output file: /tmp/Rtmpp9aRk7/output_single.har
#> File size: 810 bytes
#> 

# Example 2: Save multiple numeric data frames
DECOM <- expand.grid(REG = REG, ALLOCEFF = c("A1", "A2"), stringsAsFactors = FALSE)
DECOM$Value <- seq_len(nrow(DECOM))

save_har(
  data_list = list(WELF = WELF, DECOM = DECOM),
  file_path = file.path(tempdir(), "output_multi.har"),
  dimensions = list(
    WELF = c("REG", "COLUMN"),
    DECOM = c("REG", "ALLOCEFF")
  ),
  value_cols = list(
    WELF = "Value",
    DECOM = "Value"
  ),
  long_desc = list(
    WELF = "Welfare Decomposition",
    DECOM = "Allocative efficiency effect"
  ),
  coefficients = list(
    WELF = "WELF",
    DECOM = "DECOM"
  ),
  export_sets = TRUE,
  lowercase = FALSE
)
#> WELF with maxsize 2e+06
#> DECO with maxsize 2e+06
#> 
#> Successfully wrote 5 header(s) to HAR file
#>   Set headers (1C type): 3
#>   Mapping headers (1C type): 0
#>   Data headers (RE/2I type): 2
#> 
#> All dimensions sorted A-Z (no custom mapping provided)
#> 
#> Output file: /tmp/Rtmpp9aRk7/output_multi.har
#> File size: 1,448 bytes
#> 

# Example 3: Save a mapping vector
SC <- c("AF", "AP", "BA")
GSEC <- c("OCR", "V_F", "GRO")
MASC <- c(AF = "OCR", AP = "V_F", BA = "GRO")

save_har(
  data_list = list(SC = SC, GSEC = GSEC, MASC = MASC),
  file_path = file.path(tempdir(), "mapping.har"),
  mappings = list(MASC = c("SC", "GSEC")),
  long_desc = list(
    SC = "Set SC",
    GSEC = "Set GSEC",
    MASC = "Mapping SC to GSEC"
  ),
  export_sets = FALSE,
  lowercase = FALSE
)
#> 
#> Successfully wrote 3 header(s) to HAR file
#>   Set headers (1C type): 2
#>   Mapping headers (1C type): 1
#>   Data headers (RE/2I type): 0
#> 
#> Output file: /tmp/Rtmpp9aRk7/mapping.har
#> File size: 516 bytes
#> 

# Example 4: Save mixed headers
CODE <- matrix(as.integer(c(1, 2, 3, 4)), nrow = 2)
TAX <- expand.grid(SC = SC, REG = REG, stringsAsFactors = FALSE)
TAX$Value <- c(0, 0, 0, 1.2, 0, 0, 0, 0, 2.5)

save_har(
  data_list = list(
    WELF = WELF,
    REG = REG,
    SC = SC,
    GSEC = GSEC,
    MASC = MASC,
    TAX = TAX,
    CODE = CODE
  ),
  file_path = file.path(tempdir(), "output_mixed.har"),
  dimensions = list(
    WELF = c("REG", "COLUMN"),
    TAX = c("SC", "REG")
  ),
  value_cols = list(
    WELF = "Value",
    TAX = "Value"
  ),
  mappings = list(
    MASC = c("SC", "GSEC")
  ),
  long_desc = list(
    WELF = "Welfare Decomposition",
    REG = "Set REG",
    SC = "Set SC",
    GSEC = "Set GSEC",
    MASC = "Mapping SC to GSEC",
    TAX = "Sparse tax example",
    CODE = "Integer matrix example"
  ),
  coefficients = list(
    WELF = "WELF",
    TAX = "TAX"
  ),
  force_sparse = "TAX",
  export_sets = FALSE,
  lowercase = FALSE
)
#> WELF with maxsize 2e+06
#> TAX with maxsize 2e+06
#> 
#> Successfully wrote 7 header(s) to HAR file
#>   Set headers (1C type): 3
#>   Mapping headers (1C type): 1
#>   Data headers (RE/2I type): 3
#> 
#> All dimensions sorted A-Z (no custom mapping provided)
#> 
#> Output file: /tmp/Rtmpp9aRk7/output_mixed.har
#> File size: 1,808 bytes
#> 

# Example 5: Apply custom dimension ordering
dim_order <- list(
  REG = c("ROW", "USA", "EU"),
  COLUMN = c("tot_E1", "alloc_A1")
)

save_har(
  data_list = list(WELF = WELF),
  file_path = file.path(tempdir(), "output_sorted.har"),
  dimensions = list(WELF = c("REG", "COLUMN")),
  value_cols = list(WELF = "Value"),
  long_desc = list(WELF = "Welfare Decomposition"),
  coefficients = list(WELF = "WELF"),
  export_sets = TRUE,
  dim_order = dim_order,
  lowercase = FALSE
)
#> WELF with maxsize 2e+06
#> 
#> Successfully wrote 3 header(s) to HAR file
#>   Set headers (1C type): 2
#>   Mapping headers (1C type): 0
#>   Data headers (RE/2I type): 1
#> 
#> Dimension ordering applied:
#>   REG: 3 prioritized values, remaining A-Z
#>   COLUMN: 2 prioritized values, remaining A-Z
#> 
#> Output file: /tmp/Rtmpp9aRk7/output_sorted.har
#> File size: 810 bytes
#> 
```
