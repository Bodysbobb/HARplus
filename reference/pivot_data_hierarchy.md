# Create Hierarchical Pivot Table from SL4 or HAR Objects

Creates hierarchical pivot tables from structured SL4 or HAR data, with
optional Excel export. Supports both single data frames and nested
lists, preserving dimension hierarchies.

## Usage

``` r
pivot_data_hierarchy(
  data_obj,
  pivot_cols,
  name_repair = "unique",
  export = FALSE,
  file_path = NULL,
  xlsx_filename = NULL
)
```

## Arguments

- data_obj:

  A list or data frame. The SL4 or HAR data to pivot.

- pivot_cols:

  Character vector. Column names to use as pivot keys in order of
  hierarchy.

- name_repair:

  Character. Method for handling duplicate column names (`"unique"`,
  `"minimal"`, `"universal"`). Default is `"unique"`.

- export:

  Logical. If `TRUE`, exports result to Excel. Default is `FALSE`.

- file_path:

  Character. Required if export = TRUE. The path for Excel export.

- xlsx_filename:

  Character. The name for the Excel file when using multi_sheet_xlsx. If
  NULL, uses the name of the dataset. Default is `NULL`.

## Value

A pivoted data object with hierarchical structure:

- If input is a data frame: Returns a hierarchical pivot table.

- If input is a list: Returns a nested list of hierarchical pivot
  tables.

- If export = TRUE: Invisibly returns the pivoted object after Excel
  export.

## Details

- Transforms data into hierarchical pivot format with nested column
  headers.

- Supports multiple pivot columns in specified order (e.g., REG \>
  COMM).

- Handles both single data frames and nested list structures.

- Optional direct export to Excel with formatted hierarchical headers.

- Uses efficient data processing with tidyr and openxlsx.

## See also

[`pivot_data`](https://bodysbobb.github.io/HARplus/reference/pivot_data.md)

## Author

Pattawee Puangchit

## Examples

``` r
# \donttest{
# Import sample data:
sl4_data <- load_sl4x(system.file("extdata", "TAR10.sl4", package = "HARplus"))

# Extract data
data_multiple <- get_data_by_var(c("qo", "pca"), sl4_data)

# Create hierarchical pivot without export
pivot_hier <- pivot_data_hierarchy(data_multiple, 
                                   pivot_cols = c("REG", "COMM"))

# Create and export to Excel in one step
pivot_export <- pivot_data_hierarchy(data_multiple, 
                                     pivot_cols = c("REG", "COMM"),
                                     export = TRUE,
                                     file_path = file.path(tempdir(), "pivot_output.xlsx"))
#> Exported hierarchical pivot Excel file '.xlsx' to /tmp/RtmpLK60cu
# }
```
