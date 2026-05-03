# Export Data to Various Formats (CSV/STATA/TEXT/RDS/XLSX)

Exports structured SL4 or HAR data to multiple file formats, including
CSV, Stata, TXT, RDS, and XLSX. Supports nested lists, automatic
subfolder creation, and multi-sheet Excel exports.

## Usage

``` r
export_data(
  data,
  output_path,
  format = "csv",
  prefix = "",
  create_subfolder = FALSE,
  multi_sheet_xlsx = FALSE,
  xlsx_filename = NULL,
  report_output = FALSE
)
```

## Arguments

- data:

  A list or data frame. The SL4 or HAR data to export.

- output_path:

  Character. The base output directory or file path.

- format:

  Character. The export format (`"csv"`, `"stata"`, `"txt"`, `"rds"`,
  `"xlsx"`). Default is `"csv"`.

- prefix:

  Character. An optional prefix added to exported file names. Default is
  `""` (empty).

- create_subfolder:

  Logical. If `TRUE`, creates a subfolder for each format. Default is
  `FALSE`.

- multi_sheet_xlsx:

  Logical. If TRUE, exports lists as multi-sheet XLSX files.

- xlsx_filename:

  An optional filename for the XLSX file (used when
  `multi_sheet_xlsx = TRUE`).

- report_output:

  Logical. If TRUE, generates an export report.

## Value

A list containing the file paths of the exported data.

## Details

- Supports exporting data in `"csv"`, `"stata"`, `"txt"`, `"rds"`, and
  `"xlsx"` formats.

- Handles nested lists and exports each data frame individually.

- Optionally creates subfolders for each format
  (`create_subfolder = TRUE`).

- Customizes file names using `prefix`.

- When multi_sheet_xlsx = TRUE, all exported data is stored in a
  **single Excel workbook**, with each dataset as a separate sheet.

&nbsp;

- If exporting to Stata (`"stata"` format), column names containing `.`
  will be replaced with `_` to ensure compatibility.

- If `multi_sheet_xlsx = TRUE`, list elements are exported as separate
  sheets in a single XLSX file.

- The function creates necessary directories if they do not exist.

## See also

[`pivot_data`](https://bodysbobb.github.io/HARplus/reference/pivot_data.md),
[`get_data_by_var`](https://bodysbobb.github.io/HARplus/reference/get_data_by_var.md),
[`get_data_by_dims`](https://bodysbobb.github.io/HARplus/reference/get_data_by_dims.md)

## Author

Pattawee Puangchit

## Examples

``` r
# \donttest{
# Import sample data:
sl4_data <- load_sl4x(system.file("extdata", "TAR10.sl4", package = "HARplus"))

# Extract data
data_multiple <- get_data_by_var(c("qo", "pca"), sl4_data)

# Export
export_data(data_multiple, file.path(tempdir(), "output_directory"), 
           format = c("csv", "xlsx", "stata", "txt", "rds"),
           create_subfolder = TRUE,
           multi_sheet_xlsx = TRUE)
#> Exported 2 file(s) to csv format in /tmp/RtmpLK60cu/output_directory/csv
#> Exported multi-sheet Excel file 'output_directory.xlsx' to /tmp/RtmpLK60cu/output_directory/xlsx
#> Exported 2 file(s) to stata format in /tmp/RtmpLK60cu/output_directory/stata
#> Exported 2 file(s) to txt format in /tmp/RtmpLK60cu/output_directory/txt
#> Exported 2 file(s) to rds format in /tmp/RtmpLK60cu/output_directory/rds
# }
```
