# Process and Export Report (Internal)

Generates a summary report of variables and their corresponding output
files from structured SL4 or HAR data. The report is saved as an Excel
file.

## Usage

``` r
process_export_report(data, output_path, prefix = "", data_name = "data")
```

## Arguments

- data:

  A structured SL4 or HAR object or a nested list of data frames.

- output_path:

  A character string specifying the output directory or file path.

- prefix:

  A character string to prepend to the exported filenames.

## Details

- Extracts unique variable names and their associated export filenames.

- Handles nested list structures by recursively traversing them.

- The output report is saved as an Excel file named
  `"Report_<data_name>.xlsx"`.

## See also

[`export_data`](https://bodysbobb.github.io/HARplus/reference/export_data.md)

## Author

Pattawee Puangchit
