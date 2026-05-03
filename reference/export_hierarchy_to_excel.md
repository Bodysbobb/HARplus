# Export Hierarchical Pivot Table to Excel (Internal)

Internal function to export hierarchical pivot tables to Excel with
formatted headers.

## Usage

``` r
export_hierarchy_to_excel(pivot_df, file_path, xlsx_filename = NULL)
```

## Arguments

- pivot_df:

  A hierarchical pivot object created by pivot_data_hierarchy().

- file_path:

  Character. The file path for Excel export.

- xlsx_filename:

  Character. The name for the Excel file when using multi_sheet_xlsx. If
  NULL, uses the name of the dataset. Default is `NULL`.

## Value

Invisibly returns NULL.
