# Load and Process SL4 Files with Enhanced Options

Reads an SL4 file and processes its structured data into an enhanced SL4
object. Extracts structured variable information, dimensions, and
handles subtotal columns.

## Usage

``` r
load_sl4x(file_path, lowercase = FALSE, select_header = NULL)
```

## Arguments

- file_path:

  Character. The full path to the SL4 file to be read.

- lowercase:

  Logical. If `TRUE`, converts all variable names to lowercase. Default
  is `FALSE`.

- select_header:

  Character vector. Specific headers to extract; if `NULL`, all headers
  are read.

## Value

A structured list containing:

- `data`: Extracted SL4 variable data, stored as arrays or matrices.

- `dimension_info`: A list with:

  - `dimension_string`: A textual representation of dimensions (e.g.,
    "REG*COMM*YEAR").

  - `dimension_names`: The names of each dimension.

  - `dimension_sizes`: The size of each dimension.

## Details

- Uses
  [`load_harplus()`](https://bodysbobb.github.io/HARplus/reference/load_harplus.md)
  internally for optimized SL4 file reading.

- Extracts variable names, dimension structures, and metadata.

- Converts variable names to lowercase if `lowercase = TRUE`.

- Allows the selection of specific headers using `select_header`.

- Returns structured data with explicit dimension names and sizes.

## See also

[`load_harx`](https://bodysbobb.github.io/HARplus/reference/load_harx.md),
[`get_data_by_var`](https://bodysbobb.github.io/HARplus/reference/get_data_by_var.md),
[`get_data_by_dims`](https://bodysbobb.github.io/HARplus/reference/get_data_by_dims.md)

## Author

Pattawee Puangchit

## Examples

``` r
# Path to example files
sl4_path <- system.file("extdata", "TAR10.sl4", package = "HARplus")

# Basic loading
sl4_data <- load_sl4x(sl4_path)

# Load with lowercase names
sl4_data_lower <- load_sl4x(sl4_path, lowercase = TRUE)

# Load specific headers
sl4_selected <- load_sl4x(sl4_path, select_header = c("qo", "qgdp"))
```
