# Load and Process HAR Files with Header Selection

Reads a GEMPACK HAR file and extracts structured data while maintaining
compatibility with standard HAR formats. Provides flexibility in naming
conventions and header selection.

## Usage

``` r
load_harx(
  file_path,
  coefAsname = FALSE,
  lowercase = FALSE,
  select_header = NULL
)
```

## Arguments

- file_path:

  Character. The file path to the HAR file.

- coefAsname:

  Logical. If `TRUE`, replaces four-letter headers with coefficient
  names when available. Default is `FALSE`.

- lowercase:

  Logical. If `TRUE`, converts all variable names to lowercase. Default
  is `FALSE`.

- select_header:

  Character vector. Specific headers to read; if `NULL`, all headers are
  read. Example: `select_header = c("A", "E1")`.

## Value

A structured list containing:

- `data`: Extracted HAR variable data stored as matrices, arrays, or
  vectors.

- `dimension_info`: A list with:

  - `dimension_string`: A textual representation of dimensions (e.g.,
    "REG*COMM*YEAR").

  - `dimension_names`: The names of each dimension.

  - `dimension_sizes`: The size of each dimension.

## Details

- Uses
  [`load_harplus()`](https://bodysbobb.github.io/HARplus/reference/load_harplus.md)
  internally for efficient HAR file reading.

- Allows optional conversion of variable names to lowercase
  (`lowercase = TRUE`).

- Supports coefficient-based naming (`coefAsname = TRUE`).

- Enables selective header extraction via
  `select_header = c("A", "E1")`.

- Returns structured data with explicit dimension names and sizes.

## See also

[`load_sl4x`](https://bodysbobb.github.io/HARplus/reference/load_sl4x.md),
[`get_data_by_var`](https://bodysbobb.github.io/HARplus/reference/get_data_by_var.md),
[`get_data_by_dims`](https://bodysbobb.github.io/HARplus/reference/get_data_by_dims.md)

## Author

Pattawee Puangchit

## Examples

``` r
# Path to example files
har_path <- system.file("extdata", "TAR10-WEL.har", package = "HARplus")

# Basic loading
har_data <- load_harx(har_path)

# Load with coefficient names
har_data_coef <- load_harx(har_path, coefAsname = TRUE)

# Load with lowercase names
har_data_lower <- load_harx(har_path, lowercase = TRUE)

# Load specific headers
har_selected <- load_harx(har_path, select_header = c("A", "E1"))

# Load with multiple options
har_combined <- load_harx(har_path,
                         coefAsname = TRUE,
                         lowercase = TRUE,
                         select_header = c("A", "E1"))
```
