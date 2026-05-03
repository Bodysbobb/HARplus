# Load and Process GEMPACK HAR Files (Internal)

Reads a GEMPACK HAR file and efficiently extracts structured data while
maintaining compatibility with standard HAR formats. This implementation
builds upon the foundational work of the **HARr** package, reorganizing
the process for improved execution speed, memory management, and
handling of sparse data structures.

## Usage

``` r
load_harplus(con, coefAsname = FALSE, lowercase = TRUE, select_header = NULL)
```

## Arguments

- con:

  Character or connection. The file path to the HAR file or an open
  binary connection.

- coefAsname:

  Logical. If `TRUE`, replaces four-letter headers with coefficient
  names when available. Default is `FALSE`.

- lowercase:

  Logical. If `TRUE`, converts all string values to lowercase. Default
  is `TRUE`.

- select_header:

  Character vector. Specific headers to extract; if `NULL`, reads all
  headers.

## Value

A structured list where:

- Each element corresponds to a header in the HAR file.

- Names are either header names or coefficient names (if
  `coefAsname = TRUE`).

- Data maintains its original dimensions and attributes.

## Details

- **Efficient File Reading:** Reads large HAR files in chunks for better
  performance.

- **Optimized Memory Usage:** Reduces unnecessary allocations and
  improves cleanup.

- **Streamlined Header Processing:** Ensures accurate extraction of
  dimension metadata.

- **Supports Sparse Data Structures:** Handles `RESPSE` and `REFULL`
  headers efficiently.

**Supported HAR Header Types:**

- `1CFULL`: Character headers

- `2IFULL`: Integer headers

- `2RFULL`: Real headers

- `REFULL`: Real headers with extended metadata

- `RESPSE`: Sparse real headers

## See also

[`load_sl4x`](https://bodysbobb.github.io/HARplus/reference/load_sl4x.md),
[`load_harx`](https://bodysbobb.github.io/HARplus/reference/load_harx.md)

## Author

Pattawee Puangchit
