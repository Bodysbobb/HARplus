# HARplus: Enhanced Processing of GEMPACK `.HAR` and `.SL4` Files

## Overview

HARplus provides an efficient way to handle `.HAR` and `.SL4` files, especially when working with multiple input files. It helps extract, structure, and transform data while maintaining flexibility and consistency across datasets.

## Key Features

- **Efficient Data Extraction** – Supports selective header loading and optimized memory usage for handling large `.HAR` and `.SL4` files.  
- **Flexible Data Structuring** – Extract variables by name or dimension patterns while ensuring consistency across multiple inputs.  
- **Customizable Aggregation & Merging** – Manage subtotals, merge datasets, and structure data dynamically.  
- **Multiple Export Options** – Output extracted data in CSV, Stata, RDS, and Excel formats with structured formatting.  
- **Designed for GEMPACK** – Ensures smooth integration with `.HAR` and `.SL4` files while offering additional flexibility.  
- **Ideal for GTAP Model Users** – Built specifically to process and analyze GTAP model results efficiently.  

## Installation

HARplus is currently under **CRAN review** and will be available there soon. In the meantime, install it directly from GitHub using the following command:

```r
devtools::install_github("Bodysbobb/HARplus")
```

## How It Works

HARplus simplifies `.HAR` and `.SL4` file processing. You can:
- Load files and selectively extract headers.
- Extract data by variable name or dimension patterns.
- Group, merge, and restructure data with ease.
- Pivot and export data into structured formats.
- Filter subtotals and rename dimensions for clarity.

## Quick Guide to HARplus

Below is a categorized reference of the main functions in HARplus.

### Data Importing
- **`load_harx()`** – Loads `.HAR` files with selective header extraction and structured metadata.  
- **`load_sl4x()`** – Loads `.SL4` files, extracting variable names and dimension structures.

### Data Extraction
- **`get_data_by_var()`** – Extracts specific variables from `.HAR` or `.SL4` datasets, supporting subtotal filtering and merging.  
- **`get_data_by_dims()`** – Extracts data based on dimension patterns, with options for merging and subtotal filtering.

### Data Structure Summary & Comparison
- **`get_dim_elements()`** – Lists unique dimension elements (e.g., `REG`, `COMM`).  
- **`get_dim_patterns()`** – Extracts unique dimension structures (e.g., `REG*COMM*ACTS`).  
- **`get_var_structure()`** – Summarizes variable names, dimensions, and data structure.  
- **`compare_var_structure()`** – Compares variable structures across multiple datasets for compatibility.

### Data Grouping & Processing
- **`group_data_by_dims()`** – Groups extracted data by dimension priority, with support for automatic renaming and subtotal handling.  
- **`rename_dims()`** – Renames dimension names for consistency.

### Data Transformation
- **`pivot_data()`** – Converts long-format data into wide format.  
- **`pivot_data_hierarchy()`** – Creates hierarchical pivot tables for structured reporting.

### Data Export
- **`export_data()`** – Exports extracted data to CSV, Stata, TXT, RDS, or XLSX, with support for multi-sheet exports.

## Package Manual

All commands in this package have several options that allow users to play around with the data more freely and efficiently, not just import and get the data. For a complete guide on HARplus functions, check out the **[HARplus Package Manual](docs/Referene%20Manual%20-%20HARplus.pdf)** and **[Vignette](https://rpubs.com/Bodysbob/1273998)**


## License

HARplus is released under the **MIT License**. See the full license [here](LICENSE).

## Author

**Pattawee Puangchit**  
Ph.D. Candidate, Agricultural Economics  
Purdue University  
Research Assistant at GTAP  

## Acknowledgements

Acknowledgement is due to **Maros Ivanic** for his work on the `HARr` package, which served as the foundation for HARplus. This package would not have been possible without his contributions.

## Note

Sample data used in this [vignette](https://rpubs.com/Bodysbob/1273998) is obtained from the GTAPv7 model and utilizes publicly available data from the [GTAP 9 database](https://www.gtap.agecon.purdue.edu/databases/archives.asp). For more details about the GTAP database and model, refer to the **[GTAP Database](https://www.gtap.agecon.purdue.edu/)**.