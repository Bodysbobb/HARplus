# HARplus: Enhanced R Package for GEMPACK .har and .sl4 Files
[![Author](https://img.shields.io/badge/Pattawee.P-blue?label=Author)](https://bodysbobb.github.io/) ![Last Updated](https://img.shields.io/github/last-commit/Bodysbobb/GTAPViz?label=Last%20Updated&color=blue)

[![CRAN](https://www.r-pkg.org/badges/version/HARplus)](https://CRAN.R-project.org/package=HARplus/)

**Extensions:**  
[![GTAPViz](https://img.shields.io/badge/GTAPViz-276DC3?style=flat-square&logo=r&logoColor=white)](https://bodysbobb.github.io/GTAPViz/)


## Overview

HARplus is an R package designed to process and analyze .HAR and .SL4 files, making it easier for GEMPACK users and GTAP model researchers to handle large economic datasets. It simplifies the management of multiple experiment results, enabling faster and more efficient comparisons without complexity.

With HARplus, users can extract, restructure, and merge data seamlessly, ensuring compatibility across different tools. The processed data can be exported and used in R, Stata, Python, Julia, or any software that supports .txt, CSV, or Excel formats.

## Key Features

- **Efficient Data Extraction** – Supports selective header loading and optimized memory usage for handling large `.HAR` and `.SL4` files.  
- **Flexible Data Structuring** – Extract variables by name or dimension patterns while ensuring consistency across multiple inputs.  
- **Customizable Aggregation & Merging** – Manage subtotals, merge datasets, and structure data dynamically.  
- **Multiple Export Options** – Output extracted data in CSV, Stata, RDS, and Excel formats with structured formatting.  
- **Designed for GEMPACK** – Ensures smooth integration with `.HAR` and `.SL4` files while offering additional flexibility.  
- **Ideal for GTAP Model Users** – Built specifically to process and analyze GTAP model results efficiently.  

## How It Works

HARplus simplifies `.HAR` and `.SL4` file processing. You can:
- Load files and selectively extract headers.
- Extract data by variable name or dimension patterns.
- Group, merge, and restructure data with ease.
- Pivot and export data into structured formats.
- Filter subtotals and rename dimensions for clarity.

## Installation

HARplus (version 1.0.1) can be installed directly in R using:
```r
install.packages("HARplus")
```

## Quick Guide to HARplus

All commands in this package have several options that allow users to play around with the data more freely and efficiently, not just import and get the data. For a complete guide on HARplus functions, check out the **[Vignette](https://rpubs.com/Bodysbob/1273998/)** or **[GitHub Vignette](https://bodysbobb.github.io/HARplus/)**

Below is a categorized reference of the main functions in HARplus:

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

## License & Author  

HARplus is released under the **MIT License**. See the full **[license](LICENSE)**.  

**Author:**  
**Pattawee Puangchit**  
Ph.D. Candidate, Agricultural Economics  
Purdue University  
Research Assistant at GTAP  

## Acknowledgements

Acknowledgement is due to **Maros Ivanic** for his work on the `HARr` package, which served as the foundation for HARplus. This package would not have been possible without his contributions.

## GTAPViz: An Extension of HARplus for Visualization

I have developed another package specifically for visualization, particularly for GTAP users: **[GTAPViz](https://www.pattawee-pp.com/GTAPViz/)**

## GTAP Database

Sample data used in this [vignette](https://rpubs.com/Bodysbob/1273998/) is obtained from the GTAPv7 model and utilizes publicly available data from the [GTAP 9 database](https://www.gtap.agecon.purdue.edu/databases/archives.asp). For more details about the GTAP database and model, refer to the **[GTAP Database](https://www.gtap.agecon.purdue.edu/)**.
