# HARplus: Enhanced R Package for GEMPACK .har and .sl4 Files
[![Author](https://img.shields.io/badge/Pattawee.P-blue?label=Author)](https://www.pattawee-pp.com/)
![Last Updated](https://img.shields.io/github/last-commit/Bodysbobb/HARplus?label=Last%20Updated&color=blue) 
[![Star](https://img.shields.io/github/stars/Bodysbobb/HARplus?style=social)](https://github.com/Bodysbobb/HARplus/stargazers)

[![CRAN](https://www.r-pkg.org/badges/version/HARplus)](https://CRAN.R-project.org/package=HARplus)
[![GitHub Version](https://img.shields.io/github/v/tag/Bodysbobb/HARplus?label=GitHub%20Version&color=3CB371&sort=semver)](https://github.com/Bodysbobb/HARplus/releases/latest)
[![License](https://img.shields.io/github/license/Bodysbobb/HARplus?color=3CB371)](https://github.com/Bodysbobb/HARplus/blob/main/LICENSE.md/)

**Extensions:**  
[![GTAPViz](https://img.shields.io/badge/GTAPViz-276DC3?style=flat-square&logo=r&logoColor=white)](https://www.pattawee-pp.com/GTAPViz/)

---

## Version 1.2.0 - Update (02-May-2026)

### Bug fixes
- Fixed an issue where `load_sl4x()` and `load_harx()` returned a `subscript out of bounds` error for macro variables.

### New features
- `save_har()` now supports writing `1C` headers for string-format data.
- `save_har()` now supports writing ViewHAR-recognized mapping headers through the new `mappings` argument.
- Added support for writing mixed HAR files containing string sets, mapping vectors, numeric data frames, sparse numeric data, and integer matrices in a single `save_har()` call.
- Added optional `header_type` control for explicitly defining header roles when automatic detection is not sufficient.
- Added stricter header-name validation to prevent silent duplication after GEMPACK's four-character header truncation.
- Improved mapping validation: missing or invalid mapping targets are written as `"0"` and reported with warnings.
- Clarified the `save_har()` interface: `value_cols` is used only for numeric value columns in data-frame inputs, while mapping behavior is defined separately through `mappings`.
- Updated `save_har()` documentation and examples for CRAN-style usage, including mixed-header exports and ViewHAR-compatible mapping exports.

---

## Overview

HARplus is an R package designed to process and analyze .HAR and .SL4 files, making it easier for GEMPACK users and GTAP model researchers to handle large economic datasets. It simplifies the management of multiple experiment results, enabling faster and more efficient comparisons without complexity.

With HARplus, users can extract, restructure, and merge data seamlessly, ensuring compatibility across different tools. The processed data can be exported and used in R, Stata, Python, Julia, or any software that supports .txt, CSV, or Excel formats.

---

## Key Features

- **Efficient Data Extraction** – Supports selective header loading and optimized memory usage for handling large `.HAR` and `.SL4` files.  
- **Flexible Data Structuring** – Extract variables by name or dimension patterns while ensuring consistency across multiple inputs.  
- **Customizable Aggregation & Merging** – Manage subtotals, merge datasets, and structure data dynamically.  
- **Multiple Export Options** – Output extracted data in CSV, Stata, RDS, and Excel formats with structured formatting.  
- **Powerful HAR Writing** – Includes `save_har()` for exporting datasets to GEMPACK `.HAR` format with full binary compliance.  
- **Designed for GEMPACK** – Ensures smooth integration with `.HAR` and `.SL4` files while offering additional flexibility.  
- **Ideal for GTAP Model Users** – Built specifically to process and analyze GTAP model results efficiently.  

---

## How It Works

HARplus simplifies `.HAR` and `.SL4` file processing. You can:
- Load files and selectively extract headers.
- Extract data by variable name or dimension patterns.
- Group, merge, and restructure data with ease.
- Pivot and export data into structured formats.
- Filter subtotals and rename dimensions for clarity.

---

## Installation

HARplus (version 1.1.2) can be installed directly in R using:
```r
install.packages("HARplus")
```

While the latest HARplus (version 1.2.0) can be installed from my GitHub using:
```r
pak::pak("Bodysbobb/HARplus")
```

## Quick Guide to HARplus

All commands in this package have several options that allow users to play around with the data more freely and efficiently, not just import and get the data. For a complete guide on HARplus functions, check out the **[Vignette](https://rpubs.com/Bodysbob/1273998/)** or **[GitHub Vignette](https://www.pattawee-pp.com/HARplus/)**

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

### HAR Exporting (New in v1.1.1)
- **`save_har()`** – Saves processed data frames or arrays into GEMPACK-compatible `.HAR` files, automatically generating 1C set headers and supporting up to seven dimensions.  

#### Technical Specifications
- Supports both **1C (string)** and **RE (real)** headers  
- Automatically generates 1C dimension sets (e.g., `REG`, `COMM`, `ENDW`)  
- Accepts data frames or arrays with flexible column naming  
- Writes associated set headers when `export_sets = TRUE`  
- Maintains full GEMPACK binary structure with no size limitation  
- Supports up to seven dimensions and approximately 2 million elements per chunk  
- Allows **dimension renaming** and supports **duplicate dimension names** (e.g., `COMMxREGxREG`) during export (New in v1.1.2)  

### Shock Calculation Framework (New in v1.1.2)
These functions provide a complete workflow to **calculate, structure, and export GEMPACK-compatible shock files** directly from `.HAR`, `.SL4`, `.CSV`, or `.XLSX` datasets—eliminating the need for manual conversion when preparing dynamic simulation shocks.

- **`shock_calculate_uniform()`** – Calculates uniform percentage shocks across all base rates and **exports directly to GEMPACK `.HAR` format**. Supports additive (`+`, `-`) and multiplicative (`*`, `/`) adjustments.  
- **`shock_calculate()`** – Computes target-based shocks by comparing initial and target datasets, automatically **exporting the resulting shocks to `.HAR` files** with dynamic timeline headers (e.g., `ONEY`, `TWOY`, `THRY`, etc.).  
- **`create_initial_config()`**, **`create_target_config()`**, and **`create_calc_config()`** – Define input sources, column mappings, and timeline periods for use in both uniform and target-based shock calculations.  

---

## License & Author  

HARplus is released under the **MIT License**. See the full **[license](https://github.com/Bodysbobb/HARplus/blob/main/LICENSE.md)**.  

**Author:**  
**Pattawee Puangchit**  
Ph.D. Candidate, Agricultural Economics  
Purdue University  
Research Assistant at GTAP  

---

## Acknowledgements

Acknowledgement is due to **Maros Ivanic** for his work on the [`HARr`](https://github.com/USDA-REE-ERS/MTED-HARr/) package, which served as the foundation for HARplus. This package would not have been possible without his contributions.

---

## GTAPViz: An Extension of HARplus for Visualization

I have developed another package specifically for visualization, particularly for GTAP users: **[GTAPViz](https://www.pattawee-pp.com/GTAPViz/)**

---

## GTAP Database

Sample data used in this [vignette](https://rpubs.com/Bodysbob/1273998/) is obtained from the GTAPv7 model and utilizes publicly available data from the [GTAP 9 database](https://www.gtap.agecon.purdue.edu/databases/archives.asp). For more details about the GTAP database and model, refer to the **[GTAP Database](https://www.gtap.agecon.purdue.edu/)**.

---

# Older Version

## Version 1.1.3 - Update (30-Oct-2025)

- **Writing HAR files without limitations** – The new `save_har()` function fully supports writing `.HAR` files with no size restrictions, allowing up to seven dimensions and approximately two million elements per chunk.  
- **Shock calculation and HAR export** – Introduced `shock_calculate_uniform()` and `shock_calculate()` to compute and **export shock results directly into GEMPACK-compatible `.HAR` files**, supporting dynamic multi-period calculations (e.g., `ONEY`, `TWOY`, `THRY`, etc.) for recursive-dynamic simulations.