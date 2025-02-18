# HARplus: Enhanced Processing of GEMPACK `.HAR` and `.SL4` Files

## Overview

**HARplus** is an R package designed to make working with GEMPACK `.har` and `.sl4` files easier and faster. It optimizes data extraction, transformation, and manipulation while keeping everything structured and memory-efficient. No more struggling with clunky data handling—HARplus does the heavy lifting for you.

## Why Use HARplus?

- **Blazing Fast Data Extraction** – Selective header loading and optimized memory use.
- **Smart Data Structuring** – Grab variables by name or dimension patterns.
- **Seamless Aggregation & Merging** – Control subtotals, merge datasets, and cleanly group data.
- **Easy Exporting** – Save your work in CSV, Stata, RDS, and Excel with zero headaches.
- **GEMPACK-Compatible** – Works smoothly with `.HAR` and `.SL4` files.

## Installation

HARplus is currently under **CRAN review** and will be available there soon. In the meantime, install it directly from GitHub using this command:

```r
# Install from GitHub using devtools
devtools::install_github("Bodysbobb/HARplus")
```
## How It Works

HARplus simplifies `.HAR` and `.SL4` file processing. You can:
- Load files and selectively extract headers.
- Pull data by variable name or dimension patterns.
- Group, merge, and restructure data with ease.
- Pivot and export data into structured formats.
- Filter subtotals and rename dimensions for clarity.

## Package Manual

For a complete guide on HARplus functions, check out the **[HARplus Package Manual](docs/Ref.HARplus.pdf)**.

## License

HARplus is released under the **MIT License**. See the full license [here](LICENSE).


## Author

**Pattawee Puangchit**  
Ph.D. Candidate, Agricultural Economics  
Purdue University  

## Acknowledgements

Big shoutout to **Maros Ivanic** for his work on the `HARr` package, which laid the foundation for HARplus. This package wouldn’t exist without that starting point!
