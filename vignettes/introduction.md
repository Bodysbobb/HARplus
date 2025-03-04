Enhanced Processing of GEMPACK .HAR and .SL4 Files
================
Pattawee Puangchit
2025-02-19

# Overview of HARplus

The `HARplus` package enhances GEMPACK users’ experience by streamlining
`.har` and `.sl4` file processing. It efficiently extracts, organizes,
and manages dimension structures, ensuring consistency, optimized memory
usage, and simplified data manipulation. The package enables fast data
merging, aggregation, and transformation while maintaining a consistent
interface across GEMPACK file types. Users can easily pivot data into
familiar formats and export results in various formats.

A key feature of `HARplus` is its **flexible subtotal level handling**,
allowing users to selectively retain `"TOTAL"` values, decomposed
components, or both. This ensures precise data extraction for various
economic modeling needs without unnecessary redundancy.

# Introduction

This vignette covers key functions of `HARplus`, highlighting its
ability to handle multiple inputs for efficient data processing.
Designed for economic modelers and GEMPACK practitioners, it simplifies
working with multiple datasets and enhances analytical workflows.

Sample data used in this vignette is obtained from the GTAPv7 model and
utilizes publicly available data from the GTAP 9 database. For more
details, refer to the [GTAP Database
Archive](https://www.gtap.agecon.purdue.edu/databases/archives.asp).

# Acknowledgement

The development of `HARplus` builds upon the foundational work of the
`HARr` package. I sincerely acknowledge and appreciate the contributions
of Maros Ivanic, whose `HARr` package served as the baseline for this
development, advancing efficient data handling for GEMPACK users.

# Installation and Loading Example Data

Before proceeding, ensure that `HARplus` is installed and loaded:

``` r
library(HARplus)
```

# Loading Data `.HAR` and `.SL4`

First, load the example data using the following commands `<load_harx>`
and `<load_sl4x>`:

``` r
# Paths to the .har files
har_path1 <- system.file("extdata", "TAR10-WEL.har", package = "HARplus")
har_path2 <- system.file("extdata", "SUBT10-WEL.har", package = "HARplus")

# Paths to the .sl4 files
sl4_path1 <- system.file("extdata", "TAR10.sl4", package = "HARplus")
sl4_path2 <- system.file("extdata", "SUBT10.sl4", package = "HARplus")

# Load the .har files using load_harx()
har_data1 <- load_harx(har_path1)
har_data2 <- load_harx(har_path2)

# Load the .sl4 files using load_sl4x()
sl4_data1 <- load_sl4x(sl4_path1)
sl4_data2 <- load_sl4x(sl4_path2)
```

# Extracting Data

This package allows two main ways to enhance the GEMPACK user experience
and improve user-friendliness when extracting data:  
1. By selecting the variable/header name using `<get_data_by_var>`.  
2. By using dimension patterns (e.g., `REG*COMM`) with
`<get_data_by_dims>`.

Both functions have similar options:  
- Extract a single variable, multiple variables, or all variables (NULL
or “ALL”) from the datasets.  
- Extract data from a single dataset or multiple datasets.  
- Rename the experiment (i.e., dataset) name, which is displayed in the
**Experiment** column (this column is automatically added to handle
multiple dataset merges). By default, the name is the dataset name.  
- Reporting data level (all, only total, only subtotals). - Rename
dimension names, which will be used as column names.  
- Merge data across multiple datasets within the same patterns.  
- For `<get_data_by_var>`, variables with the same name (e.g., `"qo"`)
from different experiments (`exp1` and `exp2`) will be merged into a
single final dataframe, with the input source identified in the
**Experiment** column.  
- Similarly, for `<get_data_by_dims>`, data with matching dimension
patterns (e.g., `REG*COMM`) will be merged.

Let’s start with the simplest example as follows:

``` r
# Extract data for a single variable
data_qo <- get_data_by_var("qo", sl4_data1)
print(head(data_qo[["sl4_data1"]][["qo"]], 4))
#>          ACTS     REG Subtotal     Value Variable Dimension Experiment
#> 1 GrainsCrops Oceania    TOTAL -5.283822       qo  ACTS*REG  sl4_data1
#> 2    MeatLstk Oceania    TOTAL -8.473134       qo  ACTS*REG  sl4_data1
#> 3  Extraction Oceania    TOTAL -1.617209       qo  ACTS*REG  sl4_data1
#> 4    ProcFood Oceania    TOTAL -3.570224       qo  ACTS*REG  sl4_data1

# Extract multiple variables from multiple datasets
data_multiple <- get_data_by_var(c("qo", "qgdp"), sl4_data1, sl4_data2)
print(head(data_multiple[["sl4_data1"]][["qo"]], 4))
#>          ACTS     REG Subtotal     Value Variable Dimension Experiment
#> 1 GrainsCrops Oceania    TOTAL -5.283822       qo  ACTS*REG  sl4_data1
#> 2    MeatLstk Oceania    TOTAL -8.473134       qo  ACTS*REG  sl4_data1
#> 3  Extraction Oceania    TOTAL -1.617209       qo  ACTS*REG  sl4_data1
#> 4    ProcFood Oceania    TOTAL -3.570224       qo  ACTS*REG  sl4_data1
print(head(data_multiple[["sl4_data2"]][["qo"]], 4))
#>          ACTS     REG Subtotal     Value Variable Dimension Experiment
#> 1 GrainsCrops Oceania    TOTAL -5.283822       qo  ACTS*REG  sl4_data2
#> 2    MeatLstk Oceania    TOTAL -8.473134       qo  ACTS*REG  sl4_data2
#> 3  Extraction Oceania    TOTAL -1.617209       qo  ACTS*REG  sl4_data2
#> 4    ProcFood Oceania    TOTAL -3.570224       qo  ACTS*REG  sl4_data2

# Extract all variables separately from multiple datasets
data_list <- get_data_by_var(NULL, sl4_data1, sl4_data2)
print(names(data_list))
#> [1] "sl4_data1" "sl4_data2"

# Extract all variables and merge the same variable from multiple datasets
data_all <- get_data_by_var(NULL, sl4_data1, sl4_data2, merge_data = TRUE)
print(names(data_all))
#> [1] "merged"

# Return all value levels
data_all <- get_data_by_var("qo", sl4_data1, sl4_data2, subtotal_level = TRUE)
print(head(data_all[["sl4_data1"]][["qo"]], 4))
#>          ACTS     REG Subtotal     Value Variable Dimension Experiment
#> 1 GrainsCrops Oceania    TOTAL -5.283822       qo  ACTS*REG  sl4_data1
#> 2    MeatLstk Oceania    TOTAL -8.473134       qo  ACTS*REG  sl4_data1
#> 3  Extraction Oceania    TOTAL -1.617209       qo  ACTS*REG  sl4_data1
#> 4    ProcFood Oceania    TOTAL -3.570224       qo  ACTS*REG  sl4_data1

# Return only TOTAL, drop subtotal
data_total <- get_data_by_var("qo", sl4_data1, sl4_data2, subtotal_level = FALSE)
print(head(data_total[["sl4_data1"]][["qo"]], 4))
#>          ACTS     REG Subtotal     Value Variable Dimension Experiment
#> 1 GrainsCrops Oceania    TOTAL -5.283822       qo  ACTS*REG  sl4_data1
#> 2    MeatLstk Oceania    TOTAL -8.473134       qo  ACTS*REG  sl4_data1
#> 3  Extraction Oceania    TOTAL -1.617209       qo  ACTS*REG  sl4_data1
#> 4    ProcFood Oceania    TOTAL -3.570224       qo  ACTS*REG  sl4_data1

# Return only subtotal, drop TOTAL (result is empty if there in subtotal)
data_decomp <- get_data_by_var("qo", sl4_data1, sl4_data2, subtotal_level = "decomposed")
print(head(data_decomp[["sl4_data2"]][["qo"]], 4))
#>            ACTS     REG   Subtotal Value Variable Dimension Experiment
#> 101 GrainsCrops Oceania to changes     0       qo  ACTS*REG  sl4_data2
#> 102    MeatLstk Oceania to changes     0       qo  ACTS*REG  sl4_data2
#> 103  Extraction Oceania to changes     0       qo  ACTS*REG  sl4_data2
#> 104    ProcFood Oceania to changes     0       qo  ACTS*REG  sl4_data2

# Rename specific columns
data_col_renamed <- get_data_by_var("qo", sl4_data1, 
                             rename_cols = c(REG = "Region", COMM = "Commodity"))
str(data_col_renamed)
#> List of 1
#>  $ sl4_data1:List of 1
#>   ..$ qo:'data.frame':   100 obs. of  7 variables:
#>   .. ..$ ACTS      : chr [1:100] "GrainsCrops" "MeatLstk" "Extraction" "ProcFood" ...
#>   .. ..$ Region    : chr [1:100] "Oceania" "Oceania" "Oceania" "Oceania" ...
#>   .. ..$ Subtotal  : chr [1:100] "TOTAL" "TOTAL" "TOTAL" "TOTAL" ...
#>   .. ..$ Value     : num [1:100] -5.28 -8.47 -1.62 -3.57 8.39 ...
#>   .. ..$ Variable  : chr [1:100] "qo" "qo" "qo" "qo" ...
#>   .. ..$ Dimension : chr [1:100] "ACTS*REG" "ACTS*REG" "ACTS*REG" "ACTS*REG" ...
#>   .. ..$ Experiment: chr [1:100] "sl4_data1" "sl4_data1" "sl4_data1" "sl4_data1" ...

# Rename experiment names
data_exp_renamed <- get_data_by_var("qo", sl4_data1, sl4_data2, 
                             experiment_names = c("EXP1", "EXP2"))
print(names(data_exp_renamed))
#> [1] "EXP1" "EXP2"

# Merge variable data across multiple datasets with custom experiment names
data_merged <- get_data_by_var(, sl4_data1, sl4_data2,
                            experiment_names = c("EXP1", "EXP2"), 
                            merge_data = TRUE,
                            rename_cols = c(REG = "Region", COMM = "Commodity"))
print(head(data_merged$merged[[1]], 4))
#>     Commodity  Region Subtotal    Value Variable Dimension Experiment
#> 1 GrainsCrops Oceania    TOTAL 10.34259      pds  COMM*REG       EXP1
#> 2    MeatLstk Oceania    TOTAL 11.86026      pds  COMM*REG       EXP1
#> 3  Extraction Oceania    TOTAL 11.49640      pds  COMM*REG       EXP1
#> 4    ProcFood Oceania    TOTAL 13.40175      pds  COMM*REG       EXP1
```

Now, let’s delve into `<get_data_by_dims>`. This command offers an
additional feature compared to `<get_data_by_var>`: it allows for
`patter_mix`. For instance, REG*COMM and COMM*REG can be treated as
equivalent when merging data (this only applies if merge_data = TRUE).
You can experiment with the following commands:

``` r
# Merge data by dimensions (e.g., REG*COMM != COMM*REG)
data_no_mix <- get_data_by_dims(NULL, sl4_data1, sl4_data2, 
                                merge_data = TRUE, 
                                pattern_mix = FALSE)

# Merge data while allowing interchangeable dimensions (e.g., REG*COMM = COMM*REG)
data_pattern_mixed <- get_data_by_dims(NULL, sl4_data1, sl4_data2, 
                                       merge_data = TRUE, 
                                       pattern_mix = TRUE)
```

This flexibility is particularly useful when working with datasets where
dimension order does not affect the interpretation of the data.

# Grouping Data By Dimension

The `<group_data_by_dims>` function is a powerful tool that
**categorizes extracted data into meaningful dimension-based groups**.
This is a key feature of this package, as it allows GEMPACK users
(particularly those working with GTAP model results) to merge data into
a structured and useful dataframe.

For example, if users want to retrieve all variables defined by `REG`,
they can simply set `REG` as a priority while also assigning a new
column name for `REG`, such as `REG = "Region"`. The function will then
**merge all datasets that contain `REG` as a dimension element into a
single dataframe**, ensuring that all relevant data is consolidated.

Unlike `<get_data_by_dims>`, which focuses on extracting data,
`<group_data_by_dims>` **organizes and merges data dynamically** based
on the structure and priority defined by the user.

It is particularly useful because: - It **groups extracted data by
dimension levels** (`1D`, `2D`, `3D`, …). - Users can **define
priority-based merging** (e.g., prioritizing `REG` first, then `COMM`).
The function first attempts to merge all datasets containing `REG`, then
moves on to merge datasets containing `COMM`. If a dataset contains
`REG*COMM`, it will be included in the `REG` output. - It
**automatically renames dimensions** (`auto_rename = TRUE`) to improve
dataset compatibility while preserving all variable information. - It
**transforms data into a structured long format**, simplifying further
analysis. - It **supports merging grouped data across multiple
datasets**. - It **filters subtotal levels**, allowing users to retain
`"TOTAL"` values or decomposed components as needed. - If certain
datasets cannot be merged, the function generates a **detailed report
identifying the root of the issue**, enabling users to manually
manipulate the data if necessary. - This function primarily focuses on
**1D and 2D data**, ensuring that **higher-dimension data (\>2D) is
merged only with datasets that share the same pattern**.

## **Understanding Priority List in `group_data_by_dims`**

The **priority list** in `<group_data_by_dims>` allows users to control
**how dimensions are grouped and merged**.  
By defining priorities, the function ensures that **datasets containing
high-priority dimensions are merged as much as possible**, but **only if
they share the same structure**.

### **1. Single Priority Example (`REG` only)**

If we prioritize only `REG`, the function will **attempt to merge all
datasets containing `REG` as much as possible**, while ensuring that
datasets with different structures remain separate.

``` r
# Define single priority (Only Region-based grouping)
priority_list <- list("Region" = c("REG"))

# Grouping data with a single priority
grouped_data_single <- group_data_by_dims("ALL", sl4_data1, sl4_data2,
                                          priority = priority_list, auto_rename = TRUE)

# Print structure
print(names(grouped_data_single))
#> [1] "1D" "2D" "3D" "4D"
print(names(grouped_data_single[["1D"]]))
#> [1] "Region" "Other"
print(names(grouped_data_single[["2D"]]))
#> [1] "Region"
```

What happens here? - The function tries to merge all datasets containing
REG, but only if they share the same data structure. - If some datasets
have different structures, they will remain separate. - The merging
happens separately in each data dimension (e.g., 1D, 2D, etc.).

### **2. Multiple Priorities Example (`COMM` before `REG`)**

If we prioritize **`COMM` first and `REG` second**, datasets containing
`COMM` will be **merged first**, followed by `REG`, **as much as
possible** while ensuring datasets with different structures remain
separate.

``` r
# Define multiple priority levels: Sector first, then Region
priority_list <- list(
  "Sector" = c("COMM", "ACTS"),
  "Region" = c("REG")
)

# Grouping data with multiple priorities
grouped_data_multiple <- group_data_by_dims("ALL", sl4_data1, 
                                            priority = priority_list, 
                                            auto_rename = TRUE)

# Print structure
print(names(grouped_data_multiple))
#> [1] "1D" "2D" "3D" "4D"
print(names(grouped_data_multiple[["1D"]]))
#> [1] "Sector" "Region" "Other"
print(names(grouped_data_multiple[["2D"]]))
#> [1] "Sector" "Region"
```

What happens here?  
- The function attempts to merge all datasets containing `COMM` and
`ACTS` first, both of which are merged into the **Sector** column before
attempting to merge other datasets with `REG`, as long as they share the
same structure.  
- Therefore, if datasets contain `REG*COMM` and `REG*ACTS`, they will be
merged into the **Sector** dataframe, **not the REG dataframe**. - If
datasets cannot be merged due to structural differences, they are kept
separate in their respective dimension groups.

## **Understanding `auto_rename` in `group_data_by_dims`**

The `auto_rename` option in `<group_data_by_dims>` plays a crucial role
in ensuring **successful and smooth merging** of datasets.  
When enabled, it **automatically renames lower-priority dimensions** to
`"Dim1"`, `"Dim2"`, etc., which allows datasets with slightly different
dimension names to be merged, while still preserving the original
dimension structure.

Without `auto_rename`, datasets with similar but non-identical dimension
names **may fail to merge**, leading to separate outputs instead of a
unified dataset.

### **Example: Grouping Without `auto_rename`**

By default (`auto_rename = FALSE`), the function **keeps all original
dimension names**.  
This means datasets that contain **similar but not identical
dimensions** (e.g., `REG*ENDW` and `REG*END`) **cannot be merged**.

``` r
# Define priority: First by Sector (COMM, ACTS), then by Region (REG)
priority_list <- list(
  "Sector" = c("COMM", "ACTS"),
  "Region" = c("REG")
)

# Grouping data without auto_rename
grouped_data_no_rename <- group_data_by_dims("ALL", sl4_data1, 
                                             priority = priority_list, 
                                             auto_rename = FALSE)

# Print structure
print(names(grouped_data_no_rename))
#> [1] "1D"     "2D"     "3D"     "4D"     "report"
print(names(grouped_data_no_rename[["1D"]]))
#> [1] "Sector"   "Region"   "unmerged"
print(names(grouped_data_no_rename[["2D"]]))
#> [1] "Sector"   "unmerged"
```

Without `auto_rename`, the number of mergeable dataframes will be
**less** compared to when `auto_rename = TRUE`, as it **does not allow
merging across different dimension names**, even if their structures are
otherwise compatible.

# Reshaping and Renaming Data

## Pivoting Data

The `<pivot_data>` function transforms long-format data from SL4 or HAR
objects into a wide format, making it more suitable for analysis and
visualization. This transformation is particularly useful when working
with GEMPACK outputs that need to be reshaped for reporting or further
processing.

``` r
# Extract data to pivot
data_multiple <- get_data_by_var(c("qo", "pca"), sl4_data1)

# Pivot a single column
pivoted_single <- pivot_data(data_multiple, 
                             pivot_cols = "REG")

# Pivot multiple columns
pivoted_multi <- pivot_data(data_multiple, 
                            pivot_cols = c("COMM", "REG"))
```

## Pivot Data Hierarchy

While `pivot_data` provides basic pivoting functionality,
`pivot_data_hierarchy` offers enhanced capabilities for creating
hierarchical pivot tables similar to those found in spreadsheet
applications. This function is particularly useful when you need to
maintain dimensional hierarchies in your output or create Excel-ready
pivot tables.

Key differences from regular pivoting:

- Maintains hierarchical relationships between pivot columns
- Supports direct Excel export with properly formatted headers
- Preserves dimension order as specified in pivot_cols
- Creates nested column headers reflecting the hierarchy
- **Must be exported to XLSX only using the command option
  `<export = TRUE, file_path = "./path/to/output">`. This pivot table
  cannot be exported with the <export> command.**

``` r
# Create hierarchical pivot without export
pivot_hier <- pivot_data_hierarchy(data_multiple, 
                                  pivot_cols = c("REG", "COMM"))

# Create and export to Excel in one step
pivot_export <- pivot_data_hierarchy(data_multiple, 
                                   pivot_cols = c("REG", "COMM"),
                                   export = TRUE,
                                   file_path = file.path(tempdir(), "pivot_output.xlsx"))
```

The hierarchy in the resulting pivot table follows the order specified
in `pivot_cols`. For example, when using `c("REG", "COMM")`, the output
will show:

- First level: Region (REG)  
- Second level: Commodity (COMM) nested under each region
- Values arranged according to this hierarchy

When exported to Excel, the hierarchical structure is automatically
formatted with:

- Properly aligned multi-level column headers
- Bold formatting for header levels  
- Clean sheet names for multiple variables

## Rename Dimensions

The `rename_dims` function provides flexible renaming capabilities for
dimensions in SL4 or HAR objects. You can rename either dimension names,
list names, or both.

``` r
# Define a renaming map
mapping_df <- data.frame(
 old = c("REG", "COMM"),
 new = c("Region", "Commodity")
)

# Rename dimensions only
renamed_dims <- rename_dims(sl4_data1, mapping_df)

# Rename both dimensions and list names
renamed_both <- rename_dims(sl4_data1, mapping_df, rename_list_names = TRUE)
```

The mapping dataframe must have two columns: the first for current names
and the second for new names. The function preserves data structure
while updating dimension labels according to the specified mapping.

# Exporting Data to CSV/STATA/TEXT/RDS/XLSX

The `export_data` function allows you to export SL4 and HAR data into
various formats. Supported formats include: `"csv"`, `"xlsx"`,
`"stata"`, `"txt"`, and `"rds"`. You can export single data frames or
multi-variable results while preserving their structure.

``` r
# Extract data
data_multiple <- get_data_by_var(c("qo", "pca"), sl4_data1)

# Export 
export_data(data_multiple, file.path(tempdir(), "output_directory"), 
           format = c("csv", "xlsx", "stata", "txt", "rds"),
           create_subfolder = TRUE,
           multi_sheet_xlsx = TRUE)
```

# Exploring Data Structure

Since this package is designed to handle multiple inputs with a similar
structure, such as simulation outputs from the GTAP model with different
shocks or experiments, the first important step is to understand the
data structure. This process can also be useful even with a single
input, as it helps in analyzing the data shape and dimension size of
each variable.

There are a couple of commands available to illustrate the data
structure, all of which can be applied to both `.har` and `.sl4` files
in the same manner.

## Variable Structure

To get a summary of variable names, dimension counts, dimension
patterns, and optionally the column and observation counts for one or
multiple variables from a single or multiple datasets (return as
separate lists):

``` r
# (1) Getting all variables from the input file
vars_har_sum <- get_var_structure("ALL", har_data1)
vars_sl4_sum <- get_var_structure(, har_data1)

# (2) Getting selected variables
var_sl4_sum <- get_var_structure(c("pds","pfd","pms"), sl4_data1)
print(head(var_sl4_sum[["sl4_data1"]], 4))
#>   Variable    Dimensions DimSize DataShape
#> 1      pds      COMM*REG       2     10x10
#> 2      pfd COMM*ACTS*REG       3  10x10x10
#> 3      pms      COMM*REG       2     10x10


# (3) Including column size and number of observation in the summary
var_sl4_sum <- get_var_structure(c("pds","pfd","pms"), sl4_data1, sl4_data2, 
                                 include_col_size = TRUE)
print(head(var_sl4_sum[["sl4_data1"]], 4))
#>   Variable    Dimensions DimSize DataShape No.Col No.Obs
#> 1      pds      COMM*REG       2     10x10     10     10
#> 2      pfd COMM*ACTS*REG       3  10x10x10    100     10
#> 3      pms      COMM*REG       2     10x10     10     10
```

Understanding the data structure is crucial for aggregating data across
multiple experiments (inputs). Even when using the same variables,
different experiments may have varying column sizes or data structures
due to factors such as subtotal effects and other experimental settings.
These discrepancies can lead to errors if the merging process relies
solely on variable names without accounting for structural differences.

To compare data structures across multiple experiments, use the
following command:

``` r
# (1) Comparing all variable structures across experiments 
vars_comparison <- compare_var_structure(
  variables = "ALL", sl4_data1, sl4_data2
)
print(vars_comparison$match[1:2, ])
#>   Variable    Dimensions DataShape input1_ColSize input2_ColSize
#> 1      afa COMM*ACTS*REG  10x10x10            100            100
#> 2    afall COMM*ACTS*REG  10x10x10            100            100

# (2) Comparing selected variable structures across experiments 
var_comparison <- compare_var_structure(
  variables = c("pds", "pms"), sl4_data1, sl4_data2
)
print(var_comparison$match[1:2, ])
#>   Variable Dimensions DataShape input1_ColSize input2_ColSize
#> 1      pds   COMM*REG     10x10             10             10
#> 2      pms   COMM*REG     10x10             10             10
```

This function returns a list containing: - **match**: A data frame of
variables with identical dimension names and structures across inputs. -
**diff** (if any): A data frame listing variables with structural
mismatches. Note: If this list appears, you may need to focus on
handling these variables. It is designed to report potential
issue-causing variables when merging datasets.

Additionally, the function includes the `<keep_unique>` option (default:
`FALSE`) which allows users to extract variables with unique names and
structures across all inputs. This is particularly useful when inputs
contain different sets of variables that need to be combined into a
final dataset.

``` r
# (3) Extracting unique variable structures
unique_vars <- compare_var_structure(, 
                                     sl4_data1, sl4_data2,
  keep_unique = TRUE
)
print(unique_vars$match[1:10, ])
#>    Variable    Dimensions DimSize DataShape
#> 1       afa COMM*ACTS*REG       3  10x10x10
#> 2     afall COMM*ACTS*REG       3  10x10x10
#> 3     afcom          COMM       1        10
#> 4       afe ENDW*ACTS*REG       3   5x10x10
#> 5    afeall ENDW*ACTS*REG       3   5x10x10
#> 6    afecom          ENDW       1         5
#> 7    afereg           REG       1        10
#> 8    afesec          ACTS       1        10
#> 9     afreg           REG       1        10
#> 10    afsec          ACTS       1        10
```

This function returns a list containing: - **match**: A data frame
displaying distinct variable structures found across inputs. - **diff**
(if any): A data frame detailing how structures differ between inputs.

To sum up, - `keep_unique = FALSE` → Checks whether variables match
across inputs. - `keep_unique = TRUE` → Extracts all unique variable
structures, regardless of whether they match, while reports any
variables that do not align.

## Dimension Structure

The following commands can be used to retrieve unique dimension names as
patterns and their corresponding elements:

``` r
# (1) Extracting dimension patterns (e.g., REG*COMM*ACTS)
dims_strg_har <- get_dim_patterns(har_data1, har_data2)
print(dims_strg_har[1:4, ])
#> [1] "scalar"     "scalar"     "scalar"     "REG*COLUMN"

# (2) Extracting unique dimension patterns (e.g., REG*COMM*ACTS)
dims_strg_har <- get_dim_patterns(har_data1, har_data2,
                                  keep_unique =TRUE)
print(dims_strg_har[1:4, ])
#> [1] "scalar"       "REG*COLUMN"   "ALLOCEFF*REG" "REG*CTAX"

# (2) Extracting dimension elements e.g., REG, COMM, ACTS
dims_strg_har_uniq <- get_dim_elements(har_data1, har_data2, 
                                       keep_unique =TRUE)
print(dims_strg_har_uniq[1:4, ])
#> [1] "REG"      "COLUMN"   "ALLOCEFF" "CTAX"
```

### Patterns vs. Elements

Patterns represent structured dimension names (e.g., `"REG*COMM*ACTS"`),
while elements extract individual dimension elements (e.g., `"REG"`,
`"COMM"`, `"ACTS"`).
