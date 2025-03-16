pkgname <- "HARplus"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('HARplus')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("compare_var_structure")
### * compare_var_structure

flush(stderr()); flush(stdout())

### Name: compare_var_structure
### Title: Compare Variable Structures Across SL4 and HAR Objects
### Aliases: compare_var_structure

### ** Examples

# Import sample data:
har_data1 <- load_harx(system.file("extdata", "TAR10-WEL.har", package = "HARplus"))
har_data2 <- load_harx(system.file("extdata", "SUBT10-WEL.har", package = "HARplus"))

# Compare structure for a single variable across multiple datasets
compare_var_structure("A", har_data1, har_data2)

# Compare structure for multiple variables across multiple datasets
comparison_multiple <- compare_var_structure(c("A", "E1"), har_data1, har_data2)

# Extract unique variable structures across multiple datasets
unique_vars <- compare_var_structure("ALL", har_data1, har_data2, keep_unique = TRUE)




cleanEx()
nameEx("export_data")
### * export_data

flush(stderr()); flush(stdout())

### Name: export_data
### Title: Export Data to Various Formats (CSV/STATA/TEXT/RDS/XLSX)
### Aliases: export_data

### ** Examples

## Not run: 
##D # Import sample data:
##D sl4_data <- load_sl4x(system.file("extdata", "TAR10.sl4", package = "HARplus"))
##D 
##D # Extract data
##D data_multiple <- get_data_by_var(c("qo", "pca"), sl4_data)
##D 
##D # Export
##D export_data(data_multiple, file.path(tempdir(), "output_directory"), 
##D            format = c("csv", "xlsx", "stata", "txt", "rds"),
##D            create_subfolder = TRUE,
##D            multi_sheet_xlsx = TRUE)
## End(Not run)




cleanEx()
nameEx("get_data_by_dims")
### * get_data_by_dims

flush(stderr()); flush(stdout())

### Name: get_data_by_dims
### Title: Extract Data by Dimension Patterns from SL4 or HAR Objects
### Aliases: get_data_by_dims

### ** Examples

# Import sample data:
sl4_data <- load_sl4x(
  system.file("extdata", "TAR10.sl4", package = "HARplus")
)
sl4_data1 <- load_sl4x(
  system.file("extdata", "SUBT10.sl4", package = "HARplus")
)

# Extract data for a single dimension pattern
data_single_pattern <- get_data_by_dims(
  "comm*reg",
  sl4_data
)

# Extract multiple dimension patterns
data_multiple_patterns <- get_data_by_dims(
  c("comm*reg", "REG*ACTS"),
  sl4_data
)

# Extract all dimension patterns separately from multiple datasets
data_all_patterns <- get_data_by_dims(
  NULL,
  sl4_data, sl4_data1,
  merge_data = FALSE
)

# Merge data for identical patterns across multiple datasets
data_merged_patterns <- get_data_by_dims(
  NULL,
  sl4_data, sl4_data1,
  merge_data = TRUE
)

# Merge data while allowing interchangeable dimensions (e.g., A*B = B*A)
data_pattern_mixed <- get_data_by_dims(
  NULL,
  sl4_data, sl4_data1,
  merge_data = TRUE,
  pattern_mix = TRUE
)

# Retain only "TOTAL" values
data_total_only <- get_data_by_dims(
  "comm*reg",
  sl4_data,
  subtotal_level = "total"
)
data_total_only_alt <- get_data_by_dims(
  "comm*reg",
  sl4_data,
  subtotal_level = FALSE
)

# Retain only decomposed components
data_decomposed_only <- get_data_by_dims(
  "comm*reg",
  sl4_data,
  subtotal_level = "decomposed"
)

# Retain all value levels
data_all_decomp <- get_data_by_dims(
  "comm*reg",
  sl4_data,
  subtotal_level = "all"
)
data_all_decomp_alt <- get_data_by_dims(
  "comm*reg",
  sl4_data,
  subtotal_level = TRUE
)

# Rename specific columns
data_renamed <- get_data_by_dims(
  "comm*reg",
  sl4_data,
  rename_cols = c(REG = "Region", COMM = "Commodity")
)

# Merge data with custom experiment names
data_merged_experiments <- get_data_by_dims(
  "comm*reg",
  sl4_data, sl4_data1,
  experiment_names = c("EXP1", "EXP2"),
  merge_data = TRUE
)




cleanEx()
nameEx("get_data_by_var")
### * get_data_by_var

flush(stderr()); flush(stdout())

### Name: get_data_by_var
### Title: Extract Variable Data from SL4 or HAR Objects
### Aliases: get_data_by_var

### ** Examples

# Import sample data:
sl4_data <- load_sl4x(system.file("extdata", "TAR10.sl4", package = "HARplus"))
sl4_data1 <- load_sl4x(system.file("extdata", "SUBT10.sl4", package = "HARplus"))

# Extract a single variable
data_qo <- get_data_by_var("qo", sl4_data)

# Extract multiple variables
data_multiple <- get_data_by_var(c("qo", "qgdp"), sl4_data)

# Extract all variables separately from multiple datasets
data_all <- get_data_by_var(NULL, sl4_data, sl4_data1, merge_data = FALSE)

# Merge variable data across multiple datasets
data_merged <- get_data_by_var(NULL, sl4_data, sl4_data1, merge_data = TRUE)

# Retain only "TOTAL" values, removing decomposed components (subtotal_level = "total" or FALSE)
data_total_only <- get_data_by_var("qo", sl4_data, subtotal_level = "total")
data_total_only_alt <- get_data_by_var("qo", sl4_data, subtotal_level = FALSE)

# Retain only decomposed components, removing "TOTAL" (subtotal_level = "decomposed")
data_decomposed_only <- get_data_by_var("qo", sl4_data, subtotal_level = "decomposed")

# Retain all value levels (subtotal_level = "all" or TRUE)
data_all_decomp <- get_data_by_var("qo", sl4_data, subtotal_level = "all")
data_all_decomp_alt <- get_data_by_var("qo", sl4_data, subtotal_level = TRUE)

# Rename specific columns
data_renamed <- get_data_by_var("qo", sl4_data, rename_cols = c(REG = "Region", COMM = "Commodity"))

# Merge data across multiple datasets with custom experiment names
data_merged_experiments <- get_data_by_var("qo", sl4_data, sl4_data1,
experiment_names = c("EXP1", "EXP2"),
merge_data = TRUE)




cleanEx()
nameEx("get_dim_elements")
### * get_dim_elements

flush(stderr()); flush(stdout())

### Name: get_dim_elements
### Title: Get Dimension Elements from SL4 and HAR Objects
### Aliases: get_dim_elements

### ** Examples

# Import sample data:
sl4_data1 <- load_sl4x(system.file("extdata", "TAR10.sl4", package = "HARplus"))
sl4_data2 <- load_sl4x(system.file("extdata", "SUBT10.sl4", package = "HARplus"))


# Extract dimension elements from a single dataset
get_dim_elements(sl4_data1)

# Extract dimension elements from multiple datasets
get_dim_elements(sl4_data1, sl4_data2)

# Extract unique dimension elements across datasets
get_dim_elements(sl4_data1, sl4_data2, keep_unique = TRUE)




cleanEx()
nameEx("get_dim_patterns")
### * get_dim_patterns

flush(stderr()); flush(stdout())

### Name: get_dim_patterns
### Title: Get Dimension Patterns from SL4 and HAR Objects
### Aliases: get_dim_patterns

### ** Examples

# Import sample data:
sl4_data <- load_sl4x(system.file("extdata", "TAR10.sl4", package = "HARplus"))
sl4_data2 <- load_sl4x(system.file("extdata", "SUBT10.sl4", package = "HARplus"))

# Extract dimension patterns
get_dim_patterns(sl4_data)

# Extract only unique dimension patterns across datasets
get_dim_patterns(sl4_data, sl4_data2, keep_unique = TRUE)




cleanEx()
nameEx("get_var_structure")
### * get_var_structure

flush(stderr()); flush(stdout())

### Name: get_var_structure
### Title: Get Variable Structure Summary from SL4 and HAR Objects
### Aliases: get_var_structure

### ** Examples

# Import data sample:
sl4_data <- load_sl4x(system.file("extdata", "TAR10.sl4", package = "HARplus"))
sl4_data1 <- load_sl4x(system.file("extdata", "SUBT10.sl4", package = "HARplus"))

# Get summary for all variables in a single dataset
get_var_structure(data_obj = sl4_data)

# Get summary for specific variables
get_var_structure(c("gdp", "trade"), sl4_data)

# Include column and observation counts
get_var_structure("ALL", sl4_data, include_col_size = TRUE)

# Compare structures across multiple datasets
get_var_structure("ALL", sl4_data, sl4_data1)

# Include column and observation counts across multiple datasets
get_var_structure("ALL", sl4_data, sl4_data1, include_col_size = TRUE)




cleanEx()
nameEx("group_data_by_dims")
### * group_data_by_dims

flush(stderr()); flush(stdout())

### Name: group_data_by_dims
### Title: Group Data by Dimension Patterns in SL4 or HAR Objects
### Aliases: group_data_by_dims

### ** Examples

# Import sample data
sl4_data1 <- load_sl4x(system.file("extdata", "TAR10.sl4", package = "HARplus"))
sl4_data2 <- load_sl4x(system.file("extdata", "SUBT10.sl4", package = "HARplus"))

# Case 1: Multiple priority levels (Sector then Region) with auto_rename
priority_list <- list(
  "Sector" = c("COMM", "ACTS"),
  "Region" = c("REG")
)
grouped_data_multiple <- group_data_by_dims(
  patterns = "ALL",
  sl4_data1,
  priority = priority_list,
  auto_rename = TRUE
)

# Case 2: Single priority (Region only) with auto_rename
priority_list <- list("Region" = c("REG"))
grouped_data_single <- group_data_by_dims(
  patterns = "ALL",
  sl4_data1, sl4_data2,
  priority = priority_list,
  auto_rename = TRUE
)

# Case 3: Multiple priorities without auto_rename
priority_list <- list(
  "Sector" = c("COMM", "ACTS"),
  "Region" = c("REG")
)
grouped_data_no_rename <- group_data_by_dims(
  patterns = "ALL",
  sl4_data1,
  priority = priority_list,
  auto_rename = FALSE
)



cleanEx()
nameEx("load_harx")
### * load_harx

flush(stderr()); flush(stdout())

### Name: load_harx
### Title: Load and Process HAR Files with Header Selection
### Aliases: load_harx

### ** Examples

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




cleanEx()
nameEx("load_sl4x")
### * load_sl4x

flush(stderr()); flush(stdout())

### Name: load_sl4x
### Title: Load and Process SL4 Files with Enhanced Options
### Aliases: load_sl4x

### ** Examples

# Path to example files
sl4_path <- system.file("extdata", "TAR10.sl4", package = "HARplus")

# Basic loading
sl4_data <- load_sl4x(sl4_path)

# Load with lowercase names
sl4_data_lower <- load_sl4x(sl4_path, lowercase = TRUE)

# Load specific headers
sl4_selected <- load_sl4x(sl4_path, select_header = c("qo", "qgdp"))




cleanEx()
nameEx("pivot_data")
### * pivot_data

flush(stderr()); flush(stdout())

### Name: pivot_data
### Title: Pivot Data from SL4 or HAR Objects
### Aliases: pivot_data

### ** Examples

# Import sample data:
sl4_data <- load_sl4x(system.file("extdata", "TAR10.sl4", package = "HARplus"))

# Extract multiple variables
data_multiple <- get_data_by_var(c("qo", "qxs"), sl4_data)

# Pivot a single column
pivoted_data <- pivot_data(data_multiple, pivot_cols = "REG")

# Pivot multiple columns
pivoted_data_multi <- pivot_data(data_multiple, pivot_cols = c("REG", "COMM"))




cleanEx()
nameEx("pivot_data_hierarchy")
### * pivot_data_hierarchy

flush(stderr()); flush(stdout())

### Name: pivot_data_hierarchy
### Title: Create Hierarchical Pivot Table from SL4 or HAR Objects
### Aliases: pivot_data_hierarchy

### ** Examples

## Not run: 
##D # Import sample data:
##D sl4_data <- load_sl4x(system.file("extdata", "TAR10.sl4", package = "HARplus"))
##D 
##D # Extract data
##D data_multiple <- get_data_by_var(c("qo", "pca"), sl4_data)
##D 
##D # Create hierarchical pivot without export
##D pivot_hier <- pivot_data_hierarchy(data_multiple, 
##D                                    pivot_cols = c("REG", "COMM"))
##D 
##D # Create and export to Excel in one step
##D pivot_export <- pivot_data_hierarchy(data_multiple, 
##D                                      pivot_cols = c("REG", "COMM"),
##D                                      export = TRUE,
##D                                      file_path = file.path(tempdir(), "pivot_output.xlsx"))
## End(Not run)




cleanEx()
nameEx("rename_dims")
### * rename_dims

flush(stderr()); flush(stdout())

### Name: rename_dims
### Title: Rename Dimensions in SL4 or HAR Objects
### Aliases: rename_dims

### ** Examples

# Import sample data:
sl4_data <- load_sl4x(system.file("extdata", "TAR10.sl4", package = "HARplus"))

# Define a renaming map
mapping_df <- data.frame(
  old = c("REG", "COMM"),
  new = c("Region", "Commodity")
)

# Rename columns in the dataset
rename_dims(sl4_data, mapping_df)

# Rename both columns and list names
rename_dims(sl4_data, mapping_df, rename_list_names = TRUE)




### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
