## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning = FALSE,
  message = FALSE,
  eval = requireNamespace("HARplus", quietly = TRUE)
)

## ----load package-------------------------------------------------------------
library(HARplus)

## ----load data----------------------------------------------------------------
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

## ----get_data_by_var----------------------------------------------------------
# Extract data for a single variable
data_qo <- get_data_by_var("qo", sl4_data1)
print(head(data_qo[["sl4_data1"]][["qo"]], 4))

# Extract multiple variables from multiple datasets
data_multiple <- get_data_by_var(c("qo", "qgdp"), sl4_data1, sl4_data2)
print(head(data_multiple[["sl4_data1"]][["qo"]], 4))
print(head(data_multiple[["sl4_data2"]][["qo"]], 4))

# Extract all variables separately from multiple datasets
data_list <- get_data_by_var(NULL, sl4_data1, sl4_data2)
print(names(data_list))

# Extract all variables and merge the same variable from multiple datasets
data_all <- get_data_by_var(NULL, sl4_data1, sl4_data2, merge_data = TRUE)
print(names(data_all))

# Return all value levels
data_all <- get_data_by_var("qo", sl4_data1, sl4_data2, subtotal_level = TRUE)
print(head(data_all[["sl4_data1"]][["qo"]], 4))

# Return only TOTAL, drop subtotal
data_total <- get_data_by_var("qo", sl4_data1, sl4_data2, subtotal_level = FALSE)
print(head(data_total[["sl4_data1"]][["qo"]], 4))

# Return only subtotal, drop TOTAL (result is empty if there in subtotal)
data_decomp <- get_data_by_var("qo", sl4_data1, sl4_data2, subtotal_level = "decomposed")
print(head(data_decomp[["sl4_data2"]][["qo"]], 4))

# Rename specific columns
data_col_renamed <- get_data_by_var("qo", sl4_data1, 
                             rename_cols = c(REG = "Region", COMM = "Commodity"))
str(data_col_renamed)

# Rename experiment names
data_exp_renamed <- get_data_by_var("qo", sl4_data1, sl4_data2, 
                             experiment_names = c("EXP1", "EXP2"))
print(names(data_exp_renamed))

# Merge variable data across multiple datasets with custom experiment names
data_merged <- get_data_by_var(, sl4_data1, sl4_data2,
                            experiment_names = c("EXP1", "EXP2"), 
                            merge_data = TRUE,
                            rename_cols = c(REG = "Region", COMM = "Commodity"))
print(head(data_merged$merged[[1]], 4))


## ----get_data_by_dims---------------------------------------------------------
# Merge data by dimensions (e.g., REG*COMM != COMM*REG)
data_no_mix <- get_data_by_dims(NULL, sl4_data1, sl4_data2, 
                                merge_data = TRUE, 
                                pattern_mix = FALSE)

# Merge data while allowing interchangeable dimensions (e.g., REG*COMM = COMM*REG)
data_pattern_mixed <- get_data_by_dims(NULL, sl4_data1, sl4_data2, 
                                       merge_data = TRUE, 
                                       pattern_mix = TRUE)

## ----group_data_single_priority-----------------------------------------------
# Define single priority (Only Region-based grouping)
priority_list <- list("Region" = c("REG"))

# Grouping data with a single priority
grouped_data_single <- group_data_by_dims("ALL", sl4_data1, sl4_data2,
                                          priority = priority_list, auto_rename = TRUE)

# Print structure
print(names(grouped_data_single))
print(names(grouped_data_single[["1D"]]))
print(names(grouped_data_single[["2D"]]))

## ----group_data_multi_priority------------------------------------------------
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
print(names(grouped_data_multiple[["1D"]]))
print(names(grouped_data_multiple[["2D"]]))

## ----group_data_no_auto_rename------------------------------------------------
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
print(names(grouped_data_no_rename[["1D"]]))
print(names(grouped_data_no_rename[["2D"]]))

## ----pivot--------------------------------------------------------------------
# Extract data to pivot
data_multiple <- get_data_by_var(c("qo", "pca"), sl4_data1)

# Pivot a single column
pivoted_single <- pivot_data(data_multiple, 
                             pivot_cols = "REG")

# Pivot multiple columns
pivoted_multi <- pivot_data(data_multiple, 
                            pivot_cols = c("COMM", "REG"))


## ----pivot_data_hierarchy, eval=FALSE-----------------------------------------
# # Create hierarchical pivot without export
# pivot_hier <- pivot_data_hierarchy(data_multiple,
#                                   pivot_cols = c("REG", "COMM"))
# 
# # Create and export to Excel in one step
# pivot_export <- pivot_data_hierarchy(data_multiple,
#                                    pivot_cols = c("REG", "COMM"),
#                                    export = TRUE,
#                                    file_path = file.path(tempdir(), "pivot_output.xlsx"))
# 

## ----rename-------------------------------------------------------------------
# Define a renaming map
mapping_df <- data.frame(
 old = c("REG", "COMM"),
 new = c("Region", "Commodity")
)

# Rename dimensions only
renamed_dims <- rename_dims(sl4_data1, mapping_df)

# Rename both dimensions and list names
renamed_both <- rename_dims(sl4_data1, mapping_df, rename_list_names = TRUE)

## ----export data, eval=FALSE--------------------------------------------------
# # Extract data
# data_multiple <- get_data_by_var(c("qo", "pca"), sl4_data1)
# 
# # Export
# export_data(data_multiple, file.path(tempdir(), "output_directory"),
#            format = c("csv", "xlsx", "stata", "txt", "rds"),
#            create_subfolder = TRUE,
#            multi_sheet_xlsx = TRUE)

## ----get_var_structure--------------------------------------------------------
# (1) Getting all variables from the input file
vars_har_sum <- get_var_structure("ALL", har_data1)
vars_sl4_sum <- get_var_structure(, har_data1)

# (2) Getting selected variables
var_sl4_sum <- get_var_structure(c("pds","pfd","pms"), sl4_data1)
print(head(var_sl4_sum[["sl4_data1"]], 4))


# (3) Including column size and number of observation in the summary
var_sl4_sum <- get_var_structure(c("pds","pfd","pms"), sl4_data1, sl4_data2, 
                                 include_col_size = TRUE)
print(head(var_sl4_sum[["sl4_data1"]], 4))

## ----compare_var_structure----------------------------------------------------
# (1) Comparing all variable structures across experiments 
vars_comparison <- compare_var_structure(
  variables = "ALL", sl4_data1, sl4_data2
)
print(vars_comparison$match[1:2, ])

# (2) Comparing selected variable structures across experiments 
var_comparison <- compare_var_structure(
  variables = c("pds", "pms"), sl4_data1, sl4_data2
)
print(var_comparison$match[1:2, ])

## ----unique_compare_var_structure---------------------------------------------
# (3) Extracting unique variable structures
unique_vars <- compare_var_structure(, 
                                     sl4_data1, sl4_data2,
  keep_unique = TRUE
)
print(unique_vars$match[1:10, ])

## ----get_dim_patterns---------------------------------------------------------
# (1) Extracting dimension patterns (e.g., REG*COMM*ACTS)
dims_strg_har <- get_dim_patterns(har_data1, har_data2)
print(dims_strg_har[1:4, ])

# (2) Extracting unique dimension patterns (e.g., REG*COMM*ACTS)
dims_strg_har <- get_dim_patterns(har_data1, har_data2,
                                  keep_unique =TRUE)
print(dims_strg_har[1:4, ])

# (2) Extracting dimension elements e.g., REG, COMM, ACTS
dims_strg_har_uniq <- get_dim_elements(har_data1, har_data2, 
                                       keep_unique =TRUE)
print(dims_strg_har_uniq[1:4, ])


