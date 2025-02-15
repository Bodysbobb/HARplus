# Example of Usage of read_harx -------------------------------------------------------------------

# Define the input folder path
input_folder <- "D:/GitHub/GTAP-Results-using-R/TestData/in"

# 1. Reading the data
har_data1 <- read_harx(file.path(input_folder, "ExB14-WEL.har"))
har_data2 <- read_harx(file.path(input_folder, "ExB15-WEL.har"))

# 2. Retrieving data structure
data_dims_har <- get_dims_har(har_data1)

# 3. Extracting data from a selected header

## Retrieve a summary of the variable, including its data, dimensions, and dimension sizes
var_summary_har <- get_var_summary_har(har_data1, "A")

## Extract a dataframe of the selected Header
var_data_har_E1 <- get_data_har(har_data1, "E1", 
                                add_experiment = TRUE,
                                experiment_name = "Exp1", 
                                rename_cols = c(REG = "Region",
                                                FORM = "Type"))


var_data_har_A <- get_data_har(har_data1, "A", 
                               add_experiment = TRUE,
                               experiment_name = "Exp1", 
                               rename_cols = c(REG = "Region"))


# 4. Extracting data from all header
var_data_har_C16 <- get_data_har(har_data1, "C16", 
                                 add_experiment = TRUE,
                                 experiment_name = "Exp1", 
                                 rename_cols = c(REG = "Region"))

# 5. Extracting datafram from the selected header across multiple HAR files
multi_data <- get_var_data_multi_har("A", har_data1, har_data2, 
                                     experiment_names = c("Scenario1", "Scenario2"))

# 6. Extracting datafram from all header across multiple HAR files
all_data <- get_all_multi_har(har_data1, har_data2, 
                              experiment_names = c("Scenario1", "Scenario2"))



# Example of Usage of read_sl4x --------------------------------------------------------

# Define the input folder path
input_folder <- "D:/GitHub/GTAP-Results-using-R/TestData/in"

# 1. Reading the data
sl4_data1 <- read_sl4x(file.path(input_folder, "ExB14.sl4"))
sl4_data2 <- read_sl4x(file.path(input_folder, "ExB15.sl4"))

# 2. Retrieving data structure
data_dims <- get_dims_summary(sl4_data1)

## Extracting unique element of dimension name
dim.element <- get_dims_elements(sl4_data1)

## Extracting unique strings of dimension name
dim.string <- get_dims_strings(sl4_data1)


# 3. Extracting data for a selected variable

## Retrieve a summary of the variable, including its data, dimensions, and dimension sizes
var_summary <- get_var_summary(sl4_data1, "qo")

## Extract a dataframe of the selected variable
# (1) By default, `drop_subtotals = FALSE`. If set to `TRUE`, subtotal values are included.
#     Subtotals are indicated in the "Type" column if available; otherwise, the column will contain "TOTAL".
# (2) By default, the dataset does not include an "Experiment" column.
#     If `add_experiment = TRUE`, the experiment name can be specified.
var_data <- get_var_data(sl4_data1, "qo", drop_subtotals = FALSE, 
                         add_experiment = TRUE, experiment_name = "Exp1", 
                         rename_cols = c(REG = "Region"))


# Alternatively, extract data without adding an experiment column
var_data_no_exp_oricol <- get_var_data(sl4_data1, "qo")

# 4. Grouping data by a selected dimension within an experiment (solution file)
agg_dim <- group_by_dims(sl4_data1, "comm*reg", drop_subtotals = TRUE, 
                         add_experiment = TRUE, experiment_name = "Exp1", 
                         rename_cols = c(REG = "Region", COMM = "Sector"))

# 5. Grouping across all dimensions within an experiment
agg_all <- group_all(sl4_data1, drop_subtotals = FALSE, 
                     add_experiment = TRUE, experiment_name = "Exp1", 
                     rename_cols = c(REG = "Region", COMM = "Sector"))

# 6. Extracting a dataframe of the selected variable across multiple solution files
# By default:
# - The experiment name is assigned based on the dataset name.
# - The "Type" column will automatically include subtotals if at least one dataset contains them.
# - Subtotals can only be suppressed if all datasets do not include them.
data_multi <- get_var_data_multi("qo", sl4_data1, sl4_data2, 
                                 experiment_names = c("EXP1", "EXP2"), 
                                 drop_subtotals = FALSE, 
                                 rename_cols = c(REG = "Region", COMM = "Sector"))
# Althernatively
data_multi_oricol <- get_var_data_multi("qo", sl4_data1, sl4_data2, 
                                        experiment_names = c("EXP1", "EXP2"), 
                                        drop_subtotals = FALSE)


# 7. Grouping by a selected dimension across multiple experiments
agg_dim_multi <- group_by_dims_multi("comm*reg", sl4_data1, sl4_data2, 
                                     experiment_names = c("EXP1", "EXP2"), 
                                     drop_subtotals = FALSE, 
                                     rename_cols = c(REG = "Region", COMM = "Sector"))

# 8. Grouping across all dimensions across multiple experiments
agg_all_multi <- group_all_multi(sl4_data1, sl4_data2,
                                 experiment_names = c("Test1", "Test2"), 
                                 rename_cols = c(REG = "Region", COMM = "Sector"))


# 9. Combining data based on defined priority
regrouped_data <- combine_by_dims(
  agg_all_multi,
  dimension_map = data.frame(
    dimension = c("COMM", "ACTS", "REG"),
    group = c("Sector", "Sector", "Region"),
    priority = c(1, 1, 2)
  )
)