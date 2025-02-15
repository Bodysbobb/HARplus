# Example of Usage of read_harx ===============================================
# Load example HAR files
input_folder <- "C:/GitHub/GTAP-Results-using-R/TestData/in"
har_data1 <- read_harx(file.path(input_folder, "ExB14-WEL.har"))
har_data2 <- read_harx(file.path(input_folder, "ExB15-WEL.har"))
# (1) One header, one experiment ----------------------------------------

# Default experiment name (will use "har_data1")
one.head.basic <- get_har_data("A", har_data1)

# With custom experiment name
one.head.named <- get_har_data("A", har_data1, 
                               experiment_names = "baseline")

# With column renaming
one.head.renamed <- get_har_data("A", har_data1, 
                                 experiment_names = "baseline",
                                 rename_cols = c(REG = "Region"))

# (2) Multiple headers, one experiment ---------------------------------

# Default experiment name
multi.head.basic <- get_har_data(c("A", "A1", "E1"), 
                                 har_data1)

# With custom experiment name
multi.head.named <- get_har_data(
  c("A", "A1", "E1"),
  har_data1,
  experiment_names = "baseline"
)

# With column renaming
multi.head.renamed <- get_har_data(
  c("A", "A1", "E1"),
  har_data1,
  experiment_names = "baseline",
  rename_cols = c(REG = "Region", COMM = "Commodity")
)

# (3) One header, multiple experiments --------------------------------

# Default experiment names (will use "har_data1" and "har_data2")
one.head.multi.basic <- get_har_data("A",
                                     har_data1, har_data2)

# Custom experiment names
one.head.multi.named <- get_har_data("A",
                                     har_data1, har_data2,
                                     experiment_names = c("baseline", "policy"))

# With column renaming
one.head.multi.renamed <- get_har_data("A",
                                       har_data1, har_data2,
                                       experiment_names = c("baseline", "policy"),
                                       rename_cols = c(REG = "Region"))

# (4) Multiple headers, multiple experiments --------------------------

# Default experiment names
multi.head.multi.basic <- get_har_data(
  c("A", "A1", "E1"),
  har_data1, har_data2
)

# Custom experiment names
multi.head.multi.named <- get_har_data(
  c("A", "A1", "E1"),
  har_data1, har_data2,
  experiment_names = c("baseline", "policy")
)

# Full example with all options
multi.head.multi.full <- get_har_data(
  c("A", "A1", "E1"),
  har_data1, har_data2,
  experiment_names = c("baseline", "policy"),
  drop_subtotals = TRUE,
  rename_cols = c(REG = "Region", COMM = "Commodity")
)

# (5) Extract all headers --------------------------------------------

# From single experiment
all.head.one <- get_har_data(NULL, har_data1)

# From multiple experiments
all.head.multi <- get_har_data(NULL, 
                               har_data1, har_data2,
                               experiment_names = c("baseline", "policy"))



# EXAMPLE OF <read_sl4x> ===============================================
# Reading the data
sl4_data1 <- read_sl4x(file.path(input_folder, "ExB14.sl4"))
sl4_data2 <- read_sl4x(file.path(input_folder, "ExB15.sl4"))

# EXAMPLE OF <get_sl4_data> ===============================================

# (1) One variable, one experiment ----------------------------------------
# Default experiment name
one.var.oricol <- get_sl4_data("qo", sl4_data1)

# With custom experiment name
one.var.oricol.exp <- get_sl4_data("qo", sl4_data1, 
                                    experiment_names = "exp1")

# With column renaming
one.var.recol <- get_sl4_data("qo", sl4_data1, 
                               experiment_names = "exp1", 
                               rename_cols = c(REG = "Region"))

# With dropping subtotals
one.var.no.subtotals <- get_sl4_data("qo", sl4_data1,
                                      experiment_names = "exp1",
                                      drop_subtotals = TRUE)

# (2) Multiple variables, one experiment ---------------------------------
# Default experiment name
multi.var.one.exp <- get_sl4_data(c("qo", "qgdp", "EV", "u"), 
                                   sl4_data1)

# With custom experiment name
multi.var.one.exp.named <- get_sl4_data(c("qo", "qgdp", "EV", "u"), 
                                         sl4_data1, 
                                         experiment_names = "baseline")

# With column renaming
multi.var.one.exp.renamed <- get_sl4_data(c("qo", "qgdp", "EV", "u"), 
                                           sl4_data1,
                                           experiment_names = "baseline",
                                           rename_cols = c(REG = "Region", 
                                                           COMM = "Commodity"))

# (3) One variable, multiple experiments --------------------------------
# Default experiment names
one.var.multi.exp <- get_sl4_data("qo", 
                                   sl4_data1, sl4_data2)

# Custom experiment names
one.var.multi.exp.named <- get_sl4_data("qo",
                                         sl4_data1, sl4_data2,
                                         experiment_names = c("baseline", "policy"))

# With column renaming
one.var.multi.exp.renamed <- get_sl4_data("qo",
                                           sl4_data1, sl4_data2,
                                           experiment_names = c("baseline", "policy"),
                                           rename_cols = c(REG = "Region"))

# (4) Multiple variables, multiple experiments --------------------------
# Default experiment names
multi.var.multi.exp <- get_sl4_data(c("qo", "qgdp", "EV", "u"),
                                     sl4_data1, sl4_data2)

# Custom experiment names
multi.var.multi.exp.named <- get_sl4_data(c("qo", "qgdp", "EV", "u"),
                                           sl4_data1, sl4_data2,
                                           experiment_names = c("baseline", "policy"))

# With column renaming and dropping subtotals
multi.var.multi.exp.full <- get_sl4_data(c("qo", "qgdp", "EV", "u"),
                                          sl4_data1, sl4_data2,
                                          experiment_names = c("baseline", "policy"),
                                          drop_subtotals = TRUE,
                                          rename_cols = c(REG = "Region", 
                                                          COMM = "Commodity"))


# EXAMPLE OF <extract_by_dims > ===============================================

# (1) One pattern, one experiment ----------------------------------------
# Default experiment name
one.pat.oricol <- extract_by_dims ("comm*reg", sl4_data1)

# With custom experiment name
one.pat.oricol.exp <- extract_by_dims ("comm*reg", sl4_data1, 
                                     experiment_names = "exp1")

# With column renaming
one.pat.recol <- extract_by_dims ("comm*reg", sl4_data1, 
                                experiment_names = "exp1",
                                rename_cols = c(REG = "Region"))

# (2) Multiple patterns, one experiment ---------------------------------
# Default experiment name
multi.pat.one.exp <- extract_by_dims (c("comm*reg", "comm*reg*reg", "reg", "COMM"), 
                                    sl4_data1)

# With custom experiment name
multi.pat.one.exp.named <- extract_by_dims (
  c("comm*reg", "comm*reg*reg", "reg", "COMM"),
  sl4_data1,
  experiment_names = "baseline"
)

# With column renaming
multi.pat.one.exp.renamed <- extract_by_dims (
  c("comm*reg", "comm*reg*reg", "reg", "COMM"),
  sl4_data1,
  experiment_names = "baseline",
  rename_cols = c(REG = "Region", COMM = "Commodity")
)

# (3) One pattern, multiple experiments --------------------------------
# Default experiment names
one.pat.multi.exp <- extract_by_dims ("comm*reg",
                                    sl4_data1, sl4_data2)

# Custom experiment names
one.pat.multi.exp.named <- extract_by_dims ("comm*reg",
                                          sl4_data1, sl4_data2,
                                          experiment_names = c("baseline", "policy"))

# With column renaming
one.pat.multi.exp.renamed <- extract_by_dims ("comm*reg",
                                            sl4_data1, sl4_data2,
                                            experiment_names = c("baseline", "policy"),
                                            rename_cols = c(REG = "Region"))

# (4) Multiple patterns, multiple experiments --------------------------
# Default experiment names
multi.pat.multi.exp <- extract_by_dims (
  c("comm*reg", "comm*reg*reg", "reg", "COMM"),
  sl4_data1, sl4_data2
)

# Custom experiment names
multi.pat.multi.exp.named <- extract_by_dims (
  c("comm*reg", "comm*reg*reg", "reg", "COMM"),
  sl4_data1, sl4_data2,
  experiment_names = c("baseline", "policy")
)

# With column renaming and dropping subtotals
multi.pat.multi.exp.full <- extract_by_dims (
  c("comm*reg", "comm*reg*reg", "reg", "COMM"),
  sl4_data1, sl4_data2,
  experiment_names = c("baseline", "policy"),
  drop_subtotals = FALSE,
  rename_cols = c(REG = "Region", COMM = "Commodity")
)

# (5) Extract all patterns --------------------------------------------
# From single experiment
all.pat.one.exp <- extract_by_dims (NULL, sl4_data1)

# From multiple experiments
all.pat.multi.exp <- extract_by_dims (NULL, sl4_data1, sl4_data2, 
                                    experiment_names = c("baseline", "policy"))


# EXAMPLE OF <group_by_dims> ===============================================
regrouped_data <- group_by_dims(
  all.pat.multi.exp,  
  dimension_map = data.frame(
    dimension = c("COMM", "ACTS", "REG"),
    group = c("Sector", "Sector", "Region"),
    priority = c(1, 1, 2)
  )
)

# EXAMPLE OF <group_by_dims_from_sl4> ===============================================
result <- group_by_dims_from_sl4(
  sl4_data1, sl4_data2,
  dimension_map = data.frame(
    dimension = c("COMM", "ACTS", "REG"),
    group = c("Sector", "Sector", "Region"),
    priority = c(1, 1, 2)
  )
)