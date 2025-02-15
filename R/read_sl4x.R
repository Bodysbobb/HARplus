#' Read and Process SL4 Data
#' 
#' Reads an SL4 file and processes its structured data into an enhanced SL4 object. 
#' Extracts metadata, dimensions, and variables while handling subtotal information.
#' 
#' @param filename The path to the SL4 file.
#' @param toLowerCase Logical; whether to convert variable names to lowercase.
#' @return An enhanced SL4 object containing structured data, metadata, and dimension details.
#' @export
#' @importFrom HARr read_har
#' @examples
#' # ─── Load SL4 Data ─────────────────────────────────────────────────
#' sl4_path <- "path/to/file.sl4"
#' sl4_data <- read_sl4x(sl4_path)
#' 
#' # ─── Load SL4 Data with lowercase -─────────────────────────────────
#' sl4_data <- read_sl4x(sl4_path, toLowerCase = TRUE)
read_sl4x <- function(filename, toLowerCase = FALSE) {
  if (!file.exists(filename)) {
    stop(sprintf("File '%s' does not exist", filename))
  }
  
  solution <- HARr::read_har(filename, toLowerCase = toLowerCase)
  subtotals <- c('TOTAL', solution$STDS)
  dimension_info <- list()
  
  for(v in 1:length(solution$VCNM)) {
    var_name <- solution$VCNM[v]
    if(solution$VCNI[v] > 0) {
      dim_indices <- solution$VCSP[v]:(solution$VCSP[v] + solution$VCNI[v] - 1)
      dimensions <- solution$VCSN[dim_indices]
      dim_names <- solution$STNM[dimensions]
      dim_sizes <- solution$SSZ[dimensions]
      dim_string <- paste(dim_names, collapse="*")
      
      dimension_info[[var_name]] <- list(
        dimension_string = dim_string,
        dimension_names = dim_names,
        dimension_sizes = dim_sizes
      )
    }
  }
  
  results <- Map(function(f) {
    if(solution$VCNI[f] > 0) {
      dimensions <- solution$VCSN[solution$VCSP[f]:(solution$VCSP[f]+solution$VCNI[f]-1)]
      sizes <- c(solution$SSZ[dimensions], length(subtotals))
      labels <- c(
        Map(
          function(g) if(solution$SSZ[g]==0) c() else solution$STEL[solution$ELAD[g]:(solution$ELAD[g]+solution$SSZ[g]-1)], 
          dimensions
        ),
        list(subtotals)
      )
      names(labels) <- c(solution$STNM[dimensions], 'subtotal')
    } else {
      sizes <- c(length(subtotals))
      labels <- list(subtotals)
      names(labels) <- c('subtotal')
    }
    array(NA, dim = sizes, dimnames = labels)
  }, 1:length(solution$VCNM))
  
  names(results) <- solution$VCNM
  
  partials <- solution$OREX > 0 & solution$OREX != solution$VNCP
  stHeaders <- c('CUMS', unlist(Map(function(f) 
    sprintf('%sS',formatC(f, width=3, zero.print = TRUE, flag = "0")), 
    1:length(solution$STDS))))
  
  for (v in which(partials == FALSE & solution$PCUM>0)) {
    range <- solution$PCUM[v]:(solution$PCUM[v]+solution$ORND[v]-1)
    results[[solution$VARS[v]]][] <- unlist(Map(function(f) solution[[f]][range], stHeaders))
  }
  
  for (v in which(partials == FALSE & solution$PCUM==0)) {
    results[[solution$VARS[v]]][] <- 0
  }
  
  start <- 1
  for (v in which(partials == TRUE & solution$PCUM>0)) {
    range <- solution$PCUM[v]:(solution$PCUM[v]+solution$ORND[v]-1)
    positions <- solution$ORNL[start - 1 + (1:solution$ORND[v])]
    start <- solution$ORND[v]+start
    toFill <- rep(FALSE, solution$VNCP[v])
    toFill[positions] <- TRUE
    results[[solution$VARS[v]]][toFill] <- unlist(Map(function(f) solution[[f]][range], stHeaders))
  }
  
  enhanced_results <- list(
    data = results,
    dimension_info = dimension_info,
    metadata = list(
      filename = filename,
      creation_date = as.character(Sys.time()),
      variables = solution$VCNM
    )
  )
  
  class(enhanced_results) <- "enhanced_sl4"
  return(enhanced_results)
}


#' ─────────────────────────────────────────────────────────────────────────────
#' Get Variable Summary from SL4 Object
#' 
#' Generates a summary of the variables within an enhanced SL4 object, 
#' listing their dimension sizes and structures.
#' 
#' @param sl4_obj An enhanced SL4 object created using `read_sl4x()`.
#' @return A data frame containing variable names, dimension counts, and dimension strings.
#' @export
#' @examples
#' # ─── Load SL4 Data ───────────────────────────────────────────────────
#' sl4_data <- read_sl4x("path/to/sl4file.sl4")
#'
#' # ─── Get Summary of All Variables ────────────────────────────────────
#' var_summary <- get_dims_summary(sl4_data)
get_dims_summary <- function(sl4_obj) {
  variables <- character(0)
  sizes <- numeric(0)
  dimensions <- character(0)
  
  for (var_name in names(sl4_obj$dimension_info)) {
    dim_info <- sl4_obj$dimension_info[[var_name]]
    variables <- c(variables, var_name)
    sizes <- c(sizes, length(dim_info$dimension_names))
    dimensions <- c(dimensions, dim_info$dimension_string)
  }
  
  summary_df <- data.frame(
    Variable = variables,
    Size = sizes,
    Dimensions = dimensions,
    stringsAsFactors = FALSE,
    row.names = NULL
  )
  
  summary_df[order(variables), ]
}


#' ─────────────────────────────────────────────────────────────────────────────
#' Get Dimension Information for a Variable
#' 
#' Retrieves detailed dimension information for a specific variable 
#' in an enhanced SL4 object, including its names and sizes.
#' 
#' @param sl4_obj An enhanced SL4 object created using `read_sl4x()`.
#' @param var_name The name of the variable to retrieve dimension details for.
#' @return A list containing dimension names, sizes, and structures for the specified variable.
#' @export
#' @examples
#' # ─── Load SL4 Data ───────────────────────────────────────────────────
#' sl4_data <- read_sl4x("path/to/sl4file.sl4")
#'
#' # ─── Get Summary of Selected Variable ────────────────────────────────
#' var_summary <- get_var_summary(sl4_data, "qo")
get_var_summary <- function(sl4_obj, var_name) {
  if(!var_name %in% names(sl4_obj$dimension_info)) {
    stop(sprintf("Variable '%s' not found", var_name))
  }
  return(sl4_obj$dimension_info[[var_name]])
}


#' ─────────────────────────────────────────────────────────────────────────────
#' Extract Unique Dimension Elements
#' 
#' Extracts and lists unique dimension elements present in an SL4 object.
#' 
#' @param sl4_obj A structured SL4 object containing dimension information.
#' @return A data frame containing unique dimension elements.
#' @export
#' @examples
#' # ─── Load SL4 Data ───────────────────────────────────────────────────
#' sl4_data <- read_sl4x("path/to/sl4file.sl4")
#'
#' # ─── Retrieve Unique Dimension Elements for Mapping ─────────────────
#' dim_elements <- get_dims_elements(sl4_data)
#'
#' # ─── Create a Mapping Data Frame for Renaming ──────────────────────
#' mapping_df <- data.frame(
#'   old = dim_elements$DimName,  # Extracted dimension names
#'   new = paste0("New_", dim_elements$DimName)  # Example: Adding "New_" prefix
#' )
#'
#' # ─── Apply Column Renaming ──────────────────────────────────────────
#' renamed_sl4_data <- rename_sl4col_all(sl4_data, mapping_df)
get_dims_elements <- function(sl4_obj) {
  dimensions <- character(0)
  
  for (var_name in names(sl4_obj$dimension_info)) {
    dim_info <- sl4_obj$dimension_info[[var_name]]
    dimensions <- c(dimensions, dim_info$dimension_string)
  }
  
  unique_dim_names <- unique(unlist(strsplit(dimensions, "\\*")))
  
  return(data.frame(DimName = unique_dim_names, stringsAsFactors = FALSE))
}


#' ─────────────────────────────────────────────────────────────────────────────
#' Extract Unique Dimension Strings
#' 
#' Extracts and lists unique dimension strings present in an SL4 object.
#' 
#' @param sl4_obj A structured SL4 object containing dimension information.
#' @return A data frame containing unique dimension strings.
#' @export
#' @examples
#' # ─── Load SL4 Data ───────────────────────────────────────────────────
#' sl4_data <- read_sl4x("path/to/sl4file.sl4")
#'
#' # ─── Retrieve Unique Dimension Strings ───────────────────────────────
#' dim_strings <- get_dims_strings(sl4_data)
get_dims_strings <- function(sl4_obj) {
  dimensions <- character(0)
  
  for (var_name in names(sl4_obj$dimension_info)) {
    dim_info <- sl4_obj$dimension_info[[var_name]]
    dimensions <- c(dimensions, dim_info$dimension_string)
  }
  
  unique_dim_names <- unique(dimensions)
  
  return(data.frame(DimName = unique_dim_names, stringsAsFactors = FALSE))
}


#' ─────────────────────────────────────────────────────────────────────────────
#' Rename Data Frame Columns Based on Mapping
#' 
#' Renames the columns of a data frame within an SL4 object according to a specified mapping, 
#' ensuring that duplicate names are avoided by appending numeric suffixes if necessary.
#' 
#' @param sl4_obj An enhanced SL4 object containing structured data.
#' @param mapping_df A two-column data frame where the first column contains old names, 
#'        and the second column contains new names.
#' @return An SL4 object with updated column names.
#' @export
#' @examples
#' # ─── Load SL4 Data ─────────────────────────────────────────────────
#' sl4_path <- "path/to/file.sl4"
#' sl4_obj <- read_sl4x(sl4_path)
#'
#' # ─── Retrieve Unique Dimension Elements ────────────────────────────
#' dim_elements <- get_dims_elements(sl4_obj)
#'
#' # ─── Create a Mapping Data Frame for Renaming ──────────────────────
#' mapping_df <- data.frame(
#'   old = dim_elements$DimName,  # Extracted dimension names
#'   new = paste0("New_", dim_elements$DimName)  # New names (Example)
#' )
#'
#' # ─── Rename Columns Using the Mapping ───────────────────────────────
#' renamed_sl4_obj <- rename_sl4col_all(sl4_obj, mapping_df)
rename_sl4col_all <- function(sl4_obj, mapping_df) {
  if (!is.list(sl4_obj) || !"data" %in% names(sl4_obj)) {
    stop("Invalid SL4 object: Expected a structured SL4 object with a 'data' component.")
  }
  if (!is.data.frame(mapping_df) || ncol(mapping_df) != 2) {
    stop("Invalid mapping_df: Expected a two-column data frame with old and new names.")
  }
  
  old_names <- mapping_df[[1]]
  new_names <- mapping_df[[2]]
  rename_cols <- setNames(new_names, old_names)
  
  # Rename columns in the SL4 object
  for (var_name in names(sl4_obj$data)) {
    df <- sl4_obj$data[[var_name]]
    
    if (!is.data.frame(df)) next  # Skip non-data frame elements
    
    for (old_name in names(rename_cols)) {
      matching_cols <- which(names(df) == old_name)
      if (length(matching_cols) > 0) {
        for (i in seq_along(matching_cols)) {
          new_name <- if (i == 1) rename_cols[old_name] else paste0(rename_cols[old_name], i - 1)
          names(df)[matching_cols[i]] <- new_name
        }
      }
    }
    
    sl4_obj$data[[var_name]] <- df  # Update the SL4 object with renamed data frame
  }
  
  return(sl4_obj)  # Return the modified SL4 object
}


#' ─────────────────────────────────────────────────────────────────────────────
#' Extract Data from SL4 Objects
#' 
#' Extracts structured data for one or more variables from one or more SL4 objects,
#' returning a list of data frames (one per variable) when multiple variables are requested.
#' 
#' @param var_names Character vector of variable names to extract. Use NULL to extract all variables.
#' @param ... One or more SL4 objects.
#' @param experiment_names Optional character vector of names for each SL4 object.
#' @param drop_subtotals Logical; whether to remove subtotal rows.
#' @param rename_cols A named vector specifying column name replacements.
#' @return If single variable: a data frame. If multiple variables: a list of data frames, named by variable.
#' @export
#' @examples
#' # ─── Load SL4 Data ───────────────────────────────────────────────────
#' sl4_data1 <- read_sl4x("path/to/sl4file1.sl4")
#' sl4_data2 <- read_sl4x("path/to/sl4file2.sl4")
#'
#' # ─── Extracting Variables ────────────────────────────────────────────
#' # Extract a single variable from a single SL4 file
#' get_sl4_data("qo", sl4_data1)
#'
#' # Extract multiple variables from a single SL4 file
#' get_sl4_data(c("qo", "qgdp"), sl4_data1)
#'
#' # ─── Handling Subtotals ───────────────────────────────────────────────
#' # Drop subtotals for a single variable
#' get_sl4_data("qo", sl4_data1, drop_subtotals = TRUE)
#'
#' # Drop subtotals for multiple variables
#' get_sl4_data(c("qo", "qgdp"), sl4_data1, drop_subtotals = TRUE)
#'
#' # ─── Renaming Columns ────────────────────────────────────────────────
#' # Rename columns when extracting data
#' get_sl4_data("qo", sl4_data1, rename_cols = c(REG = "Region", COMM = "Commodity"))
#'
#' # ─── Working with Multiple SL4 Files ─────────────────────────────────
#' # Extract one variable from multiple SL4 files
#' get_sl4_data("qo", sl4_data1, sl4_data2, experiment_names = c("baseline", "policy"))
#' 
#' # Extract multiple variables from multiple SL4 files
#' get_sl4_data(c("qo", "qgdp"), sl4_data1, sl4_data2, experiment_names = c("baseline", "policy"))
#'
#' # ─── Extracting All Variables ────────────────────────────────────────
#' # Extract all variables from a single SL4 file
#' get_sl4_data(NULL, sl4_data1)
#' 
#' # Extract all variables from multiple SL4 files
#' get_sl4_data(NULL, sl4_data1, sl4_data2, experiment_names = c("baseline", "policy"))
get_sl4_data <- function(var_names = NULL, ..., experiment_names = NULL, 
                         drop_subtotals = FALSE, rename_cols = NULL) {
  sl4_list <- list(...)
  
  if (length(sl4_list) == 0) {
    stop("At least one SL4 object is required.")
  }
  
  if (is.null(experiment_names)) {
    dots <- match.call(expand.dots = FALSE)$...
    experiment_names <- if (length(dots) == 1) {
      deparse(dots[[1]])
    } else {
      vapply(dots, deparse, character(1))
    }
  }
  if (length(experiment_names) != length(sl4_list)) {
    stop("The number of experiment names must match the number of SL4 objects.")
  }
  
  if (is.null(var_names)) {
    var_names <- unique(unlist(lapply(sl4_list, function(x) names(x$data))))
  }
  
  if (is.character(var_names) && length(var_names) == 1) {
    var_name <- var_names
    
    df_list <- lapply(seq_along(sl4_list), function(i) {
      sl4_obj <- sl4_list[[i]]
      experiment_name <- experiment_names[i]
      
      if (!var_name %in% names(sl4_obj$data)) {
        warning(sprintf("Variable '%s' not found in experiment '%s'", var_name, experiment_name))
        return(NULL)
      }
      
      var_data <- sl4_obj$data[[var_name]]
      dim_info <- sl4_obj$dimension_info[[var_name]]
      
      df <- as.data.frame.table(var_data, stringsAsFactors = FALSE, responseName = "Value")
      setNames(df, c(dim_info$dimension_names, "Type"))
      
      df$Variable <- var_name
      df$Dimension <- dim_info$dimension_string
      df$Experiment <- experiment_name
      
      if (drop_subtotals) {
        df <- df[df$Type != "TOTAL", ]
      }
      
      rename_sl4col(df, rename_cols)
    })
    
    df_list <- Filter(Negate(is.null), df_list)
    names(df_list) <- experiment_names[sapply(df_list, function(x) !is.null(x))]
    
    if (length(df_list) == 0) {
      stop(sprintf("Variable '%s' not found in any experiment", var_name))
    }
    
    result <- do.call(rbind, df_list)
    col_order <- intersect(c("Experiment", "Variable", "Dimension", "Type", names(result), "Value"), names(result))
    
    return(result[, col_order, drop = FALSE])
  }
  
  result_list <- lapply(var_names, function(var_name) {
    tryCatch({
      get_sl4_data(var_name, ..., 
                   experiment_names = experiment_names,
                   drop_subtotals = drop_subtotals,
                   rename_cols = rename_cols)
    }, error = function(e) {
      warning(sprintf("Could not process variable '%s': %s", var_name, e$message))
      NULL
    })
  })
  
  names(result_list) <- var_names
  result_list <- Filter(Negate(is.null), result_list)
  
  if (length(result_list) == 0) {
    stop("No variables could be processed")
  }
  
  return(result_list)
}


#' ─────────────────────────────────────────────────────────────────────────────
#' Group SL4 Data by Dimension Patterns
#' 
#' Groups and processes structured data from one or more SL4 objects based on 
#' specified dimension patterns or all unique patterns. Returns either a single 
#' data frame (for one pattern) or a list of data frames (for multiple patterns).
#' 
#' @param patterns Character vector of dimension patterns to extract. Use NULL to extract all unique patterns.
#' @param ... One or more SL4 objects.
#' @param experiment_names Optional character vector of names for each SL4 object.
#' @param drop_subtotals Logical; whether to remove subtotal rows.
#' @param rename_cols A named vector specifying column name replacements.
#' @return If single pattern: a data frame. If multiple patterns: a list of data frames, named by pattern.
#' @export
#' @examples
#' # ─── Load SL4 Data ───────────────────────────────────────────────────
#' sl4_data1 <- read_sl4x("path/to/sl4file1.sl4")
#' sl4_data2 <- read_sl4x("path/to/sl4file2.sl4")
#'
#' # ─── Extracting Data for One Pattern ─────────────────────────────────
#' # Extract data for one pattern from a single SL4 file (default experiment name)
#' extract_by_dims ("comm*reg", sl4_data1)
#'
#' # Extract data for one pattern with a custom experiment name
#' extract_by_dims ("comm*reg", sl4_data1, experiment_names = "exp1")
#'
#' # Extract data for one pattern with renamed columns
#' extract_by_dims ("comm*reg", sl4_data1, experiment_names = "exp1", 
#'                rename_cols = c(REG = "Region", COMM = "Commodity"))
#'
#' # ─── Extracting Multiple Patterns from a Single Experiment ───────────
#' # Extract multiple patterns from a single SL4 file
#' extract_by_dims (c("comm*reg", "comm*reg*reg", "reg", "COMM"), sl4_data1)
#'
#' # Extract multiple patterns with a custom experiment name
#' extract_by_dims (c("comm*reg", "comm*reg*reg", "reg", "COMM"), 
#'                sl4_data1, experiment_names = "baseline")
#'
#' # Extract multiple patterns with column renaming
#' extract_by_dims (c("comm*reg", "comm*reg*reg", "reg", "COMM"), 
#'                sl4_data1, experiment_names = "baseline",
#'                rename_cols = c(REG = "Region", COMM = "Commodity"))
#'
#' # ─── Extracting Data for One Pattern Across Multiple Experiments ─────
#' # Extract a single pattern from multiple SL4 files (default experiment names)
#' extract_by_dims ("comm*reg", sl4_data1, sl4_data2)
#'
#' # Extract a single pattern with custom experiment names
#' extract_by_dims ("comm*reg", sl4_data1, sl4_data2, 
#'                experiment_names = c("baseline", "policy"))
#'
#' # Extract a single pattern with column renaming
#' extract_by_dims ("comm*reg", sl4_data1, sl4_data2, 
#'                experiment_names = c("baseline", "policy"),
#'                rename_cols = c(REG = "Region"))
#'
#' # ─── Extracting Multiple Patterns Across Multiple Experiments ─────────
#' # Extract multiple patterns from multiple SL4 files
#' extract_by_dims (c("comm*reg", "comm*reg*reg", "reg", "COMM"), 
#'                sl4_data1, sl4_data2)
#'
#' # Extract multiple patterns with custom experiment names
#' extract_by_dims (c("comm*reg", "comm*reg*reg", "reg", "COMM"), 
#'                sl4_data1, sl4_data2,
#'                experiment_names = c("baseline", "policy"))
#'
#' # Extract multiple patterns, rename columns, and drop subtotals
#' extract_by_dims (c("comm*reg", "comm*reg*reg", "reg", "COMM"), 
#'                sl4_data1, sl4_data2,
#'                experiment_names = c("baseline", "policy"),
#'                drop_subtotals = TRUE,
#'                rename_cols = c(REG = "Region", COMM = "Commodity"))
#'
#' # ─── Extracting All Patterns ─────────────────────────────────────────
#' # Extract all patterns from a single SL4 file
#' extract_by_dims (NULL, sl4_data1)
#'
#' # Extract all patterns from multiple SL4 files with custom experiment names
#' extract_by_dims (NULL, sl4_data1, sl4_data2, 
#'                experiment_names = c("baseline", "policy"))
#' Group SL4 Data by Dimension Patterns
#' 
#' @param patterns Character vector of patterns or NULL for all patterns
#' @param ... One or more SL4 objects
#' @param experiment_names Optional names for each SL4 object
#' @param drop_subtotals Logical; whether to remove subtotal rows
#' @param rename_cols Named vector for renaming columns
#' @export
extract_by_dims  <- function(patterns = NULL, ..., experiment_names = NULL, 
                                  drop_subtotals = FALSE, rename_cols = NULL) {
  sl4_list <- list(...)
  
  if (length(sl4_list) == 0) {
    stop("At least one SL4 object is required.")
  }
  
  # Handle experiment names
  if (is.null(experiment_names)) {
    dots <- match.call(expand.dots = FALSE)$...
    experiment_names <- if (length(dots) == 1) {
      deparse(dots[[1]])
    } else {
      vapply(dots, deparse, character(1))
    }
  }
  
  if (length(experiment_names) != length(sl4_list)) {
    stop("The number of experiment names must match the number of SL4 objects.")
  }
  
  # Get all patterns if none specified
  if (is.null(patterns)) {
    all_patterns <- list()
    for (i in seq_along(sl4_list)) {
      patterns_i <- sapply(sl4_list[[i]]$dimension_info, 
                           function(x) x$dimension_string)
      all_patterns[[i]] <- patterns_i
    }
    patterns <- unique(unlist(all_patterns))  # Keep original patterns
  }
  
  # Single pattern optimization
  if (is.character(patterns) && length(patterns) == 1) {
    pattern <- patterns
    df_list <- list()
    
    # Find original pattern name from the first matching pattern
    original_pattern <- NULL
    for (sl4_obj in sl4_list) {
      dim_strings <- sapply(sl4_obj$dimension_info, function(x) x$dimension_string)
      matching_idx <- which(sapply(dim_strings, function(x) 
        are_patterns_equal(x, pattern)))
      if (length(matching_idx) > 0) {
        original_pattern <- dim_strings[matching_idx[1]]
        break
      }
    }
    
    if (is.null(original_pattern)) {
      stop(sprintf("Pattern '%s' not found in any experiment", pattern))
    }
    
    for (i in seq_along(sl4_list)) {
      sl4_obj <- sl4_list[[i]]
      exp_name <- experiment_names[i]
      
      matching_vars <- names(sl4_obj$dimension_info)[
        sapply(sl4_obj$dimension_info, function(x) 
          are_patterns_equal(x$dimension_string, pattern)
        )
      ]
      
      if (length(matching_vars) == 0) {
        warning(sprintf("No variables found with pattern '%s' in experiment '%s'", 
                        pattern, exp_name))
        next
      }
      
      var_data_list <- list()
      for (var_name in matching_vars) {
        var_data <- sl4_obj$data[[var_name]]
        dim_info <- sl4_obj$dimension_info[[var_name]]
        
        df <- as.data.frame.table(var_data, stringsAsFactors = FALSE, responseName = "Value")
        setNames(df, c(dim_info$dimension_names, "Type"))
        
        df$Variable <- var_name
        df$Dimension <- dim_info$dimension_string
        df$Experiment <- exp_name
        
        df <- df[!is.na(df$Value), ]
        
        if (nrow(df) > 0) {
          var_data_list[[var_name]] <- df
        }
      }
      
      if (length(var_data_list) > 0) {
        df_list[[exp_name]] <- do.call(rbind, var_data_list)
      }
    }
    
    if (length(df_list) == 0) {
      stop(sprintf("No non-NA data found for pattern: %s", pattern))
    }
    
    result <- do.call(rbind, df_list)
    rownames(result) <- NULL
    
    # Check if dropping subtotals would create structural issues
    if (drop_subtotals) {
      has_totals <- any(result$Type == "TOTAL")
      if (has_totals && all(result$Type == "TOTAL")) {
        stop("Cannot drop subtotals: all data consists of subtotals. Please set drop_subtotals = FALSE")
      }
      result <- result[result$Type != "TOTAL", ]
    }
    
    if (!is.null(rename_cols)) {
      result <- rename_sl4col(result, rename_cols)
    }
    
    attr(result, "pattern") <- original_pattern  # Use original pattern name
    class(result) <- c("grouped_sl4", class(result))
    
    return(result)
  }
  
  # Multiple patterns case
  result_list <- list()
  pattern_errors <- character()
  
  for (pattern in patterns) {
    tryCatch({
      result <- extract_by_dims (pattern, ...,
                                      experiment_names = experiment_names,
                                      drop_subtotals = drop_subtotals,
                                      rename_cols = rename_cols)
      if (!is.null(result) && nrow(result) > 0) {
        # Use original pattern name as key
        result_list[[attr(result, "pattern")]] <- result
      }
    }, error = function(e) {
      msg <- conditionMessage(e)
      if (grepl("Cannot drop subtotals", msg)) {
        pattern_errors <- c(pattern_errors, 
                            sprintf("Pattern '%s': %s", pattern, msg))
      } else {
        warning(sprintf("Could not process pattern '%s': %s", pattern, msg))
      }
    })
  }
  
  if (length(result_list) == 0) {
    if (length(pattern_errors) > 0) {
      stop("Processing failed due to subtotal constraints:\n", 
           paste(pattern_errors, collapse = "\n"), 
           "\nPlease set drop_subtotals = FALSE to process these patterns.")
    } else {
      stop("No patterns could be processed")
    }
  }
  
  attr(result_list, "n_patterns") <- length(result_list)
  attr(result_list, "patterns") <- names(result_list)
  if (length(sl4_list) > 1) {
    attr(result_list, "experiments") <- experiment_names
  }
  class(result_list) <- c("grouped_multi_sl4", class(result_list))
  
  return(result_list)
}


#' ─────────────────────────────────────────────────────────────────────────────
#' Group and Merge Data by Dimension Priority
#' 
#' This function organizes and merges **grouped datasets** (from `extract_by_dims()`) 
#' based on **predefined dimension priorities**.  
#' 
#' It ensures that datasets with **identical column structures** are merged, while 
#' reporting those that cannot be merged due to **structural differences**.
#' 
#' The function processes datasets across **different dimension levels** (e.g., `1D`, `2D`, `3D`) 
#' while maintaining structured groupings based on **dimension priorities**.
#' 
#' @param grouped_data A named list of data frames grouped by dimension patterns.
#'                     This is typically the output from `extract_by_dims()`.
#' @param dimension_map A data frame defining the priority grouping with three required columns:
#'   - `dimension`: Dimension name (e.g., `"COMM"`, `"ACTS"`, `"REG"`).
#'   - `group`: Group name for aggregation (e.g., `"Sector"`, `"Region"`).
#'   - `priority`: Numeric priority (lower value = higher priority).
#' @return A structured list containing:
#'   - **`merged_data`**: A list of merged datasets based on dimension priorities.
#'   - **`unmergeable`**: Datasets that could not be merged due to different structures.
#'   - **`merge_report`**: A data frame listing datasets that failed to merge.
#' @export
#' @examples
#' # ─── Load Grouped SL4 Data ────────────────────────────────────────────
#' grouped_sl4 <- extract_by_dims(NULL, sl4_data1, sl4_data2, 
#'                                experiment_names = c("baseline", "policy"))
#'
#' # ─── Define Dimension Priority Mapping ───────────────────────────────
#' priority_mapping <- data.frame(
#'   dimension = c("COMM", "ACTS", "REG"),
#'   group = c("Sector", "Sector", "Region"),
#'   priority = c(1, 1, 2),  # "Sector" (COMM, ACTS) > "Region" (REG)
#'   stringsAsFactors = FALSE
#' )
#'
#' # ─── Apply Dimension Priority Grouping ──────────────────────────────
#' regrouped_data <- group_by_dims(grouped_sl4, priority_mapping)
#'
#' # ─── Accessing the Results ──────────────────────────────────────────
#' merged_data <- regrouped_data$merged_data
#' unmergeable_data <- regrouped_data$unmergeable
#' merge_report <- regrouped_data$merge_report
group_by_dims <- function(grouped_data, dimension_map) {
  if (!all(c("dimension", "group", "priority") %in% names(dimension_map))) {
    stop("dimension_map must contain columns: dimension, group, and priority")
  }
  
  priority_groups <- unique(dimension_map$group)
  
  get_dim_count <- function(name) {
    if (grepl("\\*", name)) length(strsplit(name, "\\*")[[1]]) else 1
  }
  
  get_priority_group <- function(name) {
    parts <- strsplit(tolower(name), "\\*")[[1]]
    dims <- tolower(dimension_map$dimension)
    for (group in priority_groups) {
      group_dims <- dims[dimension_map$group == group]
      if (any(vapply(group_dims, function(x) 
        any(grepl(paste0("^", x, "$"), parts, ignore.case = TRUE)), logical(1)))) {
        return(group)
      }
    }
    return("Other")
  }
  
  get_df_signature <- function(df) {
    list(cols = sort(colnames(df)), ncol = ncol(df))
  }
  
  result <- list()
  unmergeable <- list()
  merge_issues <- data.frame(Dataset = character(), Issue = character())
  all_dims <- unique(vapply(names(grouped_data), get_dim_count, numeric(1)))
  
  for (dim_count in sort(all_dims)) {
    dim_name <- paste0(dim_count, "D")
    result[[dim_name]] <- list()
    dim_data <- grouped_data[vapply(names(grouped_data), get_dim_count, numeric(1)) == dim_count]
    
    group_priorities <- tapply(dimension_map$priority, dimension_map$group, min)
    sorted_groups <- c(names(sort(group_priorities)), "Other")
    
    for (priority_group in sorted_groups) {
      group_data <- list()
      for (data_name in names(dim_data)) {
        if (get_priority_group(data_name) == priority_group) {
          group_data[[data_name]] <- dim_data[[data_name]]
        }
      }
      
      if (length(group_data) > 0) {
        signatures <- lapply(group_data, get_df_signature)
        signature_groups <- split(group_data, vapply(signatures, function(x) 
          paste(x$cols, collapse = "|"), character(1)))
        
        if (length(signature_groups) > 1) {
          largest_group <- which.max(lengths(signature_groups))
          merged_data <- do.call(rbind, signature_groups[[largest_group]])
          result[[dim_name]][[priority_group]] <- merged_data
          
          other_sigs <- signature_groups[-largest_group]
          for (sig_group in other_sigs) {
            for (name in names(sig_group)) {
              unmergeable[[name]] <- sig_group[[name]]
              largest_cols <- strsplit(names(signature_groups)[largest_group], "\\|")[[1]]
              current_cols <- signatures[[name]]$cols
              merge_issues <- rbind(merge_issues, data.frame(
                Dataset = name,
                Issue = sprintf("Different column structure in %s. Expected: %s, Got: %s",
                                dim_name, paste(largest_cols, collapse = ", "),
                                paste(current_cols, collapse = ", "))))
            }
          }
        } else {
          result[[dim_name]][[priority_group]] <- do.call(rbind, group_data)
        }
      }
    }
    if (length(result[[dim_name]]) == 0) {
      result[[dim_name]] <- NULL
    }
  }
  
  if (length(unmergeable) > 0) result$unmergeable <- unmergeable
  if (nrow(merge_issues) > 0) result$merge_report <- merge_issues
  result$dimension_map <- dimension_map
  class(result) <- c("regrouped_by_priority", "list")
  return(result)
}


#' ─────────────────────────────────────────────────────────────────────────────
#' Extract and Group SL4 Data by Dimension Priority
#' 
#' This function **directly processes SL4 objects**, extracting and grouping 
#' structured data based on **dimension priorities**.
#' 
#' @param ... One or more SL4 objects (from `read_sl4x()`).
#' @param dimension_map A data frame defining dimension priorities (see `group_by_dims()`).
#' @return A structured list (same output as `group_by_dims()`).
#' @export
#' @examples
#' # ─── Load SL4 Data ───────────────────────────────────────────────────
#' sl4_data1 <- read_sl4x("path/to/sl4file1.sl4")
#' sl4_data2 <- read_sl4x("path/to/sl4file2.sl4")
#'
#' # ─── Define Priority Mapping ─────────────────────────────────────────
#' priority_mapping <- data.frame(
#'   dimension = c("COMM", "ACTS", "REG"),
#'   group = c("Sector", "Sector", "Region"),
#'   priority = c(1, 1, 2)
#' )
#'
#' # ─── Extract and Group Directly from SL4 ─────────────────────────────
#' result <- group_by_dims_from_sl4(sl4_data1, sl4_data2, dimension_map = priority_mapping)
group_by_dims_from_sl4 <- function(..., dimension_map) {
  if (!all(c("dimension", "group", "priority") %in% names(dimension_map))) {
    stop("dimension_map must contain columns: dimension, group, and priority")
  }
  
  sl4_list <- list(...)
  experiment_names <- paste0("data", seq_along(sl4_list))
  patterns <- unique(unlist(lapply(sl4_list, function(x) {
    vapply(x$dimension_info, function(d) d$dimension_string, character(1))
  })))
  
  grouped_data <- extract_by_dims(patterns = patterns, 
                                  ..., 
                                  experiment_names = experiment_names)
  
  return(group_by_dims(grouped_data, dimension_map))
}
