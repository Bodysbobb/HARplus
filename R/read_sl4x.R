#' Read and Process SL4 Data
#' 
#' Reads an SL4 file and processes its structured data into an enhanced SL4 object. 
#' Extracts metadata, dimensions, and variables while handling subtotal information.
#' 
#' @param filename The path to the SL4 file.
#' @param toLowerCase Logical; whether to convert variable names to lowercase.
#' @return An enhanced SL4 object containing structured data, metadata, and dimension details.
#' @export
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


#' Rename Data Frame Columns Based on Mapping
#' 
#' Renames the columns of a data frame according to a specified mapping, 
#' ensuring that duplicate names are avoided by appending numeric suffixes if necessary.
#' 
#' @param df A data frame whose columns need to be renamed.
#' @param mapping_df A two-column data frame where the first column contains old names, 
#'        and the second column contains new names.
#' @return A data frame with renamed columns.
#' @export
rename_sl4col_all <- function(df, mapping_df) {
  if (!is.null(mapping_df) && ncol(mapping_df) == 2) {
    old_names <- mapping_df[[1]]
    new_names <- mapping_df[[2]]
    rename_cols <- setNames(new_names, old_names)
    
    for (old_name in names(rename_cols)) {
      matching_cols <- which(names(df) == old_name)
      if (length(matching_cols) > 0) {
        for (i in seq_along(matching_cols)) {
          new_name <- if (i == 1) rename_cols[old_name] else paste0(rename_cols[old_name], i - 1)
          names(df)[matching_cols[i]] <- new_name
        }
      }
    }
  }
  return(df)
}


# Data Structure Summary --------------------------------------------------

#' Get Variable Summary from SL4 Object
#' 
#' Generates a summary of the variables within an enhanced SL4 object, 
#' listing their dimension sizes and structures.
#' 
#' @param sl4_obj An enhanced SL4 object created using `read_sl4x()`.
#' @return A data frame containing variable names, dimension counts, and dimension strings.
#' @export
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


#' Get Dimension Information for a Variable
#' 
#' Retrieves detailed dimension information for a specific variable 
#' in an enhanced SL4 object, including its names and sizes.
#' 
#' @param sl4_obj An enhanced SL4 object created using `read_sl4x()`.
#' @param var_name The name of the variable to retrieve dimension details for.
#' @return A list containing dimension names, sizes, and structures for the specified variable.
#' @export
get_var_summary <- function(sl4_obj, var_name) {
  if(!var_name %in% names(sl4_obj$dimension_info)) {
    stop(sprintf("Variable '%s' not found", var_name))
  }
  return(sl4_obj$dimension_info[[var_name]])
}


#' Extract Unique Dimension Elements
#' 
#' Extracts and lists unique dimension elements present in an SL4 object.
#' 
#' @param sl4_obj A structured SL4 object containing dimension information.
#' @return A data frame containing unique dimension elements.
#' @export
get_dims_elements <- function(sl4_obj) {
  dimensions <- character(0)
  
  for (var_name in names(sl4_obj$dimension_info)) {
    dim_info <- sl4_obj$dimension_info[[var_name]]
    dimensions <- c(dimensions, dim_info$dimension_string)
  }
  
  unique_dim_names <- unique(unlist(strsplit(dimensions, "\\*")))
  
  return(data.frame(DimName = unique_dim_names, stringsAsFactors = FALSE))
}


#' Extract Unique Dimension Strings
#' 
#' Extracts and lists unique dimension strings present in an SL4 object.
#' 
#' @param sl4_obj A structured SL4 object containing dimension information.
#' @return A data frame containing unique dimension strings.
#' @export
get_dims_strings <- function(sl4_obj) {
  dimensions <- character(0)
  
  for (var_name in names(sl4_obj$dimension_info)) {
    dim_info <- sl4_obj$dimension_info[[var_name]]
    dimensions <- c(dimensions, dim_info$dimension_string)
  }
  
  unique_dim_names <- unique(dimensions)
  
  return(data.frame(DimName = unique_dim_names, stringsAsFactors = FALSE))
}


# Single SL4 File ------------------------------------------------------------

#' Extract Data for a Specific Variable from SL4 Object
#' 
#' Extracts structured data from a specific variable in an SL4 object, 
#' with options for renaming columns, removing subtotal rows, and adding experiment identifiers.
#' 
#' @param sl4_obj An SL4 object containing structured data.
#' @param var_name The name of the variable to extract.
#' @param add_experiment Logical; whether to add an experiment column.
#' @param experiment_name Name of the experiment (default is derived from `sl4_obj`).
#' @param drop_subtotals Logical; whether to remove subtotal rows.
#' @param rename_cols A named vector specifying column name replacements.
#' @return A structured data frame containing the extracted data.
#' @export
get_var_data <- function(sl4_obj, var_name, add_experiment = FALSE, 
                         experiment_name = NULL, drop_subtotals = FALSE, rename_cols = NULL) {
  if (!var_name %in% names(sl4_obj$data)) {
    stop(sprintf("Variable '%s' not found", var_name))
  }
  
  if (is.null(experiment_name)) {
    experiment_name <- deparse(substitute(sl4_obj))
  }
  
  var_data <- sl4_obj$data[[var_name]]
  dim_info <- sl4_obj$dimension_info[[var_name]]
  
  df <- as.data.frame.table(var_data, stringsAsFactors = FALSE, responseName = "Value")
  rownames(df) <- NULL
  
  orig_dim_names <- dim_info$dimension_names
  colnames(df)[1:length(orig_dim_names)] <- orig_dim_names
  colnames(df)[length(orig_dim_names) + 1] <- "Type"
  
  df$Variable <- var_name
  df$Dimension <- dim_info$dimension_string
  
  if (add_experiment && !"Experiment" %in% names(df)) {
    df$Experiment <- experiment_name
  }
  
  if (drop_subtotals) {
    df <- df[df$Type != "TOTAL", ]
  }
  
  df <- rename_sl4col(df, rename_cols)  
  
  col_order <- c(intersect(c("Experiment", "Variable", "Dimension", "Type", names(df), "Value"), names(df)))
  
  return(df[, col_order, drop = FALSE])  
}


#' Group Data by Dimension Pattern
#' 
#' Groups and processes structured data from an SL4 object based on a specified 
#' dimension pattern, ensuring consistency across grouped variables.
#' 
#' @param sl4_obj An enhanced SL4 object containing structured data.
#' @param pattern A string pattern defining dimension grouping.
#' @param drop_subtotals Logical; whether to remove subtotal rows.
#' @param add_experiment Logical; whether to add an experiment column.
#' @param experiment_name Name of the experiment (default is derived from `sl4_obj`).
#' @param rename_cols A named vector specifying column name replacements.
#' @return A structured data frame containing grouped data.
#' @export
group_by_dims <- function(sl4_obj, pattern, drop_subtotals = FALSE,
                          add_experiment = FALSE, experiment_name = NULL, rename_cols = NULL) {
  if (!inherits(sl4_obj, "enhanced_sl4")) {
    stop("Input must be an enhanced_sl4 object")
  }
  
  if (is.null(experiment_name)) {
    experiment_name <- deparse(substitute(sl4_obj))
  }
  
  matching_vars <- names(sl4_obj$dimension_info)[
    sapply(sl4_obj$dimension_info, function(x) 
      are_patterns_equal(x$dimension_string, pattern)
    )
  ]
  
  if (length(matching_vars) == 0) {
    stop(sprintf("No variables found with pattern: %s", pattern))
  }
  
  df_list <- list()
  valid_vars <- character(0)
  skipped_vars <- character(0)
  
  for (var_name in matching_vars) {
    df <- get_var_data(sl4_obj, var_name, add_experiment, experiment_name, rename_cols = rename_cols)
    df <- df[!is.na(df$Value), ]
    
    if (nrow(df) == 0) {
      skipped_vars <- c(skipped_vars, var_name)
      next
    }
    
    valid_vars <- c(valid_vars, var_name)
    df_list[[var_name]] <- df
  }
  
  if (length(df_list) == 0) {
    stop(sprintf("No non-NA data found for pattern: %s", pattern))
  }
  
  result <- do.call(rbind, df_list)
  rownames(result) <- NULL
  
  if (drop_subtotals) {
    result <- result[result$Type == "TOTAL", ]
  }
  
  result <- rename_sl4col(result, rename_cols)
  
  attr(result, "pattern") <- pattern
  attr(result, "variables") <- valid_vars
  attr(result, "skipped") <- skipped_vars
  
  class(result) <- c("grouped_sl4", class(result))
  
  if (length(skipped_vars) > 0) {
    message(sprintf("Skipped %d all-NA variables: %s", 
                    length(skipped_vars), paste(skipped_vars, collapse = ", ")))
  }
  
  return(result)
}


#' Group All Data by Unique Dimension Patterns
#' 
#' Groups and processes all structured data within an SL4 object based on 
#' unique dimension patterns, ensuring consistency across variables.
#' 
#' @param sl4_obj An enhanced SL4 object containing structured data.
#' @param drop_subtotals Logical; whether to remove subtotal rows.
#' @param add_experiment Logical; whether to add an experiment column.
#' @param experiment_name Name of the experiment (default is derived from `sl4_obj`).
#' @param rename_cols A named vector specifying column name replacements.
#' @return A named list of grouped data frames by dimension patterns.
#' @export
group_all <- function(sl4_obj, drop_subtotals = FALSE,
                      add_experiment = FALSE, experiment_name = NULL, rename_cols = NULL) {
  if (is.null(experiment_name)) {
    experiment_name <- deparse(substitute(sl4_obj))
  }
  
  patterns <- unique(sapply(sl4_obj$dimension_info, function(x) x$dimension_string))
  std_patterns <- unique(sapply(patterns, standardize_pattern))
  
  unique_patterns <- sapply(std_patterns, function(std_pat) {
    patterns[which(sapply(patterns, function(x) 
      standardize_pattern(x) == std_pat))[1]]
  })
  
  grouped_list <- list()
  
  for (pattern in unique_patterns) {
    tryCatch({
      result <- group_by_dims(sl4_obj, pattern, drop_subtotals, add_experiment, experiment_name, rename_cols)
      if (!is.null(result) && nrow(result) > 0) {
        grouped_list[[standardize_pattern(pattern)]] <- result
      }
    }, error = function(e) {
      if (!grepl("No non-NA data found", e$message)) {
        warning(sprintf("Error processing pattern '%s': %s", pattern, e$message))
      }
    })
  }
  
  if (length(grouped_list) == 0) {
    stop("No valid data found after removing NA values")
  }
  
  attr(grouped_list, "n_patterns") <- length(grouped_list)
  attr(grouped_list, "patterns") <- names(grouped_list)
  
  class(grouped_list) <- c("grouped_all_sl4", class(grouped_list))
  return(grouped_list)
}


# Multiple SL4 Files ------------------------------------------------------

#' Extract Data for a Variable Across Multiple SL4 Objects
#' 
#' Extracts and combines data for a specific variable across multiple SL4 objects, 
#' ensuring consistency in structure and naming.
#' 
#' @param var_name The variable name to extract.
#' @param ... One or more SL4 objects.
#' @param experiment_names A character vector specifying names for each SL4 object.
#' @param drop_subtotals Logical; whether to remove subtotal rows.
#' @param rename_cols A named vector specifying column name replacements.
#' @return A structured data frame containing combined data from multiple SL4 objects.
#' @export
get_var_data_multi <- function(var_name, ..., experiment_names = NULL, drop_subtotals = FALSE, rename_cols = NULL) {
  sl4_list <- list(...)
  
  if (is.null(experiment_names)) {
    experiment_names <- as.character(match.call(expand.dots = FALSE)$...[-1])
  }
  
  if (length(sl4_list) == 0) {
    stop("At least one SL4 object is required.")
  }
  if (length(experiment_names) != length(sl4_list)) {
    stop("The number of experiment names must match the number of SL4 objects.")
  }
  
  df_list <- list()
  
  for (i in seq_along(sl4_list)) {
    if (!var_name %in% names(sl4_list[[i]]$data)) {
      warning(sprintf("Variable '%s' not found in experiment '%s'", var_name, experiment_names[i]))
      next
    }
    
    df <- get_var_data(sl4_list[[i]], var_name, add_experiment = TRUE, 
                       experiment_name = experiment_names[i], drop_subtotals = drop_subtotals, rename_cols = rename_cols)
    
    rownames(df) <- NULL
    df_list[[experiment_names[i]]] <- df
  }
  
  if (length(df_list) == 0) {
    stop(sprintf("Variable '%s' not found in any experiment", var_name))
  }
  
  result <- do.call(rbind, df_list)
  rownames(result) <- NULL  
  
  return(result)
}


#' Group Data by Dimension Pattern Across Multiple SL4 Objects
#' 
#' Groups and processes structured data from multiple SL4 objects based on a 
#' specified dimension pattern, ensuring consistency across grouped variables.
#' 
#' @param pattern A string pattern defining dimension grouping.
#' @param ... One or more SL4 objects.
#' @param experiment_names A character vector specifying names for each SL4 object.
#' @param drop_subtotals Logical; whether to remove subtotal rows.
#' @param rename_cols A named vector specifying column name replacements.
#' @return A structured data frame containing grouped data across multiple SL4 objects.
#' @export
group_by_dims_multi <- function(pattern, ..., experiment_names = NULL, drop_subtotals = FALSE, rename_cols = NULL) {
  sl4_list <- list(...)
  
  if (is.null(experiment_names)) {
    experiment_names <- as.character(match.call(expand.dots = FALSE)$...[-1])
  }
  
  if (length(sl4_list) == 0) {
    stop("At least one SL4 object is required.")
  }
  if (length(experiment_names) != length(sl4_list)) {
    stop("The number of experiment names must match the number of SL4 objects.")
  }
  
  df_list <- list()
  
  for (i in seq_along(sl4_list)) {
    sl4_obj <- sl4_list[[i]]
    exp_name <- experiment_names[i]
    
    tryCatch({
      df <- group_by_dims(sl4_obj, pattern, drop_subtotals, add_experiment = TRUE, experiment_name = exp_name, rename_cols = rename_cols)
      rownames(df) <- NULL
      df_list[[exp_name]] <- df
    }, error = function(e) {
      warning(sprintf("Skipping experiment '%s': %s", exp_name, e$message))
    })
  }
  
  if (length(df_list) == 0) {
    stop("No non-NA data found in any experiment.")
  }
  
  result <- do.call(rbind, df_list)
  rownames(result) <- NULL  
  
  return(result)
}


#' Group All Data Across Multiple SL4 Objects by Dimension Patterns
#' 
#' Groups and processes structured data from multiple SL4 objects based on unique 
#' dimension patterns, ensuring consistency across variables.
#' 
#' @param ... One or more SL4 objects.
#' @param experiment_names A character vector specifying names for each SL4 object.
#' @param drop_subtotals Logical; whether to remove subtotal rows.
#' @param rename_cols A named vector specifying column name replacements.
#' @return A named list of grouped data frames by dimension patterns across multiple SL4 objects.
#' @export
group_all_multi <- function(..., experiment_names = NULL, drop_subtotals = FALSE, rename_cols = NULL) {
  sl4_list <- list(...)
  
  if (is.null(experiment_names)) {
    experiment_names <- as.character(match.call(expand.dots = FALSE)$...)
  }
  
  if (length(sl4_list) == 0) stop("At least one SL4 object required")
  if (length(experiment_names) != length(sl4_list)) {
    stop("Number of experiment names must match number of SL4 objects")
  }
  
  all_patterns <- list()
  for (i in seq_along(sl4_list)) {
    patterns <- sapply(sl4_list[[i]]$dimension_info, function(x) x$dimension_string)
    all_patterns[[experiment_names[i]]] <- patterns
  }
  
  std_patterns <- unique(unlist(lapply(all_patterns, function(patterns) {
    sapply(patterns, standardize_pattern)
  })))
  
  pattern_results <- list()
  
  for (std_pat in std_patterns) {
    tryCatch({
      combined_data <- group_by_dims_multi(std_pat, ..., 
                                           experiment_names = experiment_names,
                                           drop_subtotals = drop_subtotals, rename_cols = rename_cols)
      if (nrow(combined_data) > 0) {
        pattern_results[[std_pat]] <- combined_data
      }
    }, error = function(e) {
      warning(sprintf("Skipping pattern '%s': %s", std_pat, e$message))
    })
  }
  
  if (length(pattern_results) == 0) {
    stop("No non-NA data found in any experiment for any dimension pattern")
  }
  
  attr(pattern_results, "n_patterns") <- length(pattern_results)
  attr(pattern_results, "patterns") <- names(pattern_results)
  attr(pattern_results, "experiments") <- experiment_names
  
  class(pattern_results) <- c("grouped_multi_sl4", class(pattern_results))
  return(pattern_results)
}


# Aggregating Data --------------------------------------------------------

#' Combine Data by Dimension Groups
#' 
#' Merges and structures grouped datasets based on a mapping of dimensions, 
#' ensuring consistency across merged datasets while preserving structure.
#' 
#' @param grouped_data A named list of data frames grouped by dimensions.
#' @param dimension_map A data frame containing `dimension`, `group`, and `priority` columns.
#' @return A structured list containing merged datasets, unmergeable datasets, and a merge report.
#' @export
combine_by_dims <- function(grouped_data, dimension_map) {
  if (!all(c("dimension", "group", "priority") %in% names(dimension_map))) {
    stop("dimension_map must contain columns: dimension, group, and priority")
  }
  
  priority_groups <- unique(dimension_map$group)
  
  get_dim_count <- function(name) {
    if (grepl("\\*", name)) length(strsplit(name, "\\*")[[1]]) else 1
  }
  
  get_priority_group <- function(name) {
    parts <- strsplit(name, "\\*")[[1]]
    for (group in priority_groups) {
      group_dims <- dimension_map$dimension[dimension_map$group == group]
      if (any(sapply(group_dims, function(x) any(grepl(paste0("^", x, "$"), parts))))) {
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
  merge_issues <- data.frame(Dataset = character(), Issue = character(), stringsAsFactors = FALSE)
  
  all_dims <- unique(sapply(names(grouped_data), get_dim_count))
  
  for (dim_count in sort(all_dims)) {
    dim_name <- paste0(dim_count, "D")
    result[[dim_name]] <- list()
    dim_data <- grouped_data[sapply(names(grouped_data), get_dim_count) == dim_count]
    
    sorted_groups <- c(priority_groups[order(tapply(dimension_map$priority, dimension_map$group, min))], "Other")
    
    for (priority_group in sorted_groups) {
      group_data <- list()
      for (data_name in names(dim_data)) {
        if (get_priority_group(data_name) == priority_group) {
          group_data[[data_name]] <- dim_data[[data_name]]
        }
      }
      
      if (length(group_data) > 0) {
        signatures <- lapply(group_data, get_df_signature)
        signature_groups <- list()
        
        for (name in names(group_data)) {
          sig <- paste(signatures[[name]]$cols, collapse = "|")
          if (is.null(signature_groups[[sig]])) {
            signature_groups[[sig]] <- list()
          }
          signature_groups[[sig]][[name]] <- group_data[[name]]
        }
        
        group_sizes <- sapply(signature_groups, length)
        if (length(group_sizes) > 1) {
          largest_sig <- names(which.max(group_sizes))
          merged_data <- do.call(rbind, signature_groups[[largest_sig]])
          result[[dim_name]][[priority_group]] <- merged_data
          
          for (sig in names(signature_groups)) {
            if (sig != largest_sig) {
              for (name in names(signature_groups[[sig]])) {
                unmergeable[[name]] <- signature_groups[[sig]][[name]]
                issue_desc <- sprintf("Different column structure in %s. Expected: %s, Got: %s", 
                                      dim_name, paste(strsplit(largest_sig, "\\|")[[1]], collapse = ", "),
                                      paste(signatures[[name]]$cols, collapse = ", "))
                merge_issues <- rbind(merge_issues, data.frame(Dataset = name, Issue = issue_desc, stringsAsFactors = FALSE))
              }
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
  
  if (length(unmergeable) > 0) {
    result$unmergeable <- unmergeable
  }
  if (nrow(merge_issues) > 0) {
    result$merge_report <- merge_issues
  }
  
  result$dimension_map <- dimension_map
  class(result) <- c("regrouped_by_priority", "list")
  return(result)
}
