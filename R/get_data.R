#' Extract and Structure Data from SL4 or HAR Objects
#'
#' Extracts structured data for one or more variables from one or more 
#' SL4 or HAR data objects, transforming array-like data into a tidy format.
#' Supports merging datasets, renaming columns, and filtering subtotal rows.
#'
#' @param var_names A character vector of variable names to extract. Use `"ALL"` or `NULL` 
#'   to extract all available variables.
#' @param ... One or more SL4 or HAR data objects.
#' @param experiment_names Optional character vector providing names for each input data object.
#'   If `NULL`, names are inferred from the function call.
#' @param drop_subtotals Logical; if `TRUE`, subtotal rows (with `Subtotal == "TOTAL"`) are removed.
#' @param rename_cols A named vector specifying column name replacements (e.g., `c("old" = "new")`).
#'   Applied using `rename_col()`.
#' @param merge_data Logical; if `TRUE`, attempts to merge data from multiple input objects 
#'   for each variable. If structures differ, unmerged datasets are returned separately.
#'
#' @return 
#' - If `merge_data = TRUE`: A list with two components:
#'   - `$merged`: A list of merged data frames, one per variable.
#'   - `$unmerged`: A nested list of unmerged data frames, grouped by experiment name.
#' - If `merge_data = FALSE`: A named list of variable-specific lists, where each experiment 
#'   has its extracted data.
#'
#' @details
#' The function processes variables by checking their structure, ensuring they are 
#' array-like, and reshaping them into a tidy data format with explicit dimension names. 
#' - If `merge_data = TRUE`, variables are checked for consistency across datasets before merging.
#' - If `drop_subtotals = TRUE`, rows with `Subtotal == "TOTAL"` are excluded.
#' - If `rename_cols` is provided, column names are modified accordingly.
#' 
#' The function is designed to work seamlessly with both **SL4** and **HAR** datasets, 
#' ensuring compatibility across different data formats used in economic modeling.
#'
#' @export
#'
#' @examples
#' # Load SL4 or HAR Data
#' sl4_data1 <- read_sl4x("path/to/sl4file1.sl4")
#' sl4_data2 <- read_sl4x("path/to/sl4file2.sl4")
#'
#' # Extract a single variable from a single dataset
#' get_data("qo", sl4_data1)
#'
#' # Extract multiple variables from a single dataset
#' get_data(c("qo", "qgdp"), sl4_data1)
#'
#' # Drop subtotals
#' get_data("qo", sl4_data1, drop_subtotals = TRUE)
#'
#' # Rename columns
#' get_data("qo", sl4_data1, rename_cols = c(REG = "Region", COMM = "Commodity"))
#'
#' # Extract one variable from multiple datasets
#' get_data("qo", sl4_data1, sl4_data2, experiment_names = c("baseline", "policy"))
#'
#' # Extract multiple variables from multiple datasets
#' get_data(c("qo", "qgdp"), sl4_data1, sl4_data2, experiment_names = c("baseline", "policy"))
#'
#' # Extract all variables
#' get_data("ALL", sl4_data1)
#' get_data(NULL, sl4_data1) # Equivalent to "ALL"
#'
#' # Extract all variables from multiple datasets
#' get_data(NULL, sl4_data1, sl4_data2, experiment_names = c("baseline", "policy"))
#'
#' # Merge data from multiple datasets
#' get_data(c("qo", "qgdp"), sl4_data1, sl4_data2, merge_data = TRUE)
#'
get_data <- function(var_names = "ALL", ..., experiment_names = NULL,
                     drop_subtotals = FALSE, rename_cols = NULL, merge_data = FALSE) {
  data_list <- list(...)
  
  if (length(data_list) == 0) {
    stop("At least one data object is required.")
  }
  
  if (is.null(experiment_names)) {
    dots <- match.call(expand.dots = FALSE)$...
    experiment_names <- if (length(dots) == 1) {
      deparse(dots[[1]])
    } else {
      vapply(dots, deparse, character(1))
    }
  }
  
  if (length(experiment_names) != length(data_list)) {
    stop("The number of experiment names must match the number of data objects.")
  }
  
  if (length(data_list) == 1) {
    merge_data <- FALSE
  }
  
  if (is.null(var_names) || (length(var_names) == 1 && var_names == "ALL")) {
    var_names <- unique(unlist(lapply(data_list, function(x) names(x$data))))
  }
  
  process_variable <- function(var_name, data_obj, experiment_name) {
    if (!var_name %in% names(data_obj$data)) return(NULL)
    
    var_data <- data_obj$data[[var_name]]
    # Skip scalar dimensions
    if (length(dim(var_data)) == 0) {
      return(NULL)
    }
    
    dim_info <- data_obj$dimension_info[[var_name]]
    
    df <- as.data.frame.table(var_data, stringsAsFactors = FALSE, responseName = "Value")
    
    # Handle dimension names properly
    setNames(df, c(dim_info$dimension_names))
    
    # Add Subtotal column if needed
    if ("type" %in% tolower(names(df))) {
      names(df)[tolower(names(df)) == "type"] <- "Subtotal"
    } else if ("subtotal" %in% tolower(names(df))) {
      names(df)[tolower(names(df)) == "subtotal"] <- "Subtotal"
    }
    
    df$Variable <- var_name
    df$Dimension <- dim_info$dimension_string
    df$Experiment <- experiment_name
    
    if (drop_subtotals) {
      df <- df[df$Subtotal != "TOTAL", ]
    }
    
    df <- rename_col(df, rename_cols)
    return(df)
  }
  
  if (merge_data) {
    merged_results <- list()
    unmerged_results <- list()
    
    for (var_name in var_names) {
      df_list <- lapply(seq_along(data_list), function(i) {
        process_variable(var_name, data_list[[i]], experiment_names[i])
      })
      
      df_list <- Filter(Negate(is.null), df_list)
      
      if (length(df_list) > 0) {
        col_names <- lapply(df_list, names)
        if (length(unique(lapply(col_names, paste, collapse = "|"))) == 1) {
          result <- do.call(rbind, df_list)
          rownames(result) <- NULL
          merged_results[[var_name]] <- result
        } else {
          for (i in seq_along(df_list)) {
            exp_name <- names(df_list)[i]
            if (is.null(unmerged_results[[exp_name]])) {
              unmerged_results[[exp_name]] <- list()
            }
            unmerged_results[[exp_name]][[var_name]] <- df_list[[i]]
          }
        }
      }
    }
    
    result <- list()
    if (length(merged_results) > 0) {
      result$merged <- merged_results
    }
    if (length(unmerged_results) > 0) {
      result$unmerged <- unmerged_results
    }
    
    return(result)
    
  } else {
    result_by_experiment <- lapply(seq_along(data_list), function(i) {
      obj <- data_list[[i]]
      experiment_name <- experiment_names[i]
      
      var_results <- lapply(var_names, function(var_name) {
        process_variable(var_name, obj, experiment_name)
      })
      
      names(var_results) <- var_names
      var_results <- Filter(Negate(is.null), var_results)
      
      if (length(var_results) == 0) return(NULL)
      return(var_results)
    })
    
    names(result_by_experiment) <- experiment_names
    return(Filter(Negate(is.null), result_by_experiment))
  }
}



#' Extract Data by Dimension Patterns from SL4 or HAR Objects
#'
#' This function extracts and structures data from one or more SL4 or HAR objects based on specified 
#' dimension patterns. It can process multiple patterns, merge data across experiments, rename columns, 
#' and optionally drop subtotal rows. The function works seamlessly for both SL4 and HAR datasets.
#'
#' @param patterns A character vector of dimension patterns to extract. Use `"ALL"` or `NULL` 
#'   to extract all unique patterns across the datasets.
#' @param ... One or more SL4 or HAR objects to process.
#' @param experiment_names An optional character vector specifying names for each data object. 
#'   If `NULL`, names are inferred from the function call.
#' @param drop_subtotals Logical; if `TRUE`, subtotal rows (with `Subtotal == "TOTAL"`) are removed 
#'   from the extracted data.
#' @param rename_cols A named vector specifying column name replacements (e.g., `c("old" = "new")`). 
#'   Column renaming is applied using `rename_col()`.
#' @param merge_data Logical; if `TRUE`, data is merged across multiple experiments for each pattern. 
#'   If structures differ, unmerged data is returned separately.
#' @param pattern_mix Logical; if `TRUE`, allows for pattern matching by dimension names in any order.
#'   Useful when patterns have different orders but identical dimensions.
#'
#' @return 
#' - If `merge_data = TRUE`: A list of merged data frames, one per pattern.
#' - If `merge_data = FALSE`: A list of data frames grouped by experiment and pattern.
#'
#' @details 
#' The function supports flexible extraction of data based on dimension patterns. Patterns are matched 
#' case-insensitively, and data is reshaped into a tidy format with explicit dimension names. The 
#' function handles scenarios where multiple patterns are specified and where data from different 
#' experiments need to be merged.
#'
#' Key features:
#' - Extract data for one or multiple dimension patterns.
#' - Optionally drop subtotal rows using `drop_subtotals`.
#' - Rename columns using `rename_cols`.
#' - Merge data across multiple experiments with `merge_data`.
#' - Support pattern matching with different orders using `pattern_mix`.
#'
#' @export
#'
#' @examples
#' # Load SL4 or HAR Data
#' sl4_data1 <- read_sl4x("path/to/sl4file1.sl4")
#' sl4_data2 <- read_sl4x("path/to/sl4file2.sl4")
#'
#' # Extract data for a single pattern from one dataset
#' extract_by_dims("comm*reg", sl4_data1)
#'
#' # Extract multiple patterns from a single dataset
#' extract_by_dims(c("comm*reg", "reg*year"), sl4_data1)
#'
#' # Extract data for a pattern across multiple datasets
#' extract_by_dims("comm*reg", sl4_data1, sl4_data2, experiment_names = c("baseline", "policy"))
#'
#' # Drop subtotal rows and rename columns
#' extract_by_dims("comm*reg", sl4_data1, drop_subtotals = TRUE, 
#'                 rename_cols = c(REG = "Region", COMM = "Commodity"))
#'
#' # Merge data for multiple patterns across datasets
#' extract_by_dims(c("comm*reg", "reg*year"), sl4_data1, sl4_data2, 
#'                 experiment_names = c("baseline", "policy"), merge_data = TRUE)
#'
#' # Use pattern_mix for flexible pattern matching
#' extract_by_dims("reg*comm", sl4_data1, pattern_mix = TRUE)
#'
#' # Extract all patterns from multiple datasets
#' extract_by_dims(NULL, sl4_data1, sl4_data2, experiment_names = c("baseline", "policy"))
#'
extract_by_dims <- function(patterns = "ALL", ..., experiment_names = NULL,
                            drop_subtotals = FALSE, rename_cols = NULL,
                            merge_data = FALSE, pattern_mix = FALSE) {
  data_list <- list(...)
  
  if (length(data_list) == 0) {
    stop("At least one data object is required.")
  }
  
  if (is.null(experiment_names)) {
    dots <- match.call(expand.dots = FALSE)$...
    experiment_names <- if (length(dots) == 1) {
      deparse(dots[[1]])
    } else {
      vapply(dots, deparse, character(1))
    }
  }
  
  if (length(experiment_names) != length(data_list)) {
    stop("The number of experiment names must match the number of data objects.")
  }
  
  if (length(data_list) == 1) {
    merge_data <- FALSE
  }
  
  if (is.null(patterns) || (length(patterns) == 1 && patterns == "ALL")) {
    all_patterns <- list()
    for (i in seq_along(data_list)) {
      patterns_i <- sapply(data_list[[i]]$dimension_info, 
                           function(x) x$dimension_string)
      all_patterns[[i]] <- patterns_i
    }
    patterns <- unique(unlist(all_patterns))
  }
  
  if (merge_data) {
    result_list <- list()
    pattern_errors <- character()
    
    for (pattern in patterns) {
      pattern_results <- list()
      
      for (i in seq_along(data_list)) {
        tryCatch({
          df <- process_pattern(pattern, data_list[[i]], experiment_names[i], pattern_mix)
          if (!is.null(df)) {
            pattern_results[[experiment_names[i]]] <- df
          }
        }, error = function(e) {
          warning(sprintf("Error processing pattern '%s' for experiment '%s': %s",
                          pattern, experiment_names[i], conditionMessage(e)))
        })
      }
      
      if (length(pattern_results) > 0) {
        merged_df <- do.call(rbind, pattern_results)
        rownames(merged_df) <- NULL  
        
        if (drop_subtotals) {
          if (all(merged_df$Subtotal == "TOTAL")) {
            pattern_errors <- c(pattern_errors,
                                sprintf("Pattern '%s': Cannot drop subtotals: all data consists of subtotals", pattern))
            next
          }
          merged_df <- merged_df[merged_df$Subtotal != "TOTAL", ]
        }
        
        if (!is.null(rename_cols)) {
          merged_df <- rename_col(merged_df, rename_cols)
        }
        
        original_pattern <- NULL
        for (data_obj in data_list) {
          potential_pattern <- get_original_pattern(pattern, data_obj, pattern_mix)
          if (!is.null(potential_pattern)) {
            original_pattern <- potential_pattern
            break
          }
        }
        
        if (!is.null(original_pattern)) {
          result_list[[original_pattern]] <- merged_df
        }
      }
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
    if (length(data_list) > 1) {
      attr(result_list, "experiments") <- experiment_names
    }
    class(result_list) <- c("grouped_multi_data", class(result_list))
    
    return(result_list)
    
  } else {
    result_by_experiment <- list()
    
    for (i in seq_along(data_list)) {
      pattern_results <- list()
      
      for (pattern in patterns) {
        tryCatch({
          df <- process_pattern(pattern, data_list[[i]], experiment_names[i], pattern_mix)
          if (!is.null(df)) {
            if (drop_subtotals) {
              if (all(df$Subtotal == "TOTAL")) {
                warning(sprintf("Pattern '%s' in experiment '%s': Skipping - all data consists of subtotals",
                                pattern, experiment_names[i]))
                next
              }
              df <- df[df$Subtotal != "TOTAL", ]
            }
            
            if (!is.null(rename_cols)) {
              df <- rename_col(df, rename_cols)
            }
            
            original_pattern <- get_original_pattern(pattern, data_list[[i]], pattern_mix)
            if (!is.null(original_pattern)) {
              pattern_results[[original_pattern]] <- df
            }
          }
        }, error = function(e) {
          warning(sprintf("Error processing pattern '%s' for experiment '%s': %s",
                          pattern, experiment_names[i], conditionMessage(e)))
        })
      }
      
      if (length(pattern_results) > 0) {
        result_by_experiment[[experiment_names[i]]] <- pattern_results
      }
    }
    
    if (length(data_list) == 1) {
      return(result_by_experiment[[experiment_names[1]]])
    }
    
    if (length(result_by_experiment) == 0) {
      stop("No patterns could be processed for any experiment")
    }
    
    return(result_by_experiment)
  }
}


#' Group Data by Dimension Elements
#'
#' Groups data extracted from SL4 or HAR objects based on dimension patterns, 
#' applying priority rules and renaming columns as needed. Supports automatic merging 
#' of datasets when applicable.
#'
#' @param patterns A character vector of dimension patterns to extract. Use `"ALL"` to extract all available patterns.
#' @param ... One or more SL4 or HAR objects to process.
#' @param priority A named list specifying priority dimension elements. Keys represent group names, and values are dimension elements.
#' @param rename_cols A named vector specifying column name replacements (e.g., `c("old" = "new")`).
#' @param experiment_names An optional character vector providing names for each input dataset.
#' @param drop_subtotals Logical; if `TRUE`, subtotal rows (with `Subtotal == "TOTAL"`) are removed.
#' @param auto_rename Logical; if `TRUE`, automatically renames dimension columns when merging datasets to ensure consistency.
#'
#' @return A structured list of grouped data frames based on dimension patterns. 
#' If merging is unsuccessful, a report detailing merge issues is included as an attribute.
#'
#' @details
#' The function extracts data based on dimension patterns, groups datasets by dimension elements, 
#' and attempts to merge them where applicable. It follows priority rules to group dimensions logically 
#' and applies renaming where necessary.
#'
#' - If `auto_rename = TRUE`, column names are adjusted to facilitate merging.
#' - If `drop_subtotals = TRUE`, subtotal rows labeled `"TOTAL"` are excluded.
#' - The function checks for duplicate elements in dimension patterns and ensures compatibility before merging.
#'
#' If merging is not possible due to structural differences, the function returns a report explaining 
#' why certain patterns could not be merged.
#'
#' @export
#'
#' @examples
#' # Load SL4 and HAR Data
#' sl4_data1 <- read_sl4x("path/to/file1.sl4")
#' sl4_data2 <- read_sl4x("path/to/file2.sl4")
#' 
#' # Define priority dimension elements
#' priority_list <- list(
#'   "Region" = c("REG"),
#'   "Commodity" = c("COMM"),
#'   "Factor" = c("ENDW")
#' )
#'
#' # Group data by dimension elements with default settings
#' grouped_data <- group_by_dims_element(
#'   patterns = "ALL",
#'   sl4_data1, sl4_data2,
#'   priority = priority_list
#' )
#'
#' # Group data and auto-rename columns for merging
#' grouped_data_auto <- group_by_dims_element(
#'   patterns = "ALL",
#'   sl4_data1, sl4_data2,
#'   priority = priority_list,
#'   auto_rename = TRUE
#' )
#'
#' # Drop subtotal rows and rename columns during grouping
#' grouped_data_clean <- group_by_dims_element(
#'   patterns = "ALL",
#'   sl4_data1, sl4_data2,
#'   priority = priority_list,
#'   drop_subtotals = TRUE,
#'   rename_cols = c("REG" = "Region", "COMM" = "Commodity")
#' )
#'
group_by_dims_element <- function(patterns = "ALL", ..., priority,
                                  rename_cols = NULL, experiment_names = NULL,
                                  drop_subtotals = FALSE, auto_rename = FALSE) {
  if (is.null(experiment_names)) {
    dots <- match.call(expand.dots = FALSE)$...
    experiment_names <- if (length(dots) == 1) {
      deparse(dots[[1]])
    } else {
      vapply(dots, deparse, character(1))
    }
  }
  
  data_list <- list(...)
  
  all_dims <- get_dims_elements(...)$DimName
  priority_elements <- unlist(priority)
  
  auto_rename_elements <- setdiff(all_dims, priority_elements)
  
  extracted_data <- extract_by_dims(
    patterns = patterns, ..., 
    pattern_mix = TRUE,
    merge_data = TRUE,
    drop_subtotals = drop_subtotals,
    experiment_names = experiment_names
  )
  
  dim_sizes <- vapply(names(extracted_data), function(x) {
    length(strsplit(x, "\\*")[[1]])
  }, numeric(1))
  
  result <- setNames(
    vector("list", length(unique(dim_sizes))), 
    paste0(sort(unique(dim_sizes)), "D")
  )
  
  report <- data.frame(
    Pattern = character(),
    DimSize = character(),
    Reason = character(),
    stringsAsFactors = FALSE
  )
  
  get_highest_priority <- function(pattern, priority_list) {
    pattern_elements <- strsplit(pattern, "\\*")[[1]]
    for (prio_name in names(priority_list)) {
      if (any(tolower(priority_list[[prio_name]]) %in% tolower(pattern_elements))) {
        return(prio_name)
      }
    }
    return(NULL)
  }
  
  check_mergeable <- function(pattern) {
    elements <- strsplit(pattern, "\\*")[[1]]
    
    if (length(elements) != length(unique(elements))) {
      return(list(mergeable = FALSE, reason = "Duplicate elements detected"))
    }
    
    if (!any(tolower(elements) %in% tolower(priority_elements))) {
      return(list(mergeable = FALSE, reason = "No priority elements present"))
    }
    
    return(list(mergeable = TRUE, reason = NULL))
  }
  
  process_dimension <- function(dim_size, patterns, priority_list) {
    current_patterns <- patterns[dim_sizes == dim_size]
    if (length(current_patterns) == 0) return(list())
    
    if (auto_rename) {
      dimension_groups <- list()
      processed_patterns <- character(0)
      
      if (dim_size <= 2) {
        pattern_groups <- list()
        other_patterns <- character(0)
        
        for (pattern in current_patterns) {
          highest_prio <- get_highest_priority(pattern, priority_list)
          if (!is.null(highest_prio)) {
            pattern_groups[[highest_prio]] <- c(pattern_groups[[highest_prio]], pattern)
          } else {
            other_patterns <- c(other_patterns, pattern)
          }
        }
        
        for (prio_name in names(priority_list)) {
          matching_patterns <- pattern_groups[[prio_name]]
          
          if (length(matching_patterns) > 0) {
            df_list <- lapply(matching_patterns, function(pattern) {
              df <- extracted_data[[pattern]]
              
              col_mapping <- list()
              for (col in colnames(df)) {
                for (p_name in names(priority_list)) {
                  prio_elements <- priority_list[[p_name]]
                  if (any(tolower(col) == tolower(prio_elements))) {
                    col_mapping[[col]] <- p_name
                  }
                }
              }
              
              for (old_col in names(col_mapping)) {
                colnames(df)[colnames(df) == old_col] <- col_mapping[[old_col]]
              }
              
              if (auto_rename) {
                for (col in colnames(df)) {
                  if (col %in% auto_rename_elements) {
                    colnames(df)[colnames(df) == col] <- "Dim1"
                  }
                }
              }
              df
            })
            
            common_cols <- Reduce(intersect, lapply(df_list, colnames))
            merged_df <- do.call(rbind, lapply(df_list, function(x) {
              x[, common_cols, drop = FALSE]
            }))
            
            dimension_groups[[prio_name]] <- merged_df
            processed_patterns <- c(processed_patterns, matching_patterns)
          }
        }
        
        if (length(other_patterns) > 0) {
          col_counts <- lapply(other_patterns, function(pattern) {
            ncol(extracted_data[[pattern]])
          })
          unique_counts <- unique(unlist(col_counts))
          
          for (count in unique_counts) {
            matching_others <- other_patterns[vapply(other_patterns, 
                                                     function(p) ncol(extracted_data[[p]]) == count, 
                                                     logical(1))]
            
            if (length(matching_others) > 0) {
              df_list <- lapply(matching_others, function(pattern) {
                df <- extracted_data[[pattern]]
                if (auto_rename) {
                  for (col in colnames(df)) {
                    if (col %in% auto_rename_elements) {
                      colnames(df)[colnames(df) == col] <- "Dim1"
                    }
                  }
                }
                df
              })
              
              common_cols <- Reduce(intersect, lapply(df_list, colnames))
              merged_df <- do.call(rbind, lapply(df_list, function(x) {
                x[, common_cols, drop = FALSE]
              }))
              
              group_name <- paste0("Other", if(length(unique_counts) > 1) count else "")
              dimension_groups[[group_name]] <- merged_df
            }
          }
        }
      } else {
        for (pattern in current_patterns) {
          dimension_groups[[pattern]] <- extracted_data[[pattern]]
        }
      }
      return(dimension_groups)
      
    } else {
      dimension_groups <- list()
      
      if (dim_size <= 2) {
        pattern_groups <- list()
        unmerged_patterns <- character(0)
        
        for (pattern in current_patterns) {
          merge_check <- check_mergeable(pattern)
          if (!merge_check$mergeable) {
            report <<- rbind(report, data.frame(
              Pattern = pattern,
              DimSize = paste0(dim_size, "D"),
              Reason = merge_check$reason,
              stringsAsFactors = FALSE
            ))
            unmerged_patterns <- c(unmerged_patterns, pattern)
            next
          }
          
          highest_prio <- get_highest_priority(pattern, priority_list)
          if (!is.null(highest_prio)) {
            pattern_groups[[highest_prio]] <- c(pattern_groups[[highest_prio]], pattern)
          } else {
            unmerged_patterns <- c(unmerged_patterns, pattern)
          }
        }
        
        for (prio_name in names(priority_list)) {
          matching_patterns <- pattern_groups[[prio_name]]
          if (length(matching_patterns) > 0) {
            df_list <- lapply(matching_patterns, function(pattern) {
              df <- extracted_data[[pattern]]
              for (p_name in names(priority_list)) {
                prio_elements <- priority_list[[p_name]]
                for (elem in prio_elements) {
                  if (tolower(elem) %in% tolower(colnames(df))) {
                    colnames(df)[tolower(colnames(df)) == tolower(elem)] <- p_name
                  }
                }
              }
              df
            })
            
            col_names_list <- lapply(df_list, colnames)
            if (length(unique(lapply(col_names_list, paste, collapse = "|"))) == 1) {
              merged_df <- do.call(rbind, df_list)
              dimension_groups[[prio_name]] <- merged_df
            } else {
              report <<- rbind(report, data.frame(
                Pattern = matching_patterns,
                DimSize = paste0(dim_size, "D"),
                Reason = "Different column names (fixed by auto_rename = TRUE)",
                stringsAsFactors = FALSE
              ))
              unmerged_patterns <- c(unmerged_patterns, matching_patterns)
            }
          }
        }
        
        if (length(unmerged_patterns) > 0) {
          dimension_groups$unmerged <- extracted_data[unmerged_patterns]
        }
        
      } else {
        for (pattern in current_patterns) {
          dimension_groups[[pattern]] <- extracted_data[[pattern]]
        }
      }
      
      return(dimension_groups)
    }
  }
  
  for (dim_key in names(result)) {
    dim_size <- as.numeric(sub("D$", "", dim_key))
    result[[dim_key]] <- process_dimension(dim_size, names(extracted_data), priority)
  }
  
  if (!is.null(rename_cols)) {
    result <- lapply(result, function(dim_group) {
      lapply(dim_group, function(df) {
        for (old_name in names(rename_cols)) {
          if (old_name %in% colnames(df)) {
            colnames(df)[colnames(df) == old_name] <- rename_cols[[old_name]]
          }
        }
        df
      })
    })
  }
  
  # Add report if any dimension has an unmerged sublist
  has_unmerged <- any(sapply(result, function(dim_group) {
    "unmerged" %in% names(dim_group)
  }))
  
  if (has_unmerged) {
    sorted_report <- report[order(report$DimSize), ]
    attr(result, "report") <- sorted_report
    result$report <- sorted_report  
  }
  
  class(result) <- c("grouped_dims_element", class(result))
  return(result)
}