#' Rename Columns (Internal)
#'
#' A helper function that renames columns inside the function.
#'
#' @param df A data frame.
#' @param rename_cols A named vector where names represent old column names and values represent new names.
#' @return A data frame with renamed columns.
#' @import dplyr tidyr
#' @keywords internal
rename_col <- function(df, rename_cols) {
  if (!is.null(rename_cols)) {
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


#' Extract Dimension Information (Internal)
#'
#' This internal helper function extracts and organizes dimension-related information 
#' from a given dimension metadata object.
#'
#' @param dim_info A list containing dimension metadata, including `dimension_string`, 
#' `dimension_names`, and `dimension_sizes`.
#' @return A list with extracted dimension details: 
#'   \item{dimension_string}{The original dimension string.}
#'   \item{dim_size}{The number of dimensions.}
#'   \item{data_shape}{A string representing the shape of the data (e.g., "10x20x30").}
#'   \item{col_size}{The product of all dimension sizes except the first, representing 
#'   the column size.}
#'   \item{n_obs}{The first dimension size, typically representing the number of observations.}
#' @keywords internal for data_summary code
get_dim_info <- function(dim_info) {
  list(
    dimension_string = dim_info$dimension_string,
    dim_size = length(dim_info$dimension_names),
    data_shape = paste(dim_info$dimension_sizes, collapse = "x"),
    col_size = prod(dim_info$dimension_sizes[-1], 1),
    n_obs = dim_info$dimension_sizes[1]
  )
}


#' Match patterns with optional mixing
#'
#' Performs case-insensitive comparison of patterns, with an option to allow mixed pattern matching.
#'
#' @param pattern1 A character string representing the first pattern.
#' @param pattern2 A character string representing the second pattern.
#' @param mix_patterns Logical; if TRUE, allows matching of mixed patterns.
#' @return Logical; TRUE if the patterns match, FALSE otherwise.
#' @keywords internal
pattern_match <- function(pattern1, pattern2, mix_patterns = FALSE) {
  if (mix_patterns) {
    split1 <- unlist(strsplit(tolower(pattern1), "\\*"))
    split2 <- unlist(strsplit(tolower(pattern2), "\\*"))
    return(length(split1) == length(split2) && 
             all(sort(split1) == sort(split2)))
  } else {
    return(tolower(pattern1) == tolower(pattern2))
  }
}

#' Retrieve the original pattern name
#'
#' Finds the original dimension pattern name in the data object that matches a given pattern.
#'
#' @param pattern A character string representing the pattern to search for.
#' @param data_obj A data object containing dimension information.
#' @param mix_patterns Logical; if TRUE, allows matching of mixed patterns.
#' @return The original pattern name as a character string, or NULL if no match is found.
#' @keywords internal
get_original_pattern <- function(pattern, data_obj, mix_patterns = FALSE) {
  all_vars <- names(data_obj$dimension_info)
  
  matching_vars <- character(0)
  for (var_name in all_vars) {
    dim_info <- data_obj$dimension_info[[var_name]]
    if (!is.null(dim_info$dimension_string) && 
        pattern_match(dim_info$dimension_string, pattern, mix_patterns)) {
      matching_vars <- c(matching_vars, var_name)
      break  
    }
  }
  
  if (length(matching_vars) > 0) {
    dim_info <- data_obj$dimension_info[[matching_vars[1]]]
    return(paste(dim_info$dimension_names, collapse="*"))
  }
  
  return(NULL)
}

#' Process a pattern within a data object
#'
#' Extracts and processes matching variables from a data object based on a given pattern.
#'
#' @param pattern A character string representing the pattern to match.
#' @param data_obj A data object containing dimension information and data.
#' @param exp_name A character string representing the experiment name.
#' @param pattern_mix Logical; if TRUE, allows matching of mixed patterns.
#' @return A data frame containing processed data for the matching pattern, or NULL if no matches are found.
#' @keywords internal
process_pattern <- function(pattern, data_obj, exp_name, pattern_mix = FALSE) {
  matching_vars <- names(data_obj$dimension_info)[
    sapply(data_obj$dimension_info, function(x) 
      pattern_match(x$dimension_string, pattern, pattern_mix)
    )
  ]
  
  if (length(matching_vars) == 0) {
    warning(sprintf("No variables found with pattern '%s' in experiment '%s'. Please check the dimension name or try pattern_mix = TRUE", 
                    pattern, exp_name))
    return(NULL)
  }
  
  var_data_list <- list()
  for (var_name in matching_vars) {
    var_data <- data_obj$data[[var_name]]
    dim_info <- data_obj$dimension_info[[var_name]]
    
    if (length(dim(var_data)) == 0) {
      next
    }
    
    df <- as.data.frame.table(var_data, stringsAsFactors = FALSE, responseName = "Value")
    
    setNames(df, c(dim_info$dimension_names))
    
    if ("type" %in% tolower(names(df))) {
      names(df)[tolower(names(df)) == "type"] <- "Subtotal"
    } else if ("subtotal" %in% tolower(names(df))) {
      names(df)[tolower(names(df)) == "subtotal"] <- "Subtotal"
    }
    
    df$Variable <- var_name
    df$Dimension <- dim_info$dimension_string
    df$Experiment <- exp_name
    
    df <- df[!is.na(df$Value), ]
    
    if (nrow(df) > 0) {
      var_data_list[[var_name]] <- df
    }
  }
  
  if (length(var_data_list) > 0) {
    result <- do.call(rbind, var_data_list)
    rownames(result) <- NULL 
    return(result)
  }
  return(NULL)
}
