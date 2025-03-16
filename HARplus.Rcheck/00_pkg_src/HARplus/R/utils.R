#' @title Rename Columns in a Data Frame (Internal)
#'
#' @description A helper function that renames columns in a data frame based on a specified mapping.
#' Used internally in `get_var_structure()`, `get_data_by_dims()`, and `rename_dims()`.
#'
#' @details
#' - Replaces column names according to the provided `rename_cols` mapping.
#' - Ensures no duplicate column names by appending numerical suffixes when necessary.
#' - Helps standardize column names across SL4 and HAR datasets.
#'
#' @param df A data frame containing columns to be renamed.
#' @param rename_cols A named vector where names are existing column names, 
#' and values are the corresponding new names.
#'
#' @return A modified data frame with renamed columns.
#'
#' @author Pattawee Puangchit
#' 
#' @seealso \code{\link{get_var_structure}}, \code{\link{get_data_by_dims}}, \code{\link{rename_dims}}
#'
#' @keywords internal
#' 
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


#' @title Extract and Organize Dimension Metadata (Internal)
#'
#' @description A helper function that extracts and structures dimension-related metadata 
#' from a given dimension structure. Used internally in `get_var_structure()` 
#' and `compare_var_structure()`.
#'
#' @details
#' - Retrieves structured metadata for variables in SL4 and HAR datasets.
#' - Computes data shape and ensures consistency in dimension structures.
#' - Helps determine observation counts and column sizes for variable summaries.
#'
#' @param dim_info A list containing dimension metadata, including:
#'   - `dimension_string`: A textual representation of dimensions (e.g., `"REG*COMM*YEAR"`).
#'   - `dimension_names`: A character vector of dimension names.
#'   - `dimension_sizes`: A numeric vector indicating the size of each dimension.
#'
#' @return A structured list containing:
#' - `dimension_string`: The original dimension string.
#' - `dim_size`: The number of dimensions.
#' - `data_shape`: A formatted string representing the data shape (e.g., `"10x20x30"`).
#' - `col_size`: The product of all dimension sizes except the first, representing column count.
#' - `n_obs`: The first dimension size, typically representing the number of observations.
#'
#' @author Pattawee Puangchit
#' 
#' @seealso \code{\link{get_var_structure}}, \code{\link{compare_var_structure}}
#'
#' @keywords internal
#' 
get_dim_info <- function(dim_info) {
  list(
    dimension_string = dim_info$dimension_string,
    dim_size = length(dim_info$dimension_names),
    data_shape = paste(dim_info$dimension_sizes, collapse = "x"),
    col_size = prod(dim_info$dimension_sizes[-1], 1),
    n_obs = dim_info$dimension_sizes[1]
  )
}


#' @title Match Patterns with Optional Mixing (Internal)
#'
#' @description Compares two patterns to determine if they match, with an option to allow flexible dimension order.
#'
#' @details
#' - Performs case-insensitive pattern matching.
#' - If `mix_patterns = TRUE`, allows patterns to match even if dimensions are in different order.
#'
#' @param pattern1 A character string representing the first pattern.
#' @param pattern2 A character string representing the second pattern.
#' @param mix_patterns Logical; if `TRUE`, allows dimension order to be ignored during comparison.
#'
#' @return Logical; `TRUE` if the patterns match, `FALSE` otherwise.
#'
#' @author Pattawee Puangchit
#' 
#' @seealso \code{\link{get_original_pattern}}, \code{\link{process_pattern}}, \code{\link{get_data_by_dims}}
#'
#' @keywords internal
#' 
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


#' @title Retrieve the Original Dimension Pattern (Internal)
#'
#' @description A helper function that finds the original dimension pattern name in an SL4 or HAR dataset 
#' that matches a given pattern. Used internally in `get_data_by_dims()`.
#'
#' @details
#' - Performs a case-insensitive comparison to identify matching dimension patterns.
#' - Supports flexible pattern matching when `mix_patterns = TRUE`, allowing dimension order to vary.
#' - Returns the standardized dimension pattern name as stored in the dataset.
#'
#' @param pattern Character. The pattern to search for in dimension structures.
#' @param data_obj An SL4 or HAR object containing dimension information.
#' @param mix_patterns Logical. If `TRUE`, allows dimension order to vary when matching patterns.
#'
#' @return The original dimension pattern name as a character string, or `NULL` if no match is found.
#'
#' @author Pattawee Puangchit
#' 
#' @seealso \code{\link{pattern_match}}, \code{\link{process_pattern}}, \code{\link{get_data_by_dims}}
#'
#' @keywords internal
#' 
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


#' @title Extract and Process Pattern-Matched Variables (Internal)
#'
#' @description A helper function that extracts and processes variables matching a specified pattern 
#' within an SL4 or HAR data object. Used internally in `get_data_by_dims()`.
#'
#' @details
#' - Searches for variables whose dimension structures match the given pattern.
#' - Supports flexible pattern matching when `pattern_mix = TRUE`, allowing dimension order to vary.
#' - Extracted data is converted into a tidy format, preserving dimension structures.
#' - Standardizes `"Subtotal"` column naming for consistency across datasets.
#' - Ensures only non-empty extracted variables are retained.
#'
#' @param pattern Character. The pattern to match against dimension structures.
#' @param data_obj An SL4 or HAR object containing dimension information and data.
#' @param exp_name Character. The experiment name assigned to the extracted data.
#' @param pattern_mix Logical. If `TRUE`, allows pattern matching to ignore dimension order.
#'
#' @return A data frame containing processed data for the matching pattern, or `NULL` if no matches are found.
#'
#' @author Pattawee Puangchit
#' 
#' @seealso \code{\link{get_original_pattern}}, \code{\link{pattern_match}}, \code{\link{get_data_by_dims}}
#'
#' @keywords internal
#' 
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


#' @title Process Decomposition Levels in Data Frames (Internal)
#' 
#' @description A helper function that filters data based on decomposition levels in the `"Subtotal"` column.
#' Used internally in `get_data_by_var()`, `get_data_by_dims()`, and `group_data_by_dims()`.
#'
#' @details
#' - If `subtotal_level = "total"`, **keeps only `"TOTAL"` values**, removing all decomposed components.
#' - If `subtotal_level = "decomposed"`, **keeps only decomposed components**, removing `"TOTAL"`.
#' - If `subtotal_level = "all"`, **keeps both `"TOTAL"` and decomposed values** (no filtering).
#' - If `subtotal_level = TRUE` (logical), it is **equivalent to `"all"`**, meaning all values are kept.
#' - If `subtotal_level = FALSE` (logical), it is **equivalent to `"total"`**, meaning only `"TOTAL"` values are kept, and decomposed components are removed.
#' - Filtering is applied **only when both `"TOTAL"` and decomposed values exist**.
#'
#' @param df A data frame containing a `"Subtotal"` column.
#' @param subtotal_level  Character or logical. Determines which values to retain:
#'   - `"total"`: Keeps only `"TOTAL"` values.
#'   - `"decomposed"`: Keeps only decomposed values (excludes `"TOTAL"`).
#'   - `"all"`: Keeps all rows.
#'   - `TRUE`: Equivalent to `"all"` (keeps both `"TOTAL"` and decomposed values).
#'   - `FALSE`: Equivalent to `"total"` (keeps only `"TOTAL"` values, removing decomposed components).
#'
#' @return A filtered data frame based on the specified decomposition level.
#'
#' @author Pattawee Puangchit
#' 
#' @seealso \code{\link{get_data_by_var}}, \code{\link{get_data_by_dims}}, \code{\link{group_data_by_dims}}
#'
#' @keywords internal
#' 
process_decomp_level <- function(df, subtotal_level) {
  if (!("Subtotal" %in% names(df))) return(df)
  
  level <- if (is.logical(subtotal_level)) {
    if (subtotal_level) "all" else "total"
  } else {
    subtotal_level
  }
  
  if ("Subtotal" %in% names(df)) {
    df <- switch(level,
                 "all" = df,
                 "total" = df[df$Subtotal == "TOTAL", ],
                 "decomposed" = df[df$Subtotal != "TOTAL", ],
                 df)
  }
  return(df)
}



#' @title Process and Export Report (Internal)
#'
#' @description Generates a summary report of variables and their corresponding output files
#' from structured SL4 or HAR data. The report is saved as an Excel file.
#'
#' @details
#' - Extracts unique variable names and their associated export filenames.
#' - Handles nested list structures by recursively traversing them.
#' - The output report is saved as an Excel file named `"Report_<data_name>.xlsx"`.
#'
#' @param data A structured SL4 or HAR object or a nested list of data frames.
#' @param output_path A character string specifying the output directory or file path.
#' @param prefix A character string to prepend to the exported filenames.
#'
#' @importFrom openxlsx createWorkbook addWorksheet writeData createStyle addStyle saveWorkbook
#' @importFrom tools file_ext file_path_sans_ext
#'
#' @author Pattawee Puangchit
#' 
#' @seealso \code{\link{export_data}}
#' 
#' @keywords internal
#'
process_export_report <- function(data, output_path, prefix = "", data_name = "data") {
  report_data <- data.frame(
    Variable = character(),
    OutputFile = character(),
    stringsAsFactors = FALSE
  )
  
  stack <- list(list(data = data, name = ""))
  
  while (length(stack) > 0) {
    current <- stack[[length(stack)]]
    stack <- stack[-length(stack)]
    
    if (is.data.frame(current$data) && "Variable" %in% names(current$data)) {
      output_file <- if (current$name == "") "data" else gsub("\\*", "_", current$name)
      output_file <- paste0(prefix, output_file)
      
      new_rows <- data.frame(
        Variable = unique(current$data$Variable),
        OutputFile = output_file,
        stringsAsFactors = FALSE
      )
      report_data <- rbind(report_data, new_rows)
      
    } else if (is.list(current$data)) {
      for (name in rev(names(current$data))) {
        if (name != "report") {
          new_name <- if (current$name == "") name else paste(current$name, name, sep = "_")
          stack[[length(stack) + 1]] <- list(data = current$data[[name]], name = new_name)
        }
      }
    }
  }
  
  if (nrow(report_data) > 0) {
    report_data <- unique(report_data[order(report_data$Variable), ])
    
    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "Variables")
    openxlsx::writeData(wb, "Variables", report_data, startRow = 1, colNames = TRUE)
    
    header_style <- openxlsx::createStyle(
      textDecoration = "bold",
      border = "Bottom",
      borderStyle = "medium"
    )
    openxlsx::addStyle(wb, "Variables", style = header_style, 
                       rows = 1, cols = 1:ncol(report_data))
    
    output_dir <- if (tools::file_ext(output_path) == "") output_path else dirname(output_path)
    report_filename <- paste0("Report_", data_name, ".xlsx")
    report_path <- file.path(output_dir, report_filename)
    openxlsx::saveWorkbook(wb, report_path, overwrite = TRUE)
    
    message(sprintf("Created %s in %s", report_filename, normalizePath(output_dir)))
  }
}