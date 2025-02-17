#' Get Data Summary from SL4 and HAR Object
#' 
#' Generates a summary of the variables within an enhanced SL4 or HAR object, 
#' listing their dimension sizes, structures, and optionally includes counts
#' for columns and observations.
#' 
#' @param variables Character vector of variable names to summarize. Use NULL or "ALL"
#'        to summarize all variables (default is NULL).
#' @param data_obj An enhanced SL4 or HAR object created using `read_sl4x()` or `read_harx()`.
#' @param include_col_size Logical. If TRUE, adds columns for total number of 
#'        columns and observations (default is FALSE).
#' @return A data frame containing variable names, dimension counts, dimension strings,
#'         and optionally column and observation counts.
#' @export
#' @examples
#' # ─── Load SL4 Data ───────────────────────────────────────────────────
#' sl4_data <- read_sl4x("path/to/sl4file.sl4")
#' har_data <- read_harx("path/to/sl4file.har")
#'
#' # ─── Get Basic Summary ────────────────────────────────────────────────
#' var_summary <- get_var_structure(data_obj = sl4_data)
#' var_summary_har <- get_var_structure(data_obj = har_data)
#'
#' # ─── Get Summary with Column and Observation Counts ──────────────────
#' var_summary_detailed <- get_var_structure(
#'   variables = c("gdp", "trade"), 
#'   data_obj = sl4_data, 
#'   include_col_size = TRUE
#' )
get_var_structure <- function(variables = NULL, data_obj, include_col_size = FALSE) {
  if (is.null(variables) || identical(variables, "ALL")) {
    var_list <- names(data_obj$dimension_info)
  } else {
    var_list <- variables[variables %in% names(data_obj$dimension_info)]
  }
  
  var_list <- sort(var_list)
  variables <- character(0)
  dimensions <- character(0)
  sizes <- numeric(0)
  data_shapes <- character(0)
  n_cols <- numeric(0)
  n_obs <- numeric(0)
  
  for (var_name in var_list) {
    info <- get_dim_info(data_obj$dimension_info[[var_name]])
    variables <- c(variables, var_name)
    dimensions <- c(dimensions, info$dimension_string)
    sizes <- c(sizes, info$dim_size)
    data_shapes <- c(data_shapes, info$data_shape)
    
    if (include_col_size) {
      n_cols <- c(n_cols, info$col_size)
      n_obs <- c(n_obs, info$n_obs)
    }
  }
  
  summary_df <- data.frame(
    Variable = variables,
    Dimensions = dimensions,
    DimSize = sizes,
    DataShape = data_shapes,
    stringsAsFactors = FALSE,
    row.names = NULL
  )
  
  if (include_col_size) {
    summary_df$No.Col <- n_cols
    summary_df$No.Obs <- n_obs
  }
  
  summary_df
}


#' Extract Unique Dimension Strings from SL4 and HAR Objects
#'
#' Extracts and lists unique dimension strings from one or more SL4 or HAR objects.
#'
#' @param ... One or more structured SL4 or HAR objects containing dimension information.
#' @param keep_unique Logical; if TRUE, returns only unique dimension strings across inputs.
#' @return A data frame containing unique dimension strings.
#' @export
#' @examples
#' # Load SL4 and HAR Data
#' sl4_data1 <- read_sl4x("path/to/file1.sl4")
#' sl4_data2 <- read_sl4x("path/to/file2.sl4")
#'
#' # Extract unique dimension strings from a single dataset
#' get_dims_strings(sl4_data1)
#'
#' # Extract unique dimension strings from multiple datasets
#' get_dims_strings(sl4_data1, sl4_data2)
#'
#' # Extract only unique dimension strings across datasets
#' get_dims_strings(sl4_data1, sl4_data2, keep_unique = TRUE)
#' 
get_dims_strings <- function(..., keep_unique = FALSE) {
  data_objs <- list(...)
  
  if (length(data_objs) == 0) {
    stop("At least one data object is required.")
  }
  
  dim_strings_list <- lapply(data_objs, function(data_obj) {
    sapply(names(data_obj$dimension_info), function(var_name) {
      data_obj$dimension_info[[var_name]]$dimension_string
    })
  })
  
  dim_strings <- unique(unlist(dim_strings_list))
  
  result <- data.frame(DimString = dim_strings, stringsAsFactors = FALSE)
  
  if (keep_unique) {
    result <- unique(result)
  }
  
  return(result)
}



#' Extract Unique Dimension Elements from SL4 and HAR Objects
#'
#' Extracts and lists unique dimension elements from one or more SL4 or HAR objects.
#'
#' @param ... One or more structured SL4 or HAR objects containing dimension information.
#' @param keep_unique Logical; if TRUE, returns only unique dimension elements across inputs.
#' @return A data frame containing unique dimension elements.
#' @export
#' @examples
#' # Load SL4 and HAR Data
#' sl4_data1 <- read_sl4x("path/to/file1.sl4")
#' sl4_data2 <- read_sl4x("path/to/file2.sl4")
#'
#' # Extract unique dimension elements from a single dataset
#' get_dims_elements(sl4_data1)
#'
#' # Extract unique dimension elements from multiple datasets
#' get_dims_elements(sl4_data1, sl4_data2)
#'
#' # Extract only unique dimension elements across datasets
#' get_dims_elements(sl4_data1, sl4_data2, keep_unique = TRUE)
#'
get_dims_elements <- function(..., keep_unique = FALSE) {
  data_objs <- list(...)
  
  if (length(data_objs) == 0) {
    stop("At least one data object is required.")
  }
  
  dim_elements_list <- lapply(data_objs, function(data_obj) {
    dimensions <- character(0)
    for (var_name in names(data_obj$dimension_info)) {
      dim_info <- data_obj$dimension_info[[var_name]]
      dimensions <- c(dimensions, dim_info$dimension_names)
    }
    return(dimensions)
  })
  
  dim_elements <- unique(unlist(dim_elements_list))
  
  result <- data.frame(DimName = dim_elements, stringsAsFactors = FALSE)
  
  if (keep_unique) {
    result <- unique(result)
  }
  
  return(result)
}


#' Rename Data Frame Columns and List Names in SL4 or HAR Object
#' 
#' This function renames the columns of data frames within an SL4 or HAR object, 
#' based on a mapping. It also allows renaming list element names if requested.
#' 
#' @param data_obj A structured SL4 or HAR object (represented as a list) containing data frames.
#' @param mapping_df A two-column data frame where the first column (`old`) contains current names, 
#'        and the second column (`new`) contains the new names.
#' @param rename_list_names Logical (default: `FALSE`). If `TRUE`, the function will rename 
#'        the list element names based on the mapping.
#' @return The modified SL4 or HAR object with updated column names and, optionally, updated list names.
#' @export
#' @examples
#' # ─── Load SL4 and HAR Data ─────────────────────────────────────────────
#' sl4_data <- read_sl4x("path/to/file.sl4")
#' har_data <- read_harx("path/to/file.har")
#'
#' # ─── Retrieve Unique Dimension Elements ──────────────────────────────
#' dim_elements_sl4 <- get_dims_elements(sl4_data)
#' dim_elements_har <- get_dims_elements(har_data)
#'
#' # ─── Create a Mapping Data Frame for Renaming ──────────────────────
#' mapping_df <- data.frame(
#'   old = dim_elements_sl4$DimName,  # Extracted dimension names
#'   new = paste0("New_", dim_elements_sl4$DimName)  # Example new names
#' )
#'
#' # ─── Rename Columns in SL4 and HAR Object ────────────────────────────
#' renamed_sl4_data <- rename_dims(sl4_data, mapping_df)
#' renamed_har_data <- rename_dims(har_data, mapping_df)
rename_dims <- function(data_obj, mapping_df, rename_list_names = FALSE) {
  if (!is.list(data_obj)) {
    stop("Invalid input: Expected a list object.")
  }
  if (!is.data.frame(mapping_df) || ncol(mapping_df) != 2) {
    stop("Invalid mapping_df: Expected a two-column data frame with old and new names.")
  }
  
  old_names <- mapping_df[[1]]
  new_names <- mapping_df[[2]]
  rename_cols <- setNames(new_names, old_names)
  
  rename_parts <- function(str, mapping) {
    parts <- strsplit(str, "\\*")[[1]]
    new_parts <- sapply(parts, function(part) {
      if (part %in% names(mapping)) {
        mapping[[part]]
      } else {
        part
      }
    })
    paste(new_parts, collapse = "*")
  }
  
  if (rename_list_names) {
    old_names <- names(data_obj)
    new_names <- sapply(old_names, function(name) {
      rename_parts(name, rename_cols)
    })
    names(data_obj) <- new_names
  }
  
  for (var_name in names(data_obj)) {
    df <- data_obj[[var_name]]
    
    if (!is.data.frame(df)) next  
    
    for (old_name in names(rename_cols)) {
      matching_cols <- which(names(df) == old_name)
      if (length(matching_cols) > 0) {
        for (i in seq_along(matching_cols)) {
          new_name <- if (i == 1) rename_cols[old_name] else paste0(rename_cols[old_name], i - 1)
          names(df)[matching_cols[i]] <- new_name
        }
      }
    }
    
    data_obj[[var_name]] <- df 
  }
  
  return(data_obj)  
}


#' Compare Variable Structures Across Multiple SL4 and HAR Objects
#'
#' This function compares the variable structures of multiple SL4 and HAR objects 
#' to check compatibility for `rbind` operations. It ensures that variables have 
#' consistent dimension names and order across all inputs.
#'
#' The function provides two modes:
#' - If `keep_unique = FALSE`, it verifies if variables match across inputs for 
#'   compatibility and identifies discrepancies.
#' - If `keep_unique = TRUE`, it extracts and returns unique variable structures 
#'   across all provided objects.
#'
#' @param variables Character vector specifying variable names to compare. Use `NULL` or `"ALL"`
#'        to compare all variables (default is `NULL`).
#' @param ... Named SL4 or HAR objects created using `read_sl4x()` or `read_harx()`.
#'        At least two objects must be provided.
#' @param keep_unique Logical. If `TRUE`, returns unique variable structures across 
#'        all inputs instead of checking compatibility (default is `FALSE`).
#' @return A list containing:
#'   - `match`: A data frame of variables with consistent structures across inputs.
#'   - `diff` (if differences exist): A data frame highlighting variables with mismatched 
#'     structures, showing how they differ across inputs.
#' @export
#'
#' @examples
#' # ─── Load SL4 and HAR Data ─────────────────────────────────────────────
#' sl4_data1 <- read_sl4x("path/to/file1.sl4")
#' sl4_data2 <- read_sl4x("path/to/file2.sl4")
#' har_data  <- read_harx("path/to/file.har")
#'
#' # ─── Compare Structures for Compatibility ──────────────────────────────
#' comparison <- compare_var_structure(
#'   variables = c("qo", "EV"), sl4_data1, sl4_data2
#' )
#'
#' # ─── Extract Unique Variable Structures Across Inputs ──────────────────
#' unique_vars <- compare_var_structure(
#'   variables = c("qo", "EV"), sl4_data1, sl4_data2, har_data,
#'   keep_unique = TRUE
#' )
compare_var_structure <- function(variables = NULL, ..., keep_unique = FALSE) {
  inputs <- list(...)
  input_names <- if(!is.null(names(inputs))) names(inputs) else paste0("input", seq_along(inputs))
  if (length(inputs) < 2) stop("At least two data objects must be provided for comparison")
  
  get_var_info <- function(data_obj, var_name) {
    if (!var_name %in% names(data_obj$dimension_info)) return(NULL)
    get_dim_info(data_obj$dimension_info[[var_name]])
  }
  
  if (is.null(variables) || identical(variables, "ALL")) {
    variables <- unique(unlist(lapply(inputs, function(x) names(x$dimension_info))))
    variables <- sort(variables)
  }
  
  if (keep_unique) {
    match_df <- data.frame(
      Variable = character(),
      Dimensions = character(),
      DimSize = numeric(),
      DataShape = character(),
      stringsAsFactors = FALSE
    )
    
    diff_df <- data.frame(
      Variable = character(),
      stringsAsFactors = FALSE
    )
    for (name in input_names) {
      diff_df[[paste0(name, "_DimensionName")]] <- character()
    }
    
    for (var in variables) {
      var_dims <- list()
      for (i in seq_along(inputs)) {
        info <- get_var_info(inputs[[i]], var)
        if (!is.null(info)) {
          var_dims[[input_names[i]]] <- info
        }
      }
      
      if (length(unique(sapply(var_dims, function(x) 
        paste(x$dimension_string, x$dim_size, x$data_shape)))) == 1) {
        first_info <- var_dims[[1]]
        match_df <- rbind(match_df, data.frame(
          Variable = var,
          Dimensions = first_info$dimension_string,
          DimSize = first_info$dim_size,
          DataShape = first_info$data_shape,
          stringsAsFactors = FALSE
        ))
      } else {
        new_row <- data.frame(Variable = var, stringsAsFactors = FALSE)
        for (name in input_names) {
          new_row[[paste0(name, "_DimensionName")]] <- 
            if (!is.null(var_dims[[name]])) var_dims[[name]]$dimension_string else NA
        }
        diff_df <- rbind(diff_df, new_row)
      }
    }
    
    result <- list(match = match_df)
    if (nrow(diff_df) > 0) result$diff <- diff_df
    
  } else {
    match_df <- data.frame(
      Variable = character(),
      Dimensions = character(),
      DataShape = character(),
      stringsAsFactors = FALSE
    )
    for (name in input_names) {
      match_df[[paste0(name, "_ColSize")]] <- numeric()
    }
    
    diff_df <- data.frame(
      Variable = character(),
      stringsAsFactors = FALSE
    )
    for (name in input_names) {
      diff_df[[paste0(name, "_DimSize")]] <- character()
    }
    
    for (var in variables) {
      var_infos <- list()
      col_sizes <- numeric(length(inputs))
      
      for (i in seq_along(inputs)) {
        info <- get_var_info(inputs[[i]], var)
        if (!is.null(info)) {
          var_infos[[i]] <- info
          col_sizes[i] <- info$col_size
        }
      }
      
      all_match <- length(var_infos) == length(inputs) &&
        all(sapply(var_infos[-1], function(x) 
          identical(x$dimension_string, var_infos[[1]]$dimension_string)))
      
      if (all_match) {
        new_row <- data.frame(
          Variable = var,
          Dimensions = var_infos[[1]]$dimension_string,
          DataShape = var_infos[[1]]$data_shape,
          stringsAsFactors = FALSE
        )
        for (i in seq_along(inputs)) {
          new_row[[paste0(input_names[i], "_ColSize")]] <- col_sizes[i]
        }
        match_df <- rbind(match_df, new_row)
      } else {
        new_row <- data.frame(Variable = var, stringsAsFactors = FALSE)
        for (i in seq_along(inputs)) {
          new_row[[paste0(input_names[i], "_DimSize")]] <- 
            if (!is.null(var_infos[[i]])) var_infos[[i]]$dim_size else NA
        }
        diff_df <- rbind(diff_df, new_row)
      }
    }
    
    result <- list(match = match_df)
    if (nrow(diff_df) > 0) result$diff <- diff_df
  }
  
  return(result)
}