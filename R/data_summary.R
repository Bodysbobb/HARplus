#' Get Variable Summary for a Header
#' 
#' Retrieves detailed information for a specific header within an enhanced HAR object,
#' including both dimension information and data values.
#' 
#' @param header_name Name of the header whose details are to be retrieved.
#' @param har_obj An enhanced HAR object created using `read_harx()` or `read_sl4x()`.
#' @return A list containing:
#'   \itemize{
#'     \item dimension_info: List of dimension names, sizes, and elements
#'     \item data: The actual data values for the header
#'   }
#' @export
#' @examples
#' # ─── Load HAR Data ───────────────────────────────────────────────────
#' har_data <- read_harx("path/to/file.har")
#' sl4_data <- read_sl4x("path/to/file.sl4")
#' 
#' # ─── Get Complete Summary for a Header ──────────────────────────────
#' var_har_sum <- get_var_summary_har("A", har_data)
#' var_sl4_sum <- get_var_summary_har("qo", sl4_data)
get_var_summary <- function(header_name, har_obj) {
  if (!header_name %in% names(har_obj$dimension_info)) {
    stop(sprintf("Header '%s' not found", header_name))
  }
  
  summary <- list(
    dimension_info = har_obj$dimension_info[[header_name]],
    data = har_obj$data[[header_name]]
  )
  
  return(summary)
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


#' Standardize dimension pattern
# Additional Functions (Export) -----------------------------------------------------

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
#' renamed_sl4_data <- rename_dims(sl4_data, mapping_df)
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
#' Rename Data Frame Columns and List Names in SL4 or HAR Object
#' 
#' This function renames the columns of data frames within an SL4 object (or HAR), 
#' based on a mapping. It also allows renaming list element names if requested.
#' 
#' @param sl4_obj A structured SL4 object (represented as a list) containing data frames.
#' @param mapping_df A two-column data frame where the first column (`old`) contains current names, 
#'        and the second column (`new`) contains the new names.
#' @param rename_list_names Logical (default: `FALSE`). If `TRUE`, the function will rename 
#'        the list element names based on the mapping.
#' @return The modified SL4 object with updated column names and, optionally, updated list names.
#' @export
#' @examples
#' # ─── Example 1: Using Direct Data ───────────────────────────────────────────
#' mapping_df <- data.frame(
#'   old = c("REG", "COMM"),
#'   new = c("Region", "Commodity")
#' )
#'
#' # Only rename columns
#' result1 <- rename_dims(all.pat.multi.exp, mapping_df)
#'
#' # Rename both columns and list names
#' result2 <- rename_dims(all.pat.multi.exp, mapping_df, rename_list_names = TRUE)
#'
#' # ─── Example 2: Using SL4 Data ──────────────────────────────────────────────
#' 
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
#'   new = paste0("New_", dim_elements$DimName)  # Example new names
#' )
#'
#' # ─── Rename Columns in SL4 Object ──────────────────────────────────
#' renamed_sl4_obj <- rename_dims(sl4_obj, mapping_df)
#'
#' # ─── Rename Both Columns and List Names ────────────────────────────
#' renamed_sl4_obj_full <- rename_dims(sl4_obj, mapping_df, rename_list_names = TRUE)
rename_dims <- function(sl4_obj, mapping_df, rename_list_names = FALSE) {
  if (!is.list(sl4_obj)) {
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
    old_names <- names(sl4_obj)
    new_names <- sapply(old_names, function(name) {
      rename_parts(name, rename_cols)
    })
    names(sl4_obj) <- new_names
  }
  
  for (var_name in names(sl4_obj)) {
    df <- sl4_obj[[var_name]]
    
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
    
    sl4_obj[[var_name]] <- df 
  }
  
  return(sl4_obj)  
}