#' Read and Process GEMPACK HAR Files
#' 
#' Reads a GEMPACK HAR file and enhances the extracted data by incorporating 
#' detailed dimension information for each header. This function allows for customization, 
#' such as using coefficient names instead of headers and specifying which headers to read.
#' 
#' @param con File path or connection to the HAR file.
#' @param useCoefficientsAsNames Logical; whether to use coefficient names instead of headers.
#' @param headersToRead Specific headers to read; if NULL, all headers are read.
#' @return An enhanced HAR object containing extracted data along with dimension details.
#' @export
#' @examples
#' # ─── Load HAR Data ───────────────────────────────────────────────────
#' har_path <- "path/to/file.har"
#' har_data <- read_harx(har_path)
#' 
#' # ─── Load HAR Data with coefficient names instead of headers ─────────
#' har_data <- read_harx(har_path, useCoefficientsAsNames = TRUE)
#'
#' # ─── Load HAR Data but only read selected headers ────────────────────
#' har_data <- read_harx(har_path, headersToRead = c("A", "B1", "GDP"))
read_harx <- function(con, 
                      useCoefficientsAsNames = FALSE,
                      headersToRead = NULL) {
  
  har_data <- rhar_enhanced(con, 
                             useCoefficientsAsNames = useCoefficientsAsNames,
                             toLowerCase = FALSE,  
                             headersToRead = headersToRead)
  
  dimension_info <- list()
  
  for (header_name in names(har_data)) {
    header_obj <- har_data[[header_name]]
    
    if (!is.null(dim(header_obj))) {
      dims <- dim(header_obj)
      dim_names <- dimnames(header_obj)
      
      if (!is.null(dim_names)) {
        dim_string <- paste(names(dim_names), collapse="*")
        dimension_info[[header_name]] <- list(
          dimension_string = dim_string,
          dimension_names = names(dim_names),
          dimension_sizes = dims,
          dimension_elements = dim_names
        )
      } else {
        generic_names <- paste0("DIM", seq_along(dims))
        dim_string <- paste(generic_names, collapse="*")
        dimension_info[[header_name]] <- list(
          dimension_string = dim_string,
          dimension_names = generic_names,
          dimension_sizes = dims,
          dimension_elements = lapply(dims, function(x) paste0("Element", 1:x))
        )
      }
    } else {
      dimension_info[[header_name]] <- list(
        dimension_string = "scalar",
        dimension_names = character(0),
        dimension_sizes = integer(0),
        dimension_elements = list()
      )
    }
  }
  
  enhanced_results <- list(
    data = har_data,
    dimension_info = dimension_info,
    metadata = list(
      filename = if(is.character(con)) con else summary(con)$description,
      creation_date = as.character(Sys.time()),
      headers = names(har_data)
    )
  )
  
  class(enhanced_results) <- "enhanced_har"
  
  return(enhanced_results)
}


#' ─────────────────────────────────────────────────────────────────────────────
#' Get Dimension Information for a Header
#' 
#' Retrieves detailed dimension information for a specific header within an 
#' enhanced HAR object. It returns the structure of dimensions, including 
#' their names, sizes, and element labels.
#' 
#' @param header_name Name of the header whose dimension details are to be retrieved.
#' @param har_obj An enhanced HAR object created using `read_harx()`.
#' @return A list containing the dimension names, sizes, and elements for the specified header.
#' @export
#' @examples
#' # ─── Load HAR Data ───────────────────────────────────────────────────
#' har_data <- read_harx("path/to/file.har")
#'
#' # ─── Get Dimension Details for a Header ──────────────────────────────
#' dim_info <- get_var_summary_har("GDP", har_data)
get_var_summary_har <- function(header_name, har_obj) {
  if (!header_name %in% names(har_obj$dimension_info)) {
    stop(sprintf("Header '%s' not found", header_name))
  }
  return(har_obj$dimension_info[[header_name]])
}


#' ─────────────────────────────────────────────────────────────────────────────
#' Extract Data from HAR Objects
#' 
#' Extracts and processes data from one or more headers in one or more HAR objects.
#' Returns either a single data frame (for one header) or a list of data frames 
#' (for multiple headers), with options for experiment labeling and data transformation.
#' 
#' @param headers Character vector of header names to extract. Use NULL to extract all headers.
#' @param ... One or more enhanced HAR objects created using `read_harx()`.
#' @param experiment_names Optional character vector of names for each HAR object.
#' @param drop_subtotals Logical; whether to remove subtotal rows.
#' @param rename_cols A named vector for renaming columns (e.g., `c("REG" = "Region")`).
#' @return If single header: a data frame. If multiple headers: a list of data frames.
#' @export
#' @examples
#' # ─── Load HAR Data ───────────────────────────────────────────────────
#' har_data1 <- read_harx("path/to/file1.har")
#' har_data2 <- read_harx("path/to/file2.har")
#'
#' # ─── Extracting a Single Header ──────────────────────────────────────
#' get_har_data("GDP", har_data1)
#'
#' # ─── Extracting Multiple Headers ─────────────────────────────────────
#' get_har_data(c("GDP", "EV"), har_data1)
#'
#' # ─── Extracting Data Across Multiple HAR Files ───────────────────────
#' get_har_data("GDP", har_data1, har_data2, experiment_names = c("baseline", "policy"))
#'
#' # ─── Extract All Headers ─────────────────────────────────────────────
#' get_har_data(NULL, har_data1)   # Using the <NULL>
#' get_har_data("ALL", har_data1)  # Using the word <"ALL">
#' get_har_data(NULL, har_data1, har_data2, experiment_names = c("baseline", "policy"))
get_har_data <- function(headers = "ALL", ..., experiment_names = NULL,
                         drop_subtotals = FALSE, rename_cols = NULL) {
  har_list <- list(...)
  
  if (length(har_list) == 0) {
    stop("At least one HAR object is required.")
  }
  
  # Handle experiment names
  if (is.null(experiment_names)) {
    dots <- match.call(expand.dots = FALSE)$...
    experiment_names <- if (length(dots) == 1) {
      deparse(dots[[1]]) 
    } else {
      as.character(dots)  
    }
  }
  if (length(experiment_names) != length(har_list)) {
    stop("The number of experiment names must match the number of HAR objects.")
  }
  
  # Get all headers if "ALL" or NULL specified
  if (is.null(headers) || (length(headers) == 1 && headers == "ALL")) {
    headers <- unique(unlist(lapply(har_list, function(x) names(x$data))))
  }
  
  # Internal function to extract data from a single header
  extract_header_data <- function(har_obj, header_name, experiment_name) {
    if (!header_name %in% names(har_obj$data)) {
      return(NULL)
    }
    
    var_data <- har_obj$data[[header_name]]
    dim_info <- har_obj$dimension_info[[header_name]]
    
    if (!is.numeric(as.vector(var_data))) {
      return(NULL)
    }
    
    if (is.null(dim_info$dimension_elements)) {
      return(NULL)
    }
    
    df <- as.data.frame(var_data, stringsAsFactors = FALSE)
    
    dim_names <- dim_info$dimension_names
    n_dims <- length(dim_names)
    
    if (n_dims <= 1) {
      df_long <- tibble::tibble(
        Header = header_name,
        Value = as.vector(var_data),
        Dimension = dim_info$dimension_string
      )
    } else {
      processed_dim_names <- process_dim_names(dim_names)
      
      df <- tibble::rownames_to_column(df, var = processed_dim_names[1])
      
      if (n_dims == 2) {
        df_long <- tidyr::pivot_longer(
          df,
          cols = -all_of(processed_dim_names[1]),
          names_to = processed_dim_names[2],
          values_to = "Value"
        )
      } else {
        df_long <- tidyr::pivot_longer(
          df,
          cols = -all_of(processed_dim_names[1]),
          names_to = "temp_col",
          values_to = "Value"
        )
        
        df_long <- tidyr::separate(
          df_long,
          col = "temp_col",
          into = processed_dim_names[-1],
          sep = "\\."
        )
      }
      
      df_long$Dimension <- dim_info$dimension_string
    }
    
    df_long$Header <- header_name
    df_long$Experiment <- experiment_name
    
    if (!is.null(rename_cols)) {
      for (old_name in names(rename_cols)) {
        matching_cols <- which(names(df_long) == old_name)
        if (length(matching_cols) > 0) {
          for (i in seq_along(matching_cols)) {
            new_name <- if (i == 1) rename_cols[old_name] else paste0(rename_cols[old_name], i-1)
            names(df_long)[matching_cols[i]] <- new_name
          }
        }
      }
    }
    
    if (drop_subtotals) {
      df_long <- df_long[!grepl("TOTAL", df_long$Value, ignore.case = TRUE), ]
    }
    
    return(df_long)
  }
  
  # Get all headers if none specified
  if (is.null(headers)) {
    headers <- unique(unlist(lapply(har_list, function(x) names(x$data))))
  } else if (is.character(headers) && length(headers) == 1) {
    # Single header case - will return a single data frame
    header <- headers
    df_list <- list()
    skipped_reasons <- character()
    
    for (i in seq_along(har_list)) {
      har_obj <- har_list[[i]]
      exp_name <- experiment_names[i]
      
      result <- tryCatch({
        df <- extract_header_data(har_obj, header, exp_name)
        
        if (!is.null(df) && nrow(df) > 0 && 
            all(is.numeric(df$Value) | is.na(df$Value))) {
          df <- df[!is.na(df$Value), ]
          if (nrow(df) > 0) {
            df_list[[exp_name]] <- df
          }
        }
      }, error = function(e) {
        warning(sprintf("Skipping header '%s' for experiment '%s': %s",
                        header, exp_name, conditionMessage(e)))
        skipped_reasons <<- c(skipped_reasons, conditionMessage(e))
        NULL
      })
    }
    
    if (length(df_list) == 0) {
      stop(sprintf("Header '%s' could not be processed in any experiment", header))
    }
    
    # Combine all experiments into one data frame
    result <- dplyr::bind_rows(df_list)
    
    # Add attributes
    attr(result, "header") <- header
    attr(result, "skipped_reasons") <- skipped_reasons
    class(result) <- c("har_data", class(result))
    
    return(result)
  }
  
  # Multiple headers case - return a list of data frames
  result_list <- list()
  all_skipped <- list()
  
  for (header in headers) {
    tryCatch({
      # Recursively call the function for each header
      result <- get_har_data(header, ...,
                             experiment_names = experiment_names,
                             drop_subtotals = drop_subtotals,
                             rename_cols = rename_cols)
      
      if (!is.null(result) && nrow(result) > 0) {
        result_list[[header]] <- result
      }
    }, error = function(e) {
      reason <- conditionMessage(e)
      if (!grepl("could not be processed", reason)) {
        warning(sprintf("Could not process header '%s': %s", header, reason))
      }
      all_skipped[[header]] <- reason
    })
  }
  
  if (length(result_list) == 0) {
    stop("No headers could be processed")
  }
  
  # Add attributes for multiple headers
  attr(result_list, "n_headers") <- length(result_list)
  attr(result_list, "headers") <- names(result_list)
  attr(result_list, "skipped_headers") <- all_skipped
  if (length(har_list) > 1) {
    attr(result_list, "experiments") <- experiment_names
  }
  class(result_list) <- c("har_data_list", class(result_list))
  
  return(result_list)
}