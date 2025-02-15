#' Enhanced read for GEMPACK HAR files
#' 
#' Reads a GEMPACK HAR file and enhances the extracted data by incorporating 
#' detailed dimension information for each header. The function allows for customization, 
#' such as using coefficient names instead of headers and converting names to lowercase.
#' 
#' @param con File path or connection to the HAR file.
#' @param useCoefficientsAsNames Logical; whether to use coefficient names instead of headers.
#' @param headersToRead Specific headers to read; if NULL, all headers are read.
#' @return An enhanced HAR object containing extracted data along with dimension details.
#' @export
read_harx <- function(con, 
                      useCoefficientsAsNames = FALSE,
                      headersToRead = NULL) {
  
  har_data <- HARr::read_har(con, 
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



#' Get header summary from HAR object
#' 
#' Extracts a summary of the headers within an enhanced HAR object, providing 
#' details about their dimensions and sizes. The output is a structured data frame.
#' 
#' @param har_obj An enhanced HAR object created using `read_harx()`.
#' @return A data frame containing the header names, dimension count, and 
#'         a formatted dimension string.
#' @export
get_dims_har <- function(har_obj) {
  variables <- character(0)
  sizes <- numeric(0)
  dimensions <- character(0)
  
  for (var_name in names(har_obj$dimension_info)) {
    dim_info <- har_obj$dimension_info[[var_name]]
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


#' Get dimension information for a header
#' 
#' Retrieves detailed dimension information for a specific header within an 
#' enhanced HAR object. It returns the structure of dimensions, including 
#' their names, sizes, and element labels.
#' 
#' @param har_obj An enhanced HAR object created using `read_harx()`.
#' @param var_name Name of the header whose dimension details are to be retrieved.
#' @return A list containing the dimension names, sizes, and elements for the specified header.
#' @export
get_var_summary_har <- function(har_obj, var_name) {
  if (!var_name %in% names(har_obj$dimension_info)) {
    stop(sprintf("Variable '%s' not found", var_name))
  }
  return(har_obj$dimension_info[[var_name]])
}


#' Extract Data from a Specific Header in HAR Object
#' 
#' Extracts and transforms data from a specific header in an enhanced HAR object 
#' into a structured long-format data frame. The function supports renaming columns, 
#' dropping subtotal rows, and adding experiment identifiers.
#' 
#' @param har_obj An enhanced HAR object created using `read_harx()`.
#' @param header_name The name of the header to extract.
#' @param add_experiment Logical; whether to include an experiment column.
#' @param experiment_name Name of the experiment; if NULL, derived from `har_obj`.
#' @param drop_subtotals Logical; whether to remove subtotal rows.
#' @param rename_cols A named vector for renaming columns (e.g., `c("REG" = "Region")`).
#' @return A long-format data frame containing the extracted data.
#' @export
get_data_har <- function(har_obj, header_name, add_experiment = FALSE, 
                         experiment_name = NULL, drop_subtotals = FALSE, 
                         rename_cols = NULL) {
  if (!header_name %in% names(har_obj$data)) {
    stop(sprintf("Header '%s' not found", header_name))
  }
  
  if (is.null(experiment_name)) {
    experiment_name <- deparse(substitute(har_obj))
  }
  
  var_data <- har_obj$data[[header_name]]
  dim_info <- har_obj$dimension_info[[header_name]]
  
  if (!is.numeric(as.vector(var_data))) {
    return(NULL)
  }
  
  if (is.null(dim_info$dimension_elements)) {
    stop("No dimension elements found for the selected header.")
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
  
  if (add_experiment) {
    df_long$Experiment <- experiment_name
  }
  
  if (drop_subtotals) {
    df_long <- df_long[!grepl("TOTAL", df_long$Value, ignore.case = TRUE), ]
  }
  
  return(df_long)
}


#' Extract Data from All Headers in HAR Object
#' 
#' Processes all headers within an enhanced HAR object and extracts structured data 
#' from each, returning a named list of data frames. The function allows for renaming 
#' columns, removing subtotal rows, and adding experiment identifiers.
#' 
#' @param har_obj An enhanced HAR object created using `read_harx()`.
#' @param add_experiment Logical; whether to include an experiment column.
#' @param experiment_name Name of the experiment; if NULL, derived from `har_obj`.
#' @param drop_subtotals Logical; whether to remove subtotal rows.
#' @param rename_cols A named vector for renaming columns (e.g., `c("REG" = "Region")`).
#' @return A named list of structured data frames, each corresponding to a header.
#' @export
get_all_har <- function(har_obj, 
                        add_experiment = FALSE,
                        experiment_name = NULL,
                        drop_subtotals = FALSE,
                        rename_cols = NULL) {
  
  headers <- names(har_obj$data)
  
  if (is.null(experiment_name)) {
    experiment_name <- deparse(substitute(har_obj))
  }
  
  all_results <- list()
  skipped_headers <- character()
  skipped_reasons <- character()
  
  for (header in headers) {
    result <- tryCatch({
      get_data_har(
        har_obj = har_obj,
        header_name = header,
        add_experiment = add_experiment,
        experiment_name = experiment_name,
        drop_subtotals = drop_subtotals,
        rename_cols = rename_cols
      )
    }, error = function(e) {
      skipped_headers <<- c(skipped_headers, header)
      skipped_reasons <<- c(skipped_reasons, conditionMessage(e))
      return(NULL)
    })
    
    if (!is.null(result)) {

      if (all(is.numeric(result$Value) | is.na(result$Value))) {

        result <- result[!is.na(result$Value), ]
        if (nrow(result) > 0) {
          all_results[[header]] <- result
        } else {
          skipped_headers <- c(skipped_headers, header)
          skipped_reasons <- c(skipped_reasons, "All values were NA")
        }
      } else {
        skipped_headers <- c(skipped_headers, header)
        skipped_reasons <- c(skipped_reasons, "Non-numeric values")
      }
    } else {
      if (!(header %in% skipped_headers)) {
        skipped_headers <- c(skipped_headers, header)
        skipped_reasons <- c(skipped_reasons, "Non-numeric data")
      }
    }
  }
  
  attr(all_results, "experiment") <- experiment_name
  attr(all_results, "creation_date") <- Sys.time()
  attr(all_results, "original_file") <- har_obj$metadata$filename
  attr(all_results, "skipped_headers") <- data.frame(
    Header = skipped_headers,
    Reason = skipped_reasons,
    stringsAsFactors = FALSE
  )
  
  class(all_results) <- c("har_data_list", "list")
  
  if (length(skipped_headers) > 0) {
    cat("\nSkipped Headers Summary:\n")
    for (i in seq_along(skipped_headers)) {
      cat(sprintf("- %s: %s\n", skipped_headers[i], skipped_reasons[i]))
    }
  }
  
  return(all_results)
}


#' Extract and Combine Data for a Specific Header from Multiple HAR Files
#' 
#' Aggregates data for a specific header across multiple enhanced HAR objects, 
#' transforming and combining them into a single long-format data frame. 
#' The function supports experiment labeling, renaming columns, and subtotal filtering.
#' 
#' @param header_name The name of the header to extract.
#' @param ... One or more enhanced HAR objects created using `read_harx()`.
#' @param experiment_names A character vector of experiment names corresponding to each HAR object.
#' @param drop_subtotals Logical; whether to remove subtotal rows.
#' @param rename_cols A named vector for renaming columns (e.g., `c("REG" = "Region")`).
#' @return A long-format data frame containing the combined extracted data.
#' @export
get_var_data_multi_har <- function(header_name, ..., experiment_names = NULL, 
                                   drop_subtotals = FALSE, rename_cols = NULL) {
  
  har_list <- list(...)
  num_files <- length(har_list)
  
  if (num_files == 0) {
    stop("At least one HAR object must be provided.")
  }
  
  if (is.null(experiment_names)) {
    experiment_names <- paste0("Exp", seq_len(num_files))
  } else if (length(experiment_names) != num_files) {
    stop("The number of experiment names must match the number of HAR objects.")
  }
  
  combined_data <- list()
  
  for (i in seq_along(har_list)) {
    har_obj <- har_list[[i]]
    experiment_name <- experiment_names[i]
    
    data <- tryCatch({
      get_data_har(
        har_obj = har_obj,
        header_name = header_name,
        add_experiment = TRUE,
        experiment_name = experiment_name,
        drop_subtotals = drop_subtotals,
        rename_cols = rename_cols
      )
    }, error = function(e) {
      warning(sprintf("Skipping header '%s' for experiment '%s': %s", 
                      header_name, experiment_name, conditionMessage(e)))
      return(NULL)
    })
    
    if (!is.null(data)) {
      combined_data[[experiment_name]] <- data
    }
  }
  
  final_data <- dplyr::bind_rows(combined_data)
  return(final_data)
}


#' Extract and Combine Data from All Headers Across Multiple HAR Files
#' 
#' Combines extracted data from multiple enhanced HAR objects, processing all headers 
#' in each file. The function ensures consistency across experiments, applies transformations, 
#' and structures the data into a named list of data frames.
#' 
#' @param ... One or more enhanced HAR objects created using `read_harx()`.
#' @param experiment_names A character vector of experiment names corresponding to each HAR object.
#' @param drop_subtotals Logical; whether to remove subtotal rows.
#' @param rename_cols A named vector for renaming columns (e.g., `c("REG" = "Region")`).
#' @return A named list of structured data frames, each representing a header across all HAR files.
#' @export
get_all_multi_har <- function(..., experiment_names = NULL, 
                              drop_subtotals = FALSE, rename_cols = NULL) {
  
  har_list <- list(...)
  num_files <- length(har_list)
  
  if (num_files == 0) {
    stop("At least one HAR object must be provided.")
  }
  
  if (is.null(experiment_names)) {
    experiment_names <- paste0("Exp", seq_len(num_files))
  } else if (length(experiment_names) != num_files) {
    stop("The number of experiment names must match the number of HAR objects.")
  }
  
  all_combined_results <- list()
  all_skipped_headers <- list()
  
  for (i in seq_along(har_list)) {
    har_obj <- har_list[[i]]
    experiment_name <- experiment_names[i]
    
    result <- tryCatch({
      get_all_har(
        har_obj = har_obj,
        add_experiment = TRUE,
        experiment_name = experiment_name,
        drop_subtotals = drop_subtotals,
        rename_cols = rename_cols
      )
    }, error = function(e) {
      warning(sprintf("Skipping file '%s': %s", experiment_name, conditionMessage(e)))
      return(NULL)
    })
    
    if (!is.null(result)) {
      for (header in names(result)) {
        if (!header %in% names(all_combined_results)) {
          all_combined_results[[header]] <- list()
        }
        all_combined_results[[header]][[experiment_name]] <- result[[header]]
      }
    } else {
      all_skipped_headers[[experiment_name]] <- attr(result, "skipped_headers")
    }
  }
  
  final_data <- list()
  for (header in names(all_combined_results)) {
    final_data[[header]] <- dplyr::bind_rows(all_combined_results[[header]])
  }
  
  attr(final_data, "experiment_names") <- experiment_names
  attr(final_data, "creation_date") <- Sys.time()
  attr(final_data, "skipped_headers") <- all_skipped_headers
  
  class(final_data) <- c("multi_har_data", "list")
  
  return(final_data)
}