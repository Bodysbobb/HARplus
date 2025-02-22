#' @title Get Variable Structure Summary from SL4 and HAR Objects
#'
#' @description Generates a summary of the variables within one or more SL4 or HAR objects, listing their
#' dimension sizes, structures, and optionally, column and observation counts.
#'
#' @details
#' - Extracts dimension structures for variables in one or more SL4 or HAR datasets.
#' - If `include_col_size = TRUE`, adds column and observation counts.
#' - Supports multiple datasets and returns results as a named list, with each dataset’s summary stored separately.
#' - Can summarize specific variables or `"ALL"`.
#'
#' @param variables Character vector. Variable names to summarize. Use `NULL` or `"ALL"` to summarize all variables.
#' @param ... One or more SL4 or HAR objects created using `load_sl4x()` or `load_harx()`.
#' @param include_col_size Logical. If `TRUE`, includes column and observation counts. Default is `FALSE`.
#'
#' @return A named list, where each element contains a data frame with:
#' - `Variable`: The variable name.
#' - `Dimensions`: The associated dimensions.
#' - `DimSize`: The number of dimensions.
#' - `DataShape`: The shape of the data (e.g., `10x20x30`).
#' - `No.Col`: (Optional) The number of columns.
#' - `No.Obs`: (Optional) The number of observations.
#'
#' @importFrom stats setNames
#'
#' @author Pattawee Puangchit
#'
#' @seealso \code{\link{get_dim_patterns}}, \code{\link{get_dim_elements}}
#'
#' @export
#'
#' @examples
#' # Import data sample:
#' sl4_data <- load_sl4x(system.file("extdata", "TAR10.sl4", package = "HARplus"))
#' sl4_data1 <- load_sl4x(system.file("extdata", "SUBT10.sl4", package = "HARplus"))
#'
#' # Get summary for all variables in a single dataset
#' get_var_structure(data_obj = sl4_data)
#'
#' # Get summary for specific variables
#' get_var_structure(c("gdp", "trade"), sl4_data)
#'
#' # Include column and observation counts
#' get_var_structure("ALL", sl4_data, include_col_size = TRUE)
#'
#' # Compare structures across multiple datasets
#' get_var_structure("ALL", sl4_data, sl4_data1)
#'
#' # Include column and observation counts across multiple datasets
#' get_var_structure("ALL", sl4_data, sl4_data1, include_col_size = TRUE)
#'
get_var_structure <- function(variables = NULL, ..., include_col_size = FALSE) {
  data_list <- list(...)
  if (length(data_list) == 0) {
    stop("At least one data object is required.")
  }

  dots <- match.call(expand.dots = FALSE)$...
  dataset_names <- if (!is.null(names(data_list))) {
    names(data_list)
  } else if (length(dots) == 1) {
    deparse(dots[[1]])
  } else {
    vapply(dots, deparse, character(1))
  }

  result <- lapply(seq_along(data_list), function(i) {
    data_obj <- data_list[[i]]

    if (is.null(variables) || identical(variables, "ALL")) {
      var_list <- names(data_obj$dimension_info)
    } else {
      var_list <- variables[variables %in% names(data_obj$dimension_info)]
    }

    var_list <- sort(var_list)

    if (length(var_list) == 0) return(data.frame())

    var_info <- lapply(var_list, function(var_name) {
      info <- get_dim_info(data_obj$dimension_info[[var_name]])
      df_row <- data.frame(
        Variable = var_name,
        Dimensions = info$dimension_string,
        DimSize = info$dim_size,
        DataShape = info$data_shape,
        stringsAsFactors = FALSE
      )
      if (include_col_size) {
        df_row$No.Col <- info$col_size
        df_row$No.Obs <- info$n_obs
      }
      df_row
    })

    do.call(rbind, var_info)
  })

  names(result) <- dataset_names
  return(result)
}


#' @title Compare Variable Structures Across SL4 and HAR Objects
#'
#' @description Compares variable structures across multiple SL4 and HAR datasets to ensure compatibility.
#' Identifies matching and mismatched variable structures, helping users diagnose inconsistencies.
#'
#' @details
#' - Verifies whether variables have consistent structures across multiple datasets.
#' - Ensures correct ordering of dimensions and checks for structural compatibility.
#' - If `keep_unique = TRUE`, returns a list of unique variable structures instead of performing a direct comparison.
#' - Useful for merging or aligning datasets before further processing.
#' - Helps detect differences in variable dimensions, which may arise due to model updates or dataset variations.
#'
#' @param variables Character vector. Variable names to compare. Use `NULL` or `"ALL"` to compare all variables.
#' @param ... Named SL4 or HAR objects to compare.
#' @param keep_unique Logical. If `TRUE`, returns unique variable structures across datasets instead of checking for compatibility. Default is `FALSE`.
#'
#' @return A list containing:
#' - `match`: A data frame listing variables with identical structures across datasets.
#' - `diff`: A data frame listing variables with mismatched structures, useful for debugging and alignment.
#' - If `keep_unique = TRUE`, instead of `match` and `diff`, returns a data frame with distinct variable structures across datasets.
#'
#' @importFrom stats setNames
#'
#' @author Pattawee Puangchit
#'
#' @seealso \code{\link{get_var_structure}}, \code{\link{get_dim_patterns}}, \code{\link{get_dim_elements}}
#'
#' @export
#'
#' @examples
#' # Import sample data:
#' har_data1 <- load_harx(system.file("extdata", "TAR10-WEL.har", package = "HARplus"))
#' har_data2 <- load_harx(system.file("extdata", "SUBT10-WEL.har", package = "HARplus"))
#'
#' # Compare structure for a single variable across multiple datasets
#' compare_var_structure("A", har_data1, har_data2)
#'
#' # Compare structure for multiple variables across multiple datasets
#' comparison_multiple <- compare_var_structure(c("A", "E1"), har_data1, har_data2)
#'
#' # Extract unique variable structures across multiple datasets
#' unique_vars <- compare_var_structure("ALL", har_data1, har_data2, keep_unique = TRUE)
#'
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


# Dimension Summary ------------------------------------------------------------

#' @title Get Dimension Patterns from SL4 and HAR Objects
#'
#' @description Extracts and lists unique dimension patterns (e.g., `REG*COMM`, `REG*REG*ACTS`) from one or more datasets.
#'
#' @details
#' - Extracts dimension structure details from the dataset.
#' - If multiple datasets are provided, combines their dimension information.
#' - If `keep_unique = TRUE`, returns only distinct dimension patterns.
#'
#' @param ... One or more structured SL4 or HAR objects containing dimension information.
#' @param keep_unique Logical. If `TRUE`, returns only unique dimension patterns. Default is `FALSE`.
#'
#' @return A data frame containing:
#' - `DimPattern`: The unique dimension patterns.
#'
#' @author Pattawee Puangchit
#'
#' @seealso \code{\link{get_dim_elements}}, \code{\link{get_var_structure}}
#'
#' @export
#'
#' @examples
#' # Import sample data:
#' sl4_data <- load_sl4x(system.file("extdata", "TAR10.sl4", package = "HARplus"))
#' sl4_data2 <- load_sl4x(system.file("extdata", "SUBT10.sl4", package = "HARplus"))
#'
#' # Extract dimension patterns
#' get_dim_patterns(sl4_data)
#'
#' # Extract only unique dimension patterns across datasets
#' get_dim_patterns(sl4_data, sl4_data2, keep_unique = TRUE)
#'
get_dim_patterns <- function(..., keep_unique = FALSE) {
  data_objs <- list(...)
  if (length(data_objs) == 0) stop("At least one data object is required.")

  patterns <- unlist(lapply(data_objs, function(obj) {
    sapply(obj$dimension_info, `[[`, "dimension_string")
  }))

  data.frame(
    DimPattern = if(keep_unique) unique(patterns) else patterns,
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}


#' @title Get Dimension Elements from SL4 and HAR Objects
#'
#' @description Extracts and lists unique dimension elements (e.g., `REG`, `COMM`, `ACTS`) from one or more datasets.
#'
#' @param ... One or more structured SL4 or HAR objects containing dimension information.
#' @param keep_unique Logical. If `TRUE`, returns only unique dimension elements across inputs. Default is `FALSE`.
#'
#' @return A data frame containing unique dimension elements.
#'
#' @author Pattawee Puangchit
#'
#' @seealso \code{\link{get_dim_patterns}}, \code{\link{get_var_structure}}
#'
#' @export
#'
#' @examples
#' # Import sample data:
#' sl4_data1 <- load_sl4x(system.file("extdata", "TAR10.sl4", package = "HARplus"))
#' sl4_data2 <- load_sl4x(system.file("extdata", "SUBT10.sl4", package = "HARplus"))
#'
#'
#' # Extract dimension elements from a single dataset
#' get_dim_elements(sl4_data1)
#'
#' # Extract dimension elements from multiple datasets
#' get_dim_elements(sl4_data1, sl4_data2)
#'
#' # Extract unique dimension elements across datasets
#' get_dim_elements(sl4_data1, sl4_data2, keep_unique = TRUE)
#'
get_dim_elements <- function(..., keep_unique = FALSE) {
  data_objs <- list(...)
  if (length(data_objs) == 0) stop("At least one data object is required.")

  elements <- unlist(lapply(data_objs, function(obj) {
    unique(unlist(lapply(obj$dimension_info, `[[`, "dimension_names")))
  }))

  data.frame(
    DimName = if(keep_unique) unique(elements) else elements,
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}
