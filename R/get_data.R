#' @title Extract Variable Data from SL4 or HAR Objects
#'
#' @description Extracts structured data for one or more variables from SL4 or HAR objects,
#' transforming array-like data into a tidy format.
#'
#' @details
#' - Retrieves specific variables, multiple variables, or all available variables from SL4 or HAR datasets.
#' - Supports merging data from multiple experiments (`merge_data = TRUE`).
#' - Allows renaming of column names (`rename_cols`).
#' - Handles subtotal filtering (`subtotal_level`), controlling whether `"TOTAL"` or decomposed values are retained.
#'
#' @param var_names Character vector. Variable names to extract. Use `"ALL"` or `NULL` to extract all available variables.
#' @param ... One or more SL4 or HAR data objects loaded using `load_sl4x()` or `load_harx()`.
#' @param experiment_names Character vector. Names assigned to each dataset. If `NULL`, names are inferred.
#' @param subtotal_level Character or logical. Determines which decomposition levels to retain:
#'   - `"total"`: Keeps only `"TOTAL"` values.
#'   - `"decomposed"`: Keeps only decomposed values (excludes `"TOTAL"`).
#'   - `"all"`: Keeps all rows.
#'   - `TRUE`: Equivalent to `"all"`, retaining both `"TOTAL"` and decomposed values.
#'   - `FALSE`: Equivalent to `"total"`, keeping only `"TOTAL"` values.
#' @param rename_cols Named vector. Column name replacements (`c("old_name" = "new_name")`).
#' @param merge_data Logical. If `TRUE`, attempts to merge data across multiple experiments. Default is `FALSE`.
#'
#' @return A list of structured data:
#' - If `merge_data = FALSE`, returns a named list where each element corresponds to an experiment.
#' - If `merge_data = TRUE`, returns a named list of all merged data
#'
#' @importFrom stats setNames
#' @importFrom utils tail
#'
#' @author Pattawee Puangchit
#'
#' @seealso \code{\link{get_data_by_dims}}, , \code{\link{group_data_by_dims}}, \code{\link{load_sl4x}}, \code{\link{load_harx}}
#'
#' @export
#'
#' @examples
#' # Import sample data:
#' sl4_data <- load_sl4x(system.file("extdata", "TAR10.sl4", package = "HARplus"))
#' sl4_data1 <- load_sl4x(system.file("extdata", "SUBT10.sl4", package = "HARplus"))
#'
#' # Extract a single variable
#' data_qo <- get_data_by_var("qo", sl4_data)
#'
#' # Extract multiple variables
#' data_multiple <- get_data_by_var(c("qo", "qgdp"), sl4_data)
#'
#' # Extract all variables separately from multiple datasets
#' data_all <- get_data_by_var(NULL, sl4_data, sl4_data1, merge_data = FALSE)
#'
#' # Merge variable data across multiple datasets
#' data_merged <- get_data_by_var(NULL, sl4_data, sl4_data1, merge_data = TRUE)
#'
#' # Retain only "TOTAL" values, removing decomposed components (subtotal_level = "total" or FALSE)
#' data_total_only <- get_data_by_var("qo", sl4_data, subtotal_level = "total")
#' data_total_only_alt <- get_data_by_var("qo", sl4_data, subtotal_level = FALSE)
#'
#' # Retain only decomposed components, removing "TOTAL" (subtotal_level = "decomposed")
#' data_decomposed_only <- get_data_by_var("qo", sl4_data, subtotal_level = "decomposed")
#'
#' # Retain all value levels (subtotal_level = "all" or TRUE)
#' data_all_decomp <- get_data_by_var("qo", sl4_data, subtotal_level = "all")
#' data_all_decomp_alt <- get_data_by_var("qo", sl4_data, subtotal_level = TRUE)
#'
#' # Rename specific columns
#' data_renamed <- get_data_by_var("qo", sl4_data, rename_cols = c(REG = "Region", COMM = "Commodity"))
#'
#' # Merge data across multiple datasets with custom experiment names
#' data_merged_experiments <- get_data_by_var("qo", sl4_data, sl4_data1,
#' experiment_names = c("EXP1", "EXP2"),
#' merge_data = TRUE)
#'
get_data_by_var <- function(var_names = NULL, ..., experiment_names = NULL,
                            subtotal_level = FALSE, rename_cols = NULL, merge_data = FALSE) {
  if (!is.logical(subtotal_level) && !subtotal_level %in% c("all", "total", "decomposed")) {
    stop("subtotal_level must be either logical (TRUE/FALSE) or one of: 'all', 'total', 'decomposed'")
  }

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
    if (length(dim(var_data)) == 0) return(NULL)

    dim_info <- data_obj$dimension_info[[var_name]]
    df <- as.data.frame.table(var_data, stringsAsFactors = FALSE, responseName = "Value")
    setNames(df, c(dim_info$dimension_names))

    if ("subtotal" %in% tolower(names(df))) {
      names(df)[tolower(names(df)) == "subtotal"] <- "Subtotal"
    }

    df$Variable <- var_name
    df$Dimension <- dim_info$dimension_string
    df$Experiment <- experiment_name

    df <- process_decomp_level(df, subtotal_level)
    df <- rename_col(df, rename_cols)
    return(df)
  }

  if (merge_data) {
    result <- lapply(var_names, function(var_name) {
      df_list <- lapply(seq_along(data_list), function(i) {
        process_variable(var_name, data_list[[i]], experiment_names[i])
      })
      df_list <- Filter(Negate(is.null), df_list)
      if (length(df_list) > 0) {
        result <- do.call(rbind, df_list)
        rownames(result) <- NULL
        return(result)
      }
      return(NULL)
    })
    names(result) <- var_names
    result <- Filter(Negate(is.null), result)

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



#' @title Extract Data by Dimension Patterns from SL4 or HAR Objects
#'
#' @description Retrieves structured data from SL4 or HAR objects based on specified dimension patterns.
#' Supports multiple experiments and merging datasets while maintaining structured dimension metadata.
#'
#' @details
#' - Extracts variables matching specified dimension patterns.
#' - Allows for flexible pattern matching (`pattern_mix = TRUE`).
#' - Supports merging data across multiple experiments (`merge_data = TRUE`).
#' - Provides column renaming functionality (`rename_cols`).
#' - Handles subtotal filtering (`subtotal_level`), controlling whether `"TOTAL"` or decomposed values are retained.
#'
#' @param patterns Character vector. Dimension patterns to extract. Use `"ALL"` or `NULL` to extract all available patterns.
#' @param ... One or more SL4 or HAR data objects loaded using `load_sl4x()` or `load_harx()`.
#' @param experiment_names Character vector. Names assigned to each dataset. If `NULL`, names are inferred.
#' @param subtotal_level Character or logical. Determines which decomposition levels to retain:
#'   - `"total"`: Keeps only `"TOTAL"` values.
#'   - `"decomposed"`: Keeps only decomposed values (excludes `"TOTAL"`).
#'   - `"all"`: Keeps all rows.
#'   - `TRUE`: Equivalent to `"all"`, retaining both `"TOTAL"` and decomposed values.
#'   - `FALSE`: Equivalent to `"total"`, keeping only `"TOTAL"` values.
#' @param rename_cols Named vector. Column name replacements (`c("old_name" = "new_name")`).
#' @param merge_data Logical. If `TRUE`, attempts to merge data across multiple experiments. Default is `FALSE`.
#' @param pattern_mix Logical. If `TRUE`, allows flexible pattern matching, ignoring dimension order. Default is `FALSE`.
#'
#' @return A structured list of extracted data:
#' - If `merge_data = FALSE`, returns a named list where each element corresponds to an experiment.
#' - If `merge_data = TRUE`, returns a named list of all merged data
#'
#' @importFrom stats setNames
#' @importFrom utils tail
#'
#' @author Pattawee Puangchit
#'
#' @seealso \code{\link{get_data_by_var}}, \code{\link{group_data_by_dims}}
#'
#' @export
#'
#' @examples
#' # Import sample data:
#' sl4_data <- load_sl4x(
#'   system.file("extdata", "TAR10.sl4", package = "HARplus")
#' )
#' sl4_data1 <- load_sl4x(
#'   system.file("extdata", "SUBT10.sl4", package = "HARplus")
#' )
#'
#' # Extract data for a single dimension pattern
#' data_single_pattern <- get_data_by_dims(
#'   "comm*reg",
#'   sl4_data
#' )
#'
#' # Extract multiple dimension patterns
#' data_multiple_patterns <- get_data_by_dims(
#'   c("comm*reg", "REG*ACTS"),
#'   sl4_data
#' )
#'
#' # Extract all dimension patterns separately from multiple datasets
#' data_all_patterns <- get_data_by_dims(
#'   NULL,
#'   sl4_data, sl4_data1,
#'   merge_data = FALSE
#' )
#'
#' # Merge data for identical patterns across multiple datasets
#' data_merged_patterns <- get_data_by_dims(
#'   NULL,
#'   sl4_data, sl4_data1,
#'   merge_data = TRUE
#' )
#'
#' # Merge data while allowing interchangeable dimensions (e.g., A*B = B*A)
#' data_pattern_mixed <- get_data_by_dims(
#'   NULL,
#'   sl4_data, sl4_data1,
#'   merge_data = TRUE,
#'   pattern_mix = TRUE
#' )
#'
#' # Retain only "TOTAL" values
#' data_total_only <- get_data_by_dims(
#'   "comm*reg",
#'   sl4_data,
#'   subtotal_level = "total"
#' )
#' data_total_only_alt <- get_data_by_dims(
#'   "comm*reg",
#'   sl4_data,
#'   subtotal_level = FALSE
#' )
#'
#' # Retain only decomposed components
#' data_decomposed_only <- get_data_by_dims(
#'   "comm*reg",
#'   sl4_data,
#'   subtotal_level = "decomposed"
#' )
#'
#' # Retain all value levels
#' data_all_decomp <- get_data_by_dims(
#'   "comm*reg",
#'   sl4_data,
#'   subtotal_level = "all"
#' )
#' data_all_decomp_alt <- get_data_by_dims(
#'   "comm*reg",
#'   sl4_data,
#'   subtotal_level = TRUE
#' )
#'
#' # Rename specific columns
#' data_renamed <- get_data_by_dims(
#'   "comm*reg",
#'   sl4_data,
#'   rename_cols = c(REG = "Region", COMM = "Commodity")
#' )
#'
#' # Merge data with custom experiment names
#' data_merged_experiments <- get_data_by_dims(
#'   "comm*reg",
#'   sl4_data, sl4_data1,
#'   experiment_names = c("EXP1", "EXP2"),
#'   merge_data = TRUE
#' )
#'
get_data_by_dims <- function(patterns = NULL, ..., experiment_names = NULL,
                             subtotal_level = FALSE, rename_cols = NULL,
                             merge_data = FALSE, pattern_mix = FALSE) {
  data_list <- list(...)

  if (length(data_list) == 0) {
    stop("At least one data object is required.")
  }

  if (is.null(experiment_names) && !is.null(names(data_list))) {
    experiment_names <- names(data_list)
  } else if (is.null(experiment_names)) {
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

  if (is.null(patterns) || identical(patterns, "ALL")) {
    patterns <- unique(unlist(lapply(data_list, function(x) {
      sapply(x$dimension_info, function(y) y$dimension_string)
    })))
  }

  extract_data <- function(data_obj, pattern, exp_name) {
    matching_vars <- character(0)
    for (var_name in names(data_obj$dimension_info)) {
      x <- data_obj$dimension_info[[var_name]]
      if (!is.null(x$dimension_string)) {
        if (pattern_mix) {
          p1 <- sort(strsplit(tolower(x$dimension_string), "\\*")[[1]])
          p2 <- sort(strsplit(tolower(pattern), "\\*")[[1]])
          if (identical(p1, p2)) matching_vars <- c(matching_vars, var_name)
        } else {
          if (tolower(x$dimension_string) == tolower(pattern)) {
            matching_vars <- c(matching_vars, var_name)
          }
        }
      }
    }

    if (length(matching_vars) == 0) return(NULL)

    df_list <- lapply(matching_vars, function(var_name) {
      var_data <- data_obj$data[[var_name]]
      if (length(dim(var_data)) == 0) return(NULL)

      dim_info <- data_obj$dimension_info[[var_name]]
      df <- as.data.frame.table(var_data, stringsAsFactors = FALSE, responseName = "Value")

      dim_names <- dim_info$dimension_names
      if (length(dim_names) > 0) {
        names(df)[1:length(dim_names)] <- dim_names
      }

      if ("subtotal" %in% tolower(names(df))) {
        names(df)[tolower(names(df)) == "subtotal"] <- "Subtotal"
      }

      df$Variable <- var_name
      df$Dimension <- dim_info$dimension_string
      df$Experiment <- exp_name

      if (!is.logical(subtotal_level) && !subtotal_level %in% c("all", "total", "decomposed")) {
        stop("subtotal_level must be either logical (TRUE/FALSE) or one of: 'all', 'total', 'decomposed'")
      }

      if ("Subtotal" %in% names(df)) {
        df <- switch(
          if(is.logical(subtotal_level)) if(subtotal_level) "all" else "total" else subtotal_level,
          "total" = df[df$Subtotal == "TOTAL", ],
          "decomposed" = df[df$Subtotal != "TOTAL", ],
          df
        )
      }

      if (!is.null(rename_cols)) {
        for (old_name in names(rename_cols)) {
          matches <- which(names(df) == old_name)
          if (length(matches) > 0) {
            for (i in seq_along(matches)) {
              names(df)[matches[i]] <- if(i == 1) rename_cols[old_name] else paste0(rename_cols[old_name], i-1)
            }
          }
        }
      }

      df[!is.na(df$Value), ]
    })

    df_list <- Filter(Negate(is.null), df_list)
    if (length(df_list) == 0) return(NULL)

    result <- do.call(rbind, df_list)
    rownames(result) <- NULL
    result
  }

  if (merge_data) {
    result <- lapply(patterns, function(pattern) {
      pattern_data <- lapply(seq_along(data_list), function(i) {
        extract_data(data_list[[i]], pattern, experiment_names[i])
      })
      pattern_data <- Filter(Negate(is.null), pattern_data)
      if (length(pattern_data) == 0) return(NULL)
      do.call(rbind, pattern_data)
    })
    names(result) <- patterns
    result <- Filter(Negate(is.null), result)

    if (length(result) == 0) {
      stop("No patterns could be processed")
    }

    attr(result, "n_patterns") <- length(result)
    attr(result, "patterns") <- names(result)
    if (length(data_list) > 1) {
      attr(result, "experiments") <- experiment_names
    }
    class(result) <- c("grouped_multi_data", class(result))

  } else {
    result <- lapply(seq_along(data_list), function(i) {
      pattern_data <- lapply(patterns, function(pattern) {
        extract_data(data_list[[i]], pattern, experiment_names[i])
      })
      pattern_data <- Filter(Negate(is.null), pattern_data)
      if (length(pattern_data) == 0) return(NULL)
      names(pattern_data) <- vapply(pattern_data, function(x) unique(x$Dimension)[1], character(1))
      pattern_data
    })
    names(result) <- experiment_names
    result <- Filter(Negate(is.null), result)

    if (length(data_list) == 1) {
      return(result[[1]])
    }

    if (length(result) == 0) {
      stop("No patterns could be processed for any experiment")
    }
  }

  return(result)
}


#' @title Group Data by Dimension Patterns in SL4 or HAR Objects
#'
#' @description Groups extracted SL4 or HAR data based on specified dimension structures and priority rules.
#' Supports automatic renaming, merging, subtotal filtering, and structured metadata handling.
#'
#' @details
#' - Groups extracted variables based on dimension elements.
#' - Applies predefined priority rules to structure the data.
#' - Allows automatic renaming of dimensions (`auto_rename = TRUE`).
#' - Supports merging of grouped data across multiple experiments.
#' - Handles subtotal filtering (`subtotal_level`), controlling whether `"TOTAL"` or decomposed values are retained.
#'
#' @param patterns Character vector. Dimension patterns to extract. Use `"ALL"` or `NULL` to extract all available patterns.
#' @param ... One or more SL4 or HAR objects loaded using `load_sl4x()` or `load_harx()`.
#' @param priority Named list. Specifies priority dimension elements (`c("group_name" = c("dim1", "dim2"))`).
#' @param experiment_names Character vector. Names assigned to each dataset. If `NULL`, names are inferred.
#' @param subtotal_level Character or logical. Determines which decomposition levels to retain:
#'   - `"total"`: Keeps only `"TOTAL"` values.
#'   - `"decomposed"`: Keeps only decomposed values (excludes `"TOTAL"`).
#'   - `"all"`: Keeps all rows.
#'   - `TRUE`: Equivalent to `"all"`, retaining both `"TOTAL"` and decomposed values.
#'   - `FALSE`: Equivalent to `"total"`, keeping only `"TOTAL"` values.
#' @param rename_cols Named vector. Column name replacements (`c("old_name" = "new_name")`).
#' @param auto_rename Logical. If `TRUE`, automatically renames dimensions for consistency. Default is `FALSE`.
#'
#' @return A structured list of grouped data:
#' - A named list where each element corresponds to a dimension size group (e.g., "2D", "3D").
#' - Each group contains dimension-grouped data based on priority rules.
#' - If unmerged data exists, includes a report attribute detailing merge issues.
#'
#' @importFrom stats setNames
#' @importFrom utils tail
#'
#' @author Pattawee Puangchit
#'
#' @seealso \code{\link{get_data_by_dims}}, \code{\link{get_data_by_var}}, \code{\link{load_sl4x}}, \code{\link{load_harx}}
#'
#' @export
#'
#' @examples
#' # Import sample data
#' sl4_data1 <- load_sl4x(system.file("extdata", "TAR10.sl4", package = "HARplus"))
#' sl4_data2 <- load_sl4x(system.file("extdata", "SUBT10.sl4", package = "HARplus"))
#'
#' # Case 1: Multiple priority levels (Sector then Region) with auto_rename
#' priority_list <- list(
#'   "Sector" = c("COMM", "ACTS"),
#'   "Region" = c("REG")
#' )
#' grouped_data_multiple <- group_data_by_dims(
#'   patterns = "ALL",
#'   sl4_data1,
#'   priority = priority_list,
#'   auto_rename = TRUE
#' )
#'
#' # Case 2: Single priority (Region only) with auto_rename
#' priority_list <- list("Region" = c("REG"))
#' grouped_data_single <- group_data_by_dims(
#'   patterns = "ALL",
#'   sl4_data1, sl4_data2,
#'   priority = priority_list,
#'   auto_rename = TRUE
#' )
#'
#' # Case 3: Multiple priorities without auto_rename
#' priority_list <- list(
#'   "Sector" = c("COMM", "ACTS"),
#'   "Region" = c("REG")
#' )
#' grouped_data_no_rename <- group_data_by_dims(
#'   patterns = "ALL",
#'   sl4_data1,
#'   priority = priority_list,
#'   auto_rename = FALSE
#' )
group_data_by_dims <- function(patterns = NULL, ..., priority,
                               rename_cols = NULL, experiment_names = NULL,
                               subtotal_level = FALSE, auto_rename = FALSE) {
  if (is.null(experiment_names)) {
    dots <- match.call(expand.dots = FALSE)$...
    experiment_names <- if (length(dots) == 1) {
      deparse(dots[[1]])
    } else {
      vapply(dots, deparse, character(1))
    }
  }

  data_list <- list(...)

  all_dims <- get_dim_elements(...)$DimName
  priority_elements <- unlist(priority)
  auto_rename_elements <- setdiff(all_dims, priority_elements)

  extracted_data <- get_data_by_dims(
    patterns = patterns, ...,
    pattern_mix = TRUE,
    merge_data = TRUE,
    subtotal_level = subtotal_level,
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

  handle_duplicate_cols <- function(df) {
    col_counts <- table(colnames(df))
    duplicate_cols <- names(col_counts[col_counts > 1])

    if (length(duplicate_cols) > 0) {
      for (col in duplicate_cols) {
        col_indices <- which(colnames(df) == col)
        for (i in seq_along(col_indices)[-1]) {
          colnames(df)[col_indices[i]] <- paste0(col, ".", i-1)
        }
      }
    }
    return(df)
  }

  process_dimension <- function(dim_size, patterns, priority_list) {
    current_patterns <- patterns[dim_sizes == dim_size]
    if (length(current_patterns) == 0) return(list())

    if (auto_rename) {
      dimension_groups <- list()

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

              df <- handle_duplicate_cols(df)
              return(df)
            })

            common_cols <- Reduce(intersect, lapply(df_list, colnames))
            merged_df <- do.call(rbind, lapply(df_list, function(x) {
              x[, common_cols, drop = FALSE]
            }))

            dimension_groups[[prio_name]] <- merged_df
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
                df <- handle_duplicate_cols(df)
                return(df)
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
          df <- extracted_data[[pattern]]
          df <- handle_duplicate_cols(df)
          dimension_groups[[pattern]] <- df
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
              # Handle duplicates in column names
              df <- handle_duplicate_cols(df)
              return(df)
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
          dimension_groups$unmerged <- lapply(extracted_data[unmerged_patterns], handle_duplicate_cols)
        }

      } else {
        for (pattern in current_patterns) {
          df <- extracted_data[[pattern]]
          df <- handle_duplicate_cols(df)
          dimension_groups[[pattern]] <- df
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
        rename_col(df, rename_cols)
      })
    })
  }

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
