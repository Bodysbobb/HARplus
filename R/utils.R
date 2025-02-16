#' Standardize Dimension Pattern (Internal)
#'
#' This is a helper function that standardizes dimension patterns by sorting them.
#'
#' @param pattern A character string representing a dimension pattern.
#' @return A standardized dimension pattern as a character string.
#' @keywords internal
standardize_pattern <- function(pattern) {
  dims <- unlist(strsplit(pattern, "\\*"))
  return(paste(sort(dims), collapse="*"))
}

#' Check if Dimension Patterns Are Equivalent (Internal)
#'
#' This function checks whether two dimension patterns are the same.
#'
#' @param pattern1 First dimension pattern.
#' @param pattern2 Second dimension pattern.
#' @return Logical indicating if patterns are equivalent.
#' @keywords internal
are_patterns_equal <- function(pattern1, pattern2) {
  return(standardize_pattern(tolower(pattern1)) == 
           standardize_pattern(tolower(pattern2)))
}

#' Rename Columns (Internal)
#'
#' A helper function that renames columns inside the function.
#'
#' @param df A data frame.
#' @param rename_cols A named vector where names represent old column names and values represent new names.
#' @return A data frame with renamed columns.
#' @import dplyr tidyr
#' @keywords internal
rename_sl4col <- function(df, rename_cols) {
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

#' Process Dimension Names to Handle Duplicates (Internal)
#'
#' Ensures dimension names are unique by appending suffixes where necessary.
#'
#' @param dim_names A character vector of dimension names.
#' @return A character vector of unique dimension names.
#' @keywords internal
process_dim_names <- function(dim_names) {
  result <- dim_names
  counts <- table(dim_names)
  for(name in names(counts)) {
    if(counts[name] > 1) {
      positions <- which(dim_names == name)
      for(i in seq_along(positions)[-1]) {
        result[positions[i]] <- paste0(name, i-1)
      }
    }
  }
  return(result)
}