# Helper for read_sl4x ----------------------------------------------------

#' Standardize dimension pattern
#' @param pattern Dimension pattern string
#' @return Standardized dimension pattern
standardize_pattern <- function(pattern) {
  dims <- unlist(strsplit(pattern, "\\*"))
  return(paste(sort(dims), collapse="*"))
}

#' Check if dimension patterns are equivalent
#' @param pattern1 First dimension pattern
#' @param pattern2 Second dimension pattern
#' @return Logical indicating if patterns are equivalent
are_patterns_equal <- function(pattern1, pattern2) {
  return(standardize_pattern(tolower(pattern1)) == 
           standardize_pattern(tolower(pattern2)))
}


#' Renaming columns inside the function
#' @name sl4_package
#' @import dplyr tidyr
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


#' Process Dimension Names to Handle Duplicates
#' @param dim_names A character vector of dimension names.
#' @return A character vector of unique dimension names.
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