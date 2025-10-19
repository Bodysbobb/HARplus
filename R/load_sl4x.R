#' @title Load and Process SL4 Files with Enhanced Options
#' 
#' @description Reads an SL4 file and processes its structured data into an enhanced SL4 object. 
#' Extracts structured variable information, dimensions, and handles subtotal columns.
#'
#' @details
#' - Uses `load_harplus()` internally for optimized SL4 file reading.
#' - Extracts variable names, dimension structures, and metadata.
#' - Converts variable names to lowercase if `lowercase = TRUE`.
#' - Allows the selection of specific headers using `select_header`.
#' - Returns structured data with explicit dimension names and sizes.
#'
#' @param file_path Character. The full path to the SL4 file to be read.
#' @param lowercase Logical. If `TRUE`, converts all variable names to lowercase. Default is `FALSE`.
#' @param select_header Character vector. Specific headers to extract; if `NULL`, all headers are read.
#'
#' @return A structured list containing:
#' - `data`: Extracted SL4 variable data, stored as arrays or matrices.
#' - `dimension_info`: A list with:
#'     - `dimension_string`: A textual representation of dimensions (e.g., "REG*COMM*YEAR").
#'     - `dimension_names`: The names of each dimension.
#'     - `dimension_sizes`: The size of each dimension.
#'
#' @author Pattawee Puangchit
#' 
#' @seealso \code{\link{load_harx}}, \code{\link{get_data_by_var}}, \code{\link{get_data_by_dims}}
#'
#' @export
#'
#' @examples
#' # Path to example files
#' sl4_path <- system.file("extdata", "TAR10.sl4", package = "HARplus")
#' 
#' # Basic loading
#' sl4_data <- load_sl4x(sl4_path)
#' 
#' # Load with lowercase names
#' sl4_data_lower <- load_sl4x(sl4_path, lowercase = TRUE)
#' 
#' # Load specific headers
#' sl4_selected <- load_sl4x(sl4_path, select_header = c("qo", "qgdp"))
#' 
load_sl4x <- function(file_path, lowercase = FALSE, select_header = NULL) {
  if (!file.exists(file_path)) {
    stop(sprintf("File '%s' does not exist", file_path))
  }
  
  solution <- load_harplus(file_path, lowercase = lowercase)
  subtotals <- c('TOTAL', solution$STDS)
  dimension_info <- list()
  
  var_indices <- seq_along(solution$VCNM)
  
  for (v in var_indices) {
    var_name <- solution$VCNM[v]
    if (solution$VCNI[v] > 0) {
      dim_indices <- solution$VCSP[v]:(solution$VCSP[v] + solution$VCNI[v] - 1)
      dimensions <- solution$VCSN[dim_indices]
      dim_names <- solution$STNM[dimensions]
      dim_sizes <- solution$SSZ[dimensions]
      dim_string <- paste(dim_names, collapse = "*")
      
      dimension_info[[var_name]] <- list(
        dimension_string = dim_string,
        dimension_names = dim_names,
        dimension_sizes = dim_sizes
      )
    }
  }
  
  results <- lapply(var_indices, function(f) {
    if (solution$VCNI[f] > 0) {
      dimensions <- solution$VCSN[solution$VCSP[f]:(solution$VCSP[f] + solution$VCNI[f] - 1)]
      sizes <- c(solution$SSZ[dimensions], length(subtotals))
      labels <- c(
        lapply(dimensions, function(g) {
          if (solution$SSZ[g] == 0) {
            character(0)
          } else {
            solution$STEL[solution$ELAD[g]:(solution$ELAD[g] + solution$SSZ[g] - 1)]
          }
        }),
        list(subtotals)
      )
      names(labels) <- c(solution$STNM[dimensions], 'subtotal')
    } else {
      sizes <- c(length(subtotals))
      labels <- list(subtotals)
      names(labels) <- c('subtotal')
    }
    array(NA, dim = sizes, dimnames = labels)
  })
  names(results) <- solution$VCNM
  
  partials <- solution$OREX > 0 & solution$OREX != solution$VNCP
  stHeaders <- c('CUMS', sprintf('%03dS', seq_along(solution$STDS)))
  
  complete_indices <- which(partials == FALSE & solution$PCUM > 0)
  
  for (v in complete_indices) {
    range <- solution$PCUM[v]:(solution$PCUM[v] + solution$ORND[v] - 1)
    results[[solution$VARS[v]]][] <- unlist(lapply(stHeaders, function(f) solution[[f]][range]))
  }
  
  zero_indices <- which(partials == FALSE & solution$PCUM == 0)
  
  if (length(zero_indices) > 0) {
    shoc_row <- 1
    for (v in zero_indices) {
      if (!is.null(solution$PSHK) && !is.null(solution$SHOC) && solution$PSHK[v] == 1) {
        shock_values <- solution$SHOC[shoc_row:(shoc_row + solution$OREX[v] - 1), 1]
        for (st_idx in seq_along(stHeaders)) {
          results[[solution$VARS[v]]][, st_idx] <- shock_values
        }
        shoc_row <- shoc_row + solution$OREX[v]
      } else {
        results[[solution$VARS[v]]][] <- 0
      }
    }
  }
  
  partial_indices <- which(partials == TRUE & solution$PCUM > 0)
  
  start <- 1
  for (v in partial_indices) {
    range <- solution$PCUM[v]:(solution$PCUM[v] + solution$ORND[v] - 1)
    positions <- solution$ORNL[start - 1 + (1:solution$ORND[v])]
    start <- solution$ORND[v] + start
    toFill <- logical(solution$VNCP[v])
    toFill[positions] <- TRUE
    results[[solution$VARS[v]]][toFill] <- unlist(lapply(stHeaders, function(f) solution[[f]][range]))
  }
  
  if (!is.null(select_header)) {
    results <- results[select_header]
    dimension_info <- dimension_info[select_header]
  }
  
  enhanced_results <- list(
    data = results,
    dimension_info = dimension_info
  )
  
  class(enhanced_results) <- c("sl4")
  return(enhanced_results)
}