#' @title Load and Process HAR Files with Header Selection
#'
#' @description Reads a GEMPACK HAR file and extracts structured data while maintaining compatibility 
#' with standard HAR formats. Provides flexibility in naming conventions and header selection.
#'
#' @details
#' - Uses `load_harplus()` internally for efficient HAR file reading.
#' - Allows optional conversion of variable names to lowercase (`lowercase = TRUE`).
#' - Supports coefficient-based naming (`coefAsname = TRUE`).
#' - Enables selective header extraction via `select_header = c("A", "E1")`.
#' - Returns structured data with explicit dimension names and sizes.
#'
#' @param file_path Character. The file path to the HAR file.
#' @param coefAsname Logical. If `TRUE`, replaces four-letter headers with coefficient names when available. Default is `FALSE`.
#' @param lowercase Logical. If `TRUE`, converts all variable names to lowercase. Default is `FALSE`.
#' @param select_header Character vector. Specific headers to read; if `NULL`, all headers are read. Example: `select_header = c("A", "E1")`.
#'
#' @return A structured list containing:
#' - `data`: Extracted HAR variable data stored as matrices, arrays, or vectors.
#' - `dimension_info`: A list with:
#'     - `dimension_string`: A textual representation of dimensions (e.g., "REG*COMM*YEAR").
#'     - `dimension_names`: The names of each dimension.
#'     - `dimension_sizes`: The size of each dimension.
#'
#' @author Pattawee Puangchit
#'   
#' @seealso \code{\link{load_sl4x}}, \code{\link{get_data_by_var}}, \code{\link{get_data_by_dims}}
#'
#' @export
#'
#' @examples
#' # Path to example files
#' har_path <- system.file("extdata", "TAR10-WEL.har", package = "HARplus")
#' 
#' # Basic loading
#' har_data <- load_harx(har_path)
#' 
#' # Load with coefficient names
#' har_data_coef <- load_harx(har_path, coefAsname = TRUE)
#' 
#' # Load with lowercase names
#' har_data_lower <- load_harx(har_path, lowercase = TRUE)
#' 
#' # Load specific headers
#' har_selected <- load_harx(har_path, select_header = c("A", "E1"))
#' 
#' # Load with multiple options
#' har_combined <- load_harx(har_path,
#'                          coefAsname = TRUE,
#'                          lowercase = TRUE,
#'                          select_header = c("A", "E1"))
#'
load_harx <- function(file_path, coefAsname = FALSE, lowercase = FALSE, select_header = NULL) {
  har_data <- load_harplus(file_path, 
                           coefAsname = coefAsname,
                           lowercase = lowercase,
                           select_header = select_header)
  
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
    dimension_info = dimension_info
  )
  
  class(enhanced_results) <- "enhanced_har"
  
  return(enhanced_results)
}