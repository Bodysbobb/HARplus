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
    dimension_info = dimension_info
  )
  
  class(enhanced_results) <- "enhanced_har"
  
  return(enhanced_results)
}