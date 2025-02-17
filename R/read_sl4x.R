#' Read and Process SL4 Data
#' 
#' Reads an SL4 file and processes its structured data into an enhanced SL4 object. 
#' Extracts dimensions, and variables while handling subtotal information.
#' 
#' @param filename The path to the SL4 file.
#' @param toLowerCase Logical; whether to convert variable names to lowercase.
#' @return An enhanced SL4 object containing structured data, and dimension details.
#' @export
#' @examples
#' # ─── Load SL4 Data ─────────────────────────────────────────────────
#' sl4_path <- "path/to/file.sl4"
#' sl4_data <- read_sl4x(sl4_path)
#' 
#' # ─── Load SL4 Data with lowercase -─────────────────────────────────
#' sl4_data <- read_sl4x(sl4_path, toLowerCase = TRUE)
read_sl4x <- function(filename, toLowerCase = FALSE) {
  if (!file.exists(filename)) {
    stop(sprintf("File '%s' does not exist", filename))
  }
  
  solution <- rhar_enhanced(filename, toLowerCase = toLowerCase)
  subtotals <- c('TOTAL', solution$STDS)
  dimension_info <- list()
  
  for(v in 1:length(solution$VCNM)) {
    var_name <- solution$VCNM[v]
    if(solution$VCNI[v] > 0) {
      dim_indices <- solution$VCSP[v]:(solution$VCSP[v] + solution$VCNI[v] - 1)
      dimensions <- solution$VCSN[dim_indices]
      dim_names <- solution$STNM[dimensions]
      dim_sizes <- solution$SSZ[dimensions]
      dim_string <- paste(dim_names, collapse="*")
      
      dimension_info[[var_name]] <- list(
        dimension_string = dim_string,
        dimension_names = dim_names,
        dimension_sizes = dim_sizes
      )
    }
  }
  
  results <- Map(function(f) {
    if(solution$VCNI[f] > 0) {
      dimensions <- solution$VCSN[solution$VCSP[f]:(solution$VCSP[f]+solution$VCNI[f]-1)]
      sizes <- c(solution$SSZ[dimensions], length(subtotals))
      labels <- c(
        Map(
          function(g) if(solution$SSZ[g]==0) c() else solution$STEL[solution$ELAD[g]:(solution$ELAD[g]+solution$SSZ[g]-1)], 
          dimensions
        ),
        list(subtotals)
      )
      names(labels) <- c(solution$STNM[dimensions], 'subtotal')
    } else {
      sizes <- c(length(subtotals))
      labels <- list(subtotals)
      names(labels) <- c('subtotal')
    }
    array(NA, dim = sizes, dimnames = labels)
  }, 1:length(solution$VCNM))
  
  names(results) <- solution$VCNM
  
  partials <- solution$OREX > 0 & solution$OREX != solution$VNCP
  stHeaders <- c('CUMS', unlist(Map(function(f) 
    sprintf('%sS',formatC(f, width=3, zero.print = TRUE, flag = "0")), 
    1:length(solution$STDS))))
  
  for (v in which(partials == FALSE & solution$PCUM>0)) {
    range <- solution$PCUM[v]:(solution$PCUM[v]+solution$ORND[v]-1)
    results[[solution$VARS[v]]][] <- unlist(Map(function(f) solution[[f]][range], stHeaders))
  }
  
  for (v in which(partials == FALSE & solution$PCUM==0)) {
    results[[solution$VARS[v]]][] <- 0
  }
  
  start <- 1
  for (v in which(partials == TRUE & solution$PCUM>0)) {
    range <- solution$PCUM[v]:(solution$PCUM[v]+solution$ORND[v]-1)
    positions <- solution$ORNL[start - 1 + (1:solution$ORND[v])]
    start <- solution$ORND[v]+start
    toFill <- rep(FALSE, solution$VNCP[v])
    toFill[positions] <- TRUE
    results[[solution$VARS[v]]][toFill] <- unlist(Map(function(f) solution[[f]][range], stHeaders))
  }
  
  enhanced_results <- list(
    data = results,
    dimension_info = dimension_info
  )
  
  class(enhanced_results) <- "enhanced_sl4"
  return(enhanced_results)
}