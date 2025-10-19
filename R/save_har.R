#' @title Save Data to GEMPACK HAR Format
#'
#' @description
#' Writes one or more data matrices or arrays into a GEMPACK-compatible
#' HAR file format, automatically generating associated 1C set headers.
#' 
#' @details
#' - Supports `1CFULL` (string sets) and `REFULL` (real arrays) headers  
#' - `2IFULL` (integer) and `RESPSE` (sparse) types are not implemented  
#' - Up to seven dimensions and ~2 million elements per chunk  
#' - No practical file-size limit or header count restriction
#' 
#' @param data_list A named list of data frames or arrays (names up to 4 characters).
#' @param file_path Path to the output HAR file.
#' @param dimensions A named list specifying dimension columns for each header.
#'    - HAR dimension names follow these column names; rename columns before saving to change them.
#' @param value_cols A named vector of value column names. Defaults to "Value".
#' @param long_desc A named vector of long descriptions (optional).
#' @param coefficients A named vector of coefficient names (optional).
#' @param export_sets Logical; if TRUE, exports dimension sets as 1C headers. Default is TRUE.
#' @param lowercase Logical; if TRUE, converts elements to lowercase. Default is TRUE.
#' @param dim_order Optional dimension ordering specification. Can be:
#'    - `NULL` (default): alphabetical A–Z ordering  
#'    - a data frame with columns matching dimension names  
#'    - a named list specifying order for each dimension  
#'    - a character string giving the path to an Excel or CSV file with order definitions
#' @param dim_rename Optional named list to rename dimensions in HAR output.
#'    - Names are original column names, values are desired HAR dimension names
#'    - Allows duplicate dimension names in HAR (e.g., COMMxREGxREG from COMMxSREGxDREG)
#'    - Example: list(RTMS = c(COMM = "COMM", SREG = "REG", DREG = "REG"))
#'
#' @return Invisibly returns a list containing export metadata, including file path and header summary.
#'
#' @author Pattawee Puangchit
#'
#' @seealso \code{\link{load_harx}}, \code{\link{load_sl4x}}
#'
#' @export
#'
#' @examples
#' # Example 1: Save a single matrix
#' har_path <- system.file("extdata", "TAR10-WEL.har", package = "HARplus")
#' har_data <- load_harx(har_path)
#' welfare_data <- get_data_by_var("A", har_data)
#' welfare_data_A <- welfare_data[["har_data"]][["A"]]
#'
#' save_har(
#'   data_list   = list(WELF = welfare_data),
#'   file_path   = file.path(tempdir(), "output_single.har"),
#'   dimensions  = list(WELF = c("REG", "COLUMN")),
#'   value_cols  = c(WELF = "Value"),
#'   long_desc   = c(WELF = "Welfare Decomposition"),
#'   export_sets = TRUE,
#'   lowercase   = FALSE
#' )
#'
#' # Example 2: Save multiple matrices
#' welfare_data <- get_data_by_var(c("A", "A1"), har_data)
#' welfare_data <- welfare_data[["har_data"]]
#'
#' save_har(
#'   data_list   = list(WELF = welfare_data[["A"]],
#'                      DECOM = welfare_data[["A1"]]),
#'   file_path   = file.path(tempdir(), "output_multi.har"),
#'   dimensions  = list(WELF = c("REG", "COLUMN"),
#'                      DECOM = c("REG", "ALLOCEFF")),
#'   value_cols  = list(WELF = "Value",
#'                      DECOM = "Value"),
#'   long_desc   = list(WELF = "Welfare Decomposition",
#'                      DECOM = "Allocative efficiency effect"),
#'   export_sets = TRUE,
#'   lowercase   = FALSE
#' )
#'
#' # Example 3: Apply mapping file for sorting
#' mapping <- list(
#'   REG    = c("RestofWorld", "MENA", "EU_28"),
#'   COLUMN = c("alloc_A1", "tot_E1")
#' )
#'
#' save_har(
#'   data_list   = list(
#'     WELF  = welfare_data[["A"]],
#'     DECOM = welfare_data[["A1"]]
#'   ),
#'   file_path   = file.path(tempdir(), "output_sorted.har"),
#'   dimensions  = list(
#'     WELF  = c("REG", "COLUMN"),
#'     DECOM = c("REG", "ALLOCEFF")
#'   ),
#'   value_cols  = list(
#'     WELF  = "Value",
#'     DECOM = "Value"
#'   ),
#'   long_desc   = list(
#'     WELF  = "Welfare Decomposition",
#'     DECOM = "Allocative efficiency effect"
#'   ),
#'   export_sets = TRUE,
#'   dim_order   = mapping,
#'   lowercase   = FALSE
#' )
#'
#' # Example 4: Rename dimensions with duplicate names (COMMxREGxREG)
#' tax_data <- welfare_data[["C17"]]
#' names(tax_data)[names(tax_data) == "REG"] <- "SREG"
#' tax_data$DREG <- tax_data$SREG
#' 
#' mapping <- list(
#'   COMM = c("Wheat", "Rice", "Corn"),
#'   REG  = c("USA", "EU", "CHN")
#' )
#'
#' save_har(
#'   data_list   = list(RTMS = tax_data),
#'   file_path   = file.path(tempdir(), "output_renamed.har"),
#'   dimensions  = list(RTMS = c("COLUMN", "SREG", "DREG")),
#'   value_cols  = list(RTMS = "Value"),
#'   long_desc   = list(RTMS = "Tariff rates"),
#'   dim_rename  = list(RTMS = c(COLUMN = "COMM", SREG = "REG", DREG = "REG")),
#'   export_sets = TRUE,
#'   dim_order   = mapping,
#'   lowercase   = FALSE
#' )
#' 
#' # Example 5: Exporting HAR file using custom dimension mapping
#'
#' har_path <- system.file("extdata", "TAR10-WEL.har", package = "HARplus")
#' har_data <- load_harx(har_path)
#' welfare_data <- get_data_by_var("C17", har_data)
#' welfare_data <- welfare_data[["har_data"]][["C17"]] 
#'
#' mapping <- list(
#'   COMM = c("TransComm", "MeatLstk"),
#'   REG  = c("SSA", "RestofWorld", "SEAsia")  
#' )
#'
#' # Export HAR
#' save_har(
#'   data_list   = list(RTMS = tax),
#'   file_path   = file.path(tempdir(), "output_single.har"),
#'   dimensions  = list(RTMS = c("COMM", "SREG", "DREG")),
#'   value_cols  = list(RTMS = "Value"),
#'   long_desc   = list(RTMS = "Shock RTMS"),
#'   dim_rename  = list(RTMS = c(COMM = "COMM", SREG = "REG", DREG = "REG")),
#'   export_sets = TRUE,
#'   dim_order   = mapping,
#'   lowercase   = FALSE
#' )
save_har <- function(data_list,
                     file_path,
                     dimensions,
                     value_cols = NULL,
                     long_desc = NULL,
                     coefficients = NULL,
                     export_sets = TRUE,
                     lowercase = TRUE,
                     dim_order = NULL,
                     dim_rename = NULL) {
  
  header_names <- toupper(names(data_list))
  names(data_list) <- header_names
  
  if (is.null(value_cols)) {
    value_cols <- setNames(rep("Value", length(data_list)), header_names)
  }
  if (is.null(long_desc)) {
    long_desc <- setNames(header_names, header_names)
  }
  if (is.null(coefficients)) {
    coefficients <- setNames(header_names, header_names)
  }
  
  dim_order_map <- process_dim_order(dim_order, lowercase)
  
  all_arrays <- lapply(header_names, function(hdr) {
    obj <- data_list[[hdr]]
    if (is.data.frame(obj)) {
      convert_df_to_array(obj, dimensions[[hdr]], value_cols[[hdr]], lowercase)
    } else {
      obj
    }
  })
  names(all_arrays) <- header_names
  
  if (!is.null(dim_rename)) {
    all_arrays <- lapply(header_names, function(hdr) {
      arr <- all_arrays[[hdr]]
      if (hdr %in% names(dim_rename)) {
        rename_map <- dim_rename[[hdr]]
        arr <- rename_array_dims(arr, rename_map)
      }
      arr
    })
    names(all_arrays) <- header_names
  }
  
  unique_sets <- if (export_sets) extract_unique_sets(all_arrays) else NULL
  
  if (!is.null(unique_sets) && !is.null(dim_order_map)) {
    unique_sets <- lapply(names(unique_sets), function(set_name) {
      if (set_name %in% names(dim_order_map)) {
        current_vals <- unique_sets[[set_name]]
        desired_order <- dim_order_map[[set_name]]
        common_vals <- intersect(desired_order, current_vals)
        extra_vals <- setdiff(current_vals, desired_order)
        if (length(common_vals) > 0) {
          c(common_vals, sort(extra_vals))
        } else {
          sort(current_vals)
        }
      } else {
        sort(unique_sets[[set_name]])
      }
    })
    names(unique_sets) <- names(extract_unique_sets(all_arrays))
  }
  
  if (!is.null(unique_sets)) {
    all_arrays <- lapply(all_arrays, function(arr) {
      reorder_array_by_sets(arr, unique_sets)
    })
  }
  
  con <- file(file_path, 'wb')
  on.exit(close(con))
  
  if (!is.null(unique_sets)) {
    for (nm in names(unique_sets)) {
      write_string(con, nm, unique_sets[[nm]])
    }
  }
  
  for (hdr in header_names) {
    write_matrix(con, hdr, all_arrays[[hdr]], 
                 long_desc[[hdr]], coefficients[[hdr]])
  }
  
  close(con)
  on.exit()
  
  n_total <- length(unique_sets) + length(header_names)
  n_sets <- length(unique_sets)
  n_data <- length(header_names)
  
  cat(sprintf("\nSuccessfully wrote %d header(s) to HAR file\n", n_total))
  cat(sprintf("  Set headers (1C type): %d\n", n_sets))
  cat(sprintf("  Data headers (RE type): %d\n", n_data))
  
  if (!is.null(dim_order_map) && length(dim_order_map) > 0) {
    cat("\nDimension ordering applied:\n")
    for (dm in names(dim_order_map)) {
      n_mapped <- length(dim_order_map[[dm]])
      cat(sprintf("  %s: %d prioritized values, remaining A-Z\n", dm, n_mapped))
    }
    
    all_dim_names <- unique(unlist(lapply(all_arrays, function(arr) {
      toupper(substr(names(dimnames(arr)), 1, 4))
    })))
    unsorted_dims <- setdiff(all_dim_names, names(dim_order_map))
    if (length(unsorted_dims) > 0) {
      cat("\nDimensions with A-Z sorting (no custom mapping):\n")
      for (dm in unsorted_dims) {
        cat(sprintf("  %s\n", dm))
      }
    }
  } else {
    cat("\nAll dimensions sorted A-Z (no custom mapping provided)\n")
  }
  
  cat(sprintf("\nOutput file: %s\n", normalizePath(file_path)))
  cat(sprintf("File size: %s bytes\n\n", format(file.info(file_path)$size, big.mark = ",")))
  
  invisible(list(
    file_path = normalizePath(file_path),
    headers_written = c(names(unique_sets), header_names),
    n_sets = n_sets,
    n_data = n_data
  ))
}


#' @keywords internal
#' @noRd
#' @author Pattawee Puangchit
#' @importFrom stats complete.cases
convert_df_to_array <- function(df, dim_cols, val_col, lowercase) {
  df <- df[complete.cases(df[, c(dim_cols, val_col)]), ]
  
  dim_sets <- lapply(dim_cols, function(col) {
    vals <- sort(unique(as.character(df[[col]])))
    if (lowercase) tolower(vals) else vals
  })
  names(dim_sets) <- if (lowercase) tolower(dim_cols) else dim_cols
  
  arr <- array(0, dim = vapply(dim_sets, length, integer(1)), dimnames = dim_sets)
  
  for (i in seq_len(nrow(df))) {
    idx <- vapply(seq_along(dim_cols), function(j) {
      val <- as.character(df[[dim_cols[j]]][i])
      if (lowercase) val <- tolower(val)
      match(val, dim_sets[[j]])
    }, integer(1))
    arr[matrix(idx, nrow = 1)] <- as.numeric(df[[val_col]][i])
  }
  arr
}

#' @keywords internal
#' @noRd
#' @author Pattawee Puangchit
rename_array_dims <- function(arr, rename_map) {
  if (is.null(dimnames(arr)) || is.null(names(dimnames(arr)))) {
    return(arr)
  }
  
  old_names <- names(dimnames(arr))
  new_names <- old_names
  
  for (i in seq_along(old_names)) {
    if (old_names[i] %in% names(rename_map)) {
      new_names[i] <- rename_map[[old_names[i]]]
    }
  }
  
  names(dimnames(arr)) <- new_names
  return(arr)
}

#' @keywords internal
#' @noRd
#' @author Pattawee Puangchit
reorder_array_by_sets <- function(arr, set_list) {
  
  if (is.null(dimnames(arr)) || is.null(set_list)) {
    return(arr)
  }
  
  dim_names <- names(dimnames(arr))
  
  for (i in seq_along(dim_names)) {
    dim_key <- toupper(substr(dim_names[i], 1, 4))
    
    if (dim_key %in% names(set_list)) {
      current_vals <- dimnames(arr)[[i]]
      desired_order <- set_list[[dim_key]]
      
      common_vals <- intersect(desired_order, current_vals)
      
      if (length(common_vals) > 0 && !identical(current_vals, common_vals)) {
        idx_list <- vector("list", length(dim(arr)))
        for (j in seq_along(idx_list)) {
          if (j == i) {
            idx_list[[j]] <- match(common_vals, current_vals)
          } else {
            idx_list[[j]] <- seq_len(dim(arr)[j])
          }
        }
        arr <- do.call(`[`, c(list(arr), idx_list, drop = FALSE))
      }
    }
  }
  
  arr
}

#' @keywords internal
#' @noRd
#' @author Pattawee Puangchit
extract_unique_sets <- function(arrays) {
  all_sets <- list()
  for (arr in arrays) {
    dn <- names(dimnames(arr))
    for (i in seq_along(dn)) {
      nm <- dn[i]
      hdr <- toupper(substr(nm, 1, 4))
      
      if (hdr %in% names(all_sets)) {
        all_sets[[hdr]] <- unique(c(all_sets[[hdr]], dimnames(arr)[[i]]))
      } else {
        all_sets[[hdr]] <- dimnames(arr)[[i]]
      }
    }
  }
  
  lapply(all_sets, sort)
}

#' @keywords internal
#' @importFrom utils read.csv
#' @noRd
#' @author Pattawee Puangchit
process_dim_order <- function(dim_order, lowercase) {
  
  if (is.null(dim_order)) {
    return(NULL)
  }
  
  if (is.character(dim_order) && length(dim_order) == 1) {
    if (grepl("\\.xlsx?$", dim_order, ignore.case = TRUE)) {
      if (!requireNamespace("openxlsx", quietly = TRUE)) {
        stop("Package 'openxlsx' needed for Excel files. Install with: install.packages('openxlsx')")
      }
      dim_order <- openxlsx::read.xlsx(dim_order, sheet = 1)
    } else if (grepl("\\.csv$", dim_order, ignore.case = TRUE)) {
      dim_order <- read.csv(dim_order, stringsAsFactors = FALSE)
    } else {
      stop("File must be .xlsx, .xls, or .csv")
    }
  }
  
  if (is.data.frame(dim_order)) {
    order_map <- lapply(names(dim_order), function(col) {
      vals <- dim_order[[col]]
      vals <- vals[!is.na(vals) & nchar(trimws(as.character(vals))) > 0]
      vals <- unique(as.character(vals))
      if (lowercase) tolower(vals) else vals
    })
    names(order_map) <- toupper(substr(names(dim_order), 1, 4))
    order_map <- order_map[sapply(order_map, length) > 0]
    return(order_map)
  }
  
  if (is.list(dim_order)) {
    order_map <- lapply(names(dim_order), function(nm) {
      vals <- dim_order[[nm]]
      vals <- vals[!is.na(vals) & nchar(trimws(as.character(vals))) > 0]
      vals <- unique(as.character(vals))
      if (lowercase) tolower(vals) else vals
    })
    names(order_map) <- toupper(substr(names(dim_order), 1, 4))
    order_map <- order_map[sapply(order_map, length) > 0]
    return(order_map)
  }
  
  stop("dim_order must be NULL, data frame, list, or file path")
}

#' @keywords internal
#' @noRd
#' @author Pattawee Puangchit
reorder_array_dims <- function(arr, order_map) {
  
  if (is.null(order_map) || is.null(dimnames(arr))) {
    dn <- names(dimnames(arr))
    for (i in seq_along(dn)) {
      current_vals <- dimnames(arr)[[i]]
      sorted_vals <- sort(current_vals)
      
      if (!identical(current_vals, sorted_vals)) {
        idx_list <- vector("list", length(dim(arr)))
        for (j in seq_along(idx_list)) {
          if (j == i) {
            idx_list[[j]] <- match(sorted_vals, current_vals)
          } else {
            idx_list[[j]] <- seq_len(dim(arr)[j])
          }
        }
        arr <- do.call(`[`, c(list(arr), idx_list, drop = FALSE))
      }
    }
    return(arr)
  }
  
  dim_names <- names(dimnames(arr))
  
  for (i in seq_along(dim_names)) {
    dim_key <- toupper(substr(dim_names[i], 1, 4))
    
    if (dim_key %in% names(order_map)) {
      current_vals <- dimnames(arr)[[i]]
      desired_order <- order_map[[dim_key]]
      
      common_vals <- intersect(desired_order, current_vals)
      extra_vals <- setdiff(current_vals, desired_order)
      
      if (length(common_vals) > 0) {
        new_order <- c(common_vals, sort(extra_vals))
      } else {
        new_order <- sort(current_vals)
      }
      
      if (!identical(current_vals, new_order)) {
        idx_list <- vector("list", length(dim(arr)))
        for (j in seq_along(idx_list)) {
          if (j == i) {
            idx_list[[j]] <- match(new_order, current_vals)
          } else {
            idx_list[[j]] <- seq_len(dim(arr)[j])
          }
        }
        
        arr <- do.call(`[`, c(list(arr), idx_list, drop = FALSE))
      }
    } else {
      current_vals <- dimnames(arr)[[i]]
      sorted_vals <- sort(current_vals)
      
      if (!identical(current_vals, sorted_vals)) {
        idx_list <- vector("list", length(dim(arr)))
        for (j in seq_along(idx_list)) {
          if (j == i) {
            idx_list[[j]] <- match(sorted_vals, current_vals)
          } else {
            idx_list[[j]] <- seq_len(dim(arr)[j])
          }
        }
        arr <- do.call(`[`, c(list(arr), idx_list, drop = FALSE))
      }
    }
  }
  
  arr
}

#' @keywords internal
#' @noRd
#' @author Pattawee Puangchit
write_string <- function(con, hdr_name, elements) {
  
  hdr_name <- substr(paste0(hdr_name, "    "), 1, 4)
  desc <- substr(paste0(hdr_name, strrep(" ", 70)), 1, 70)
  
  max_len <- max(12, max(nchar(elements)))
  dimensions <- c(length(elements), max_len)
  
  rec1 <- charToRaw(hdr_name)
  writeBin(as.integer(4), con, size = 4)
  writeBin(rec1, con)
  writeBin(as.integer(4), con, size = 4)
  
  rec2 <- c(
    charToRaw("    "),
    charToRaw("1CFULL"),
    charToRaw(desc),
    writeBin(as.integer(2), raw(), size = 4),
    writeBin(as.integer(dimensions), raw(), size = 4)
  )
  writeBin(as.integer(length(rec2)), con, size = 4)
  writeBin(rec2, con)
  writeBin(as.integer(length(rec2)), con, size = 4)
  
  padded_elements <- vapply(elements, function(e) {
    e <- substr(e, 1, max_len)
    paste0(e, strrep(" ", max_len - nchar(e)))
  }, character(1), USE.NAMES = FALSE)
  
  contents <- charToRaw(paste0(padded_elements, collapse = ""))
  
  rec3 <- c(
    charToRaw("    "),
    writeBin(as.integer(1), raw(), size = 4),
    writeBin(as.integer(length(elements)), raw(), size = 4),
    writeBin(as.integer(length(elements)), raw(), size = 4),
    contents
  )
  writeBin(as.integer(length(rec3)), con, size = 4)
  writeBin(rec3, con)
  writeBin(as.integer(length(rec3)), con, size = 4)
}

#' @keywords internal
#' @noRd
#' @author Pattawee Puangchit
write_matrix <- function(con, hdr_name, arr, description, coefficient) {
  
  hdr_name <- substr(paste0(hdr_name, "    "), 1, 4)
  description <- substr(paste0(description, strrep(" ", 70)), 1, 70)
  coefficient <- substr(paste0(coefficient, strrep(" ", 12)), 1, 12)
  
  dimensions <- dim(arr)
  if (is.null(dimensions)) dimensions <- c(1)
  used_dimensions <- length(dimensions)
  dimensions <- c(dimensions, rep(1, 7 - length(dimensions)))
  
  rec1 <- charToRaw(hdr_name)
  writeBin(as.integer(4), con, size = 4)
  writeBin(rec1, con)
  writeBin(as.integer(4), con, size = 4)
  
  rec2 <- c(
    charToRaw("    "),
    charToRaw("REFULL"),
    charToRaw(description),
    writeBin(as.integer(7), raw(), size = 4),
    writeBin(as.integer(dimensions), raw(), size = 4)
  )
  writeBin(as.integer(length(rec2)), con, size = 4)
  writeBin(rec2, con)
  writeBin(as.integer(length(rec2)), con, size = 4)
  
  if (!is.null(dimnames(arr)) && length(names(dimnames(arr))) > 0) {
    dim_names <- names(dimnames(arr))
    
    set_names_raw <- unlist(lapply(dim_names, function(nm) {
      nm <- toupper(substr(nm, 1, 4))
      nm <- substr(paste0(nm, strrep(" ", 12)), 1, 12)
      charToRaw(nm)
    }))
    
    set_names <- c(
      set_names_raw,
      as.raw(rep(0x6b, length(dim_names))),
      as.raw(rep(0x00, 4 + 4 * length(dim_names)))
    )
    
    unique_dim_names <- unique(dim_names)
    defined_dims <- length(unique_dim_names)
    used_dims <- length(dim_names)
  } else {
    set_names <- as.raw(c(0x00, 0x00, 0x00, 0x00))
    defined_dims <- 0
    used_dims <- 0
  }
  
  rec3 <- c(
    charToRaw("    "),
    writeBin(as.integer(defined_dims), raw(), size = 4),
    as.raw(c(0xff, 0xff, 0xff, 0xff)),
    writeBin(as.integer(used_dims), raw(), size = 4),
    charToRaw(coefficient),
    as.raw(c(0xff, 0xff, 0xff, 0xff)),
    set_names
  )
  writeBin(as.integer(length(rec3)), con, size = 4)
  writeBin(rec3, con)
  writeBin(as.integer(length(rec3)), con, size = 4)
  
  if (!is.null(dimnames(arr)) && length(dimnames(arr)) > 0) {
    unique_dim_names <- unique(names(dimnames(arr)))
    
    for (ud in unique_dim_names) {
      matching_indices <- which(names(dimnames(arr)) == ud)
      ele <- dimnames(arr)[[matching_indices[1]]]
      
      padded_ele <- vapply(ele, function(e) {
        e <- substr(e, 1, 12)
        paste0(e, strrep(" ", 12 - nchar(e)))
      }, character(1), USE.NAMES = FALSE)
      
      element_data <- unlist(lapply(padded_ele, charToRaw))
      
      ud_upper <- toupper(substr(ud, 1, 4))
      ud_padded <- substr(paste0(ud_upper, strrep(" ", 12)), 1, 12)
      
      rec_ele <- c(
        charToRaw("    "),
        writeBin(as.integer(1), raw(), size = 4),
        writeBin(as.integer(length(ele)), raw(), size = 4),
        writeBin(as.integer(length(ele)), raw(), size = 4),
        element_data
      )
      writeBin(as.integer(length(rec_ele)), con, size = 4)
      writeBin(rec_ele, con)
      writeBin(as.integer(length(rec_ele)), con, size = 4)
    }
  }
  
  total_elements <- prod(dim(arr)[1:used_dimensions])
  max_chunk <- 2e6
  
  if (total_elements <= max_chunk) {
    slice_size <- total_elements
    num_data_records <- 1
  } else {
    slice_size <- max_chunk
    num_data_records <- ceiling(total_elements / slice_size)
  }
  
  rec_frame <- c(
    charToRaw("    "),
    writeBin(as.integer(1 + num_data_records * 2), raw(), size = 4),
    writeBin(as.integer(7), raw(), size = 4),
    writeBin(as.integer(dimensions), raw(), size = 4)
  )
  writeBin(as.integer(length(rec_frame)), con, size = 4)
  writeBin(rec_frame, con)
  writeBin(as.integer(length(rec_frame)), con, size = 4)
  
  arr_vector <- as.numeric(as.vector(arr))
  orig_dims <- dim(arr)[1:used_dimensions]
  
  for (dr in seq_len(num_data_records)) {
    from_element <- (dr - 1) * slice_size + 1
    to_element <- min(dr * slice_size, total_elements)
    
    from_to_indices <- arrayInd(c(from_element, to_element), .dim = orig_dims)
    
    if (nrow(from_to_indices) == 1) {
      from_to_indices <- rbind(from_to_indices, from_to_indices)
    }
    
    from_to_vector <- c(
      as.vector(from_to_indices), 
      rep(1, 7 - used_dimensions), 
      rep(1, 7 - used_dimensions)
    )
    
    rec_bounds <- c(
      charToRaw("    "),
      writeBin(as.integer(num_data_records * 2 - dr * 2 + 2), raw(), size = 4),
      writeBin(as.integer(from_to_vector), raw(), size = 4)
    )
    writeBin(as.integer(length(rec_bounds)), con, size = 4)
    writeBin(rec_bounds, con)
    writeBin(as.integer(length(rec_bounds)), con, size = 4)
    
    data_chunk <- arr_vector[from_element:to_element]
    
    rec_data <- c(
      charToRaw("    "),
      writeBin(as.integer(num_data_records * 2 - dr * 2 + 1), raw(), size = 4),
      writeBin(data_chunk, raw(), size = 4)
    )
    writeBin(as.integer(length(rec_data)), con, size = 4)
    writeBin(rec_data, con)
    writeBin(as.integer(length(rec_data)), con, size = 4)
  }
}