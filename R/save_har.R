#' @title Save Data to GEMPACK HAR Format
#'
#' @description
#' Writes a named list of R objects to a GEMPACK-compatible HAR file, including
#' character sets, mapping vectors, integer matrices, numeric arrays, sparse
#' numeric arrays, and data frames reshaped to arrays.
#'
#' @param data_list Named list of objects to write. List names are used as HAR
#'   header names after conversion to uppercase and truncation to four characters.
#' @param file_path Character string giving the output HAR file path.
#' @param dimensions Optional named list. For data-frame inputs, each element
#'   gives the columns used as array dimensions.
#' @param value_cols Optional named list or named character vector. For
#'   data-frame inputs, each element gives the numeric value column. Defaults to
#'   \code{"Value"} when omitted.
#' @param header_type Optional named list or named character vector giving
#'   explicit header roles. Accepted values are \code{"auto"}, \code{"set"},
#'   \code{"mapping"}, \code{"real"}, \code{"sparse"}, and \code{"integer"}.
#' @param mappings Optional named list. Each element must be
#'   \code{c(source_set, destination_set)} for the corresponding mapping header.
#' @param long_desc Optional named list or named character vector of long header
#'   descriptions.
#' @param coefficients Optional named list or named character vector of
#'   coefficient names for numeric headers.
#' @param export_sets Logical. If \code{TRUE}, dimension sets from numeric
#'   arrays are written as character headers unless already supplied. Default is
#'   \code{TRUE}.
#' @param lowercase Logical. If \code{TRUE}, character elements and dimension
#'   values are converted to lowercase during data-frame and ordering
#'   processing. Default is \code{TRUE}.
#' @param dim_order Optional dimension-ordering specification. Can be
#'   \code{NULL}, a data frame, a named list, or a path to a CSV or Excel file.
#' @param dim_rename Optional named list for renaming array dimensions in the
#'   HAR output.
#' @param force_sparse Optional character vector of headers to write in sparse
#'   numeric format.
#' @param max_chunk Integer. Maximum number of elements per dense numeric data
#'   chunk. Default is \code{2e6}.
#'
#' @return Invisibly returns a list containing the output path, written headers,
#'   and counts of set, data, and mapping headers.
#'
#' @author Pattawee Puangchit
#'
#' @seealso \code{\link{load_harx}}, \code{\link{load_sl4x}}
#'
#' @export
#'
#' @examples
#' # Example 1: Save one numeric data frame
#' REG <- c("USA", "EU", "ROW")
#' COLUMN <- c("alloc_A1", "tot_E1")
#' WELF <- expand.grid(REG = REG, COLUMN = COLUMN, stringsAsFactors = FALSE)
#' WELF$Value <- seq_len(nrow(WELF))
#'
#' save_har(
#'   data_list = list(WELF = WELF),
#'   file_path = file.path(tempdir(), "output_single.har"),
#'   dimensions = list(WELF = c("REG", "COLUMN")),
#'   value_cols = list(WELF = "Value"),
#'   long_desc = list(WELF = "Welfare Decomposition"),
#'   coefficients = list(WELF = "WELF"),
#'   export_sets = TRUE,
#'   lowercase = FALSE
#' )
#'
#' # Example 2: Save multiple numeric data frames
#' DECOM <- expand.grid(REG = REG, ALLOCEFF = c("A1", "A2"), stringsAsFactors = FALSE)
#' DECOM$Value <- seq_len(nrow(DECOM))
#'
#' save_har(
#'   data_list = list(WELF = WELF, DECOM = DECOM),
#'   file_path = file.path(tempdir(), "output_multi.har"),
#'   dimensions = list(
#'     WELF = c("REG", "COLUMN"),
#'     DECOM = c("REG", "ALLOCEFF")
#'   ),
#'   value_cols = list(
#'     WELF = "Value",
#'     DECOM = "Value"
#'   ),
#'   long_desc = list(
#'     WELF = "Welfare Decomposition",
#'     DECOM = "Allocative efficiency effect"
#'   ),
#'   coefficients = list(
#'     WELF = "WELF",
#'     DECOM = "DECOM"
#'   ),
#'   export_sets = TRUE,
#'   lowercase = FALSE
#' )
#'
#' # Example 3: Save a mapping vector
#' SC <- c("AF", "AP", "BA")
#' GSEC <- c("OCR", "V_F", "GRO")
#' MASC <- c(AF = "OCR", AP = "V_F", BA = "GRO")
#'
#' save_har(
#'   data_list = list(SC = SC, GSEC = GSEC, MASC = MASC),
#'   file_path = file.path(tempdir(), "mapping.har"),
#'   mappings = list(MASC = c("SC", "GSEC")),
#'   long_desc = list(
#'     SC = "Set SC",
#'     GSEC = "Set GSEC",
#'     MASC = "Mapping SC to GSEC"
#'   ),
#'   export_sets = FALSE,
#'   lowercase = FALSE
#' )
#'
#' # Example 4: Save mixed headers
#' CODE <- matrix(as.integer(c(1, 2, 3, 4)), nrow = 2)
#' TAX <- expand.grid(SC = SC, REG = REG, stringsAsFactors = FALSE)
#' TAX$Value <- c(0, 0, 0, 1.2, 0, 0, 0, 0, 2.5)
#'
#' save_har(
#'   data_list = list(
#'     WELF = WELF,
#'     REG = REG,
#'     SC = SC,
#'     GSEC = GSEC,
#'     MASC = MASC,
#'     TAX = TAX,
#'     CODE = CODE
#'   ),
#'   file_path = file.path(tempdir(), "output_mixed.har"),
#'   dimensions = list(
#'     WELF = c("REG", "COLUMN"),
#'     TAX = c("SC", "REG")
#'   ),
#'   value_cols = list(
#'     WELF = "Value",
#'     TAX = "Value"
#'   ),
#'   mappings = list(
#'     MASC = c("SC", "GSEC")
#'   ),
#'   long_desc = list(
#'     WELF = "Welfare Decomposition",
#'     REG = "Set REG",
#'     SC = "Set SC",
#'     GSEC = "Set GSEC",
#'     MASC = "Mapping SC to GSEC",
#'     TAX = "Sparse tax example",
#'     CODE = "Integer matrix example"
#'   ),
#'   coefficients = list(
#'     WELF = "WELF",
#'     TAX = "TAX"
#'   ),
#'   force_sparse = "TAX",
#'   export_sets = FALSE,
#'   lowercase = FALSE
#' )
#'
#' # Example 5: Apply custom dimension ordering
#' dim_order <- list(
#'   REG = c("ROW", "USA", "EU"),
#'   COLUMN = c("tot_E1", "alloc_A1")
#' )
#'
#' save_har(
#'   data_list = list(WELF = WELF),
#'   file_path = file.path(tempdir(), "output_sorted.har"),
#'   dimensions = list(WELF = c("REG", "COLUMN")),
#'   value_cols = list(WELF = "Value"),
#'   long_desc = list(WELF = "Welfare Decomposition"),
#'   coefficients = list(WELF = "WELF"),
#'   export_sets = TRUE,
#'   dim_order = dim_order,
#'   lowercase = FALSE
#' )
save_har <- function(data_list,
                     file_path,
                     dimensions = NULL,
                     value_cols = NULL,
                     header_type = NULL,
                     mappings = NULL,
                     long_desc = NULL,
                     coefficients = NULL,
                     export_sets = TRUE,
                     lowercase = TRUE,
                     dim_order = NULL,
                     dim_rename = NULL,
                     force_sparse = NULL,
                     max_chunk = 2e6) {
  if (!is.list(data_list) || is.null(names(data_list)) || any(names(data_list) == "")) {
    stop("data_list must be a named list")
  }
  original_headers <- names(data_list)
  header_names <- normalize_header_names(original_headers)
  if (any(duplicated(header_names))) {
    dup <- unique(header_names[duplicated(header_names)])
    stop("Duplicated HAR header names after four-character truncation: ", paste(dup, collapse = ", "))
  }
  names(data_list) <- header_names
  dimensions <- normalize_named_spec(dimensions)
  value_cols <- normalize_named_spec(value_cols)
  header_type <- normalize_named_spec(header_type)
  mappings <- normalize_named_spec(mappings)
  long_desc <- normalize_named_spec(long_desc)
  coefficients <- normalize_named_spec(coefficients)
  dim_rename <- normalize_named_spec(dim_rename)
  force_sparse <- normalize_header_names(force_sparse)
  validate_mapping_specs(mappings, data_list)
  header_types <- vapply(header_names, function(hdr) {
    detect_header_type(hdr, data_list[[hdr]], header_type, mappings)
  }, character(1), USE.NAMES = TRUE)
  char_headers <- header_names[header_types %in% c("set", "mapping")]
  int_headers <- header_names[header_types == "integer"]
  re_headers <- header_names[header_types %in% c("real", "sparse")]
  unknown_headers <- header_names[header_types == "unknown"]
  if (length(unknown_headers) > 0) {
    warning(sprintf("Skipping unsupported data types: %s", paste(unknown_headers, collapse = ", ")))
  }
  for (hdr in re_headers) {
    if (is.data.frame(data_list[[hdr]]) && is.null(value_cols[[hdr]])) value_cols[[hdr]] <- "Value"
  }
  dim_order_map <- process_dim_order(dim_order, lowercase)
  all_arrays <- list()
  for (hdr in re_headers) {
    obj <- data_list[[hdr]]
    if (is.data.frame(obj)) {
      if (is.null(dimensions[[hdr]])) stop(sprintf("Header '%s' is a data frame but dimensions[['%s']] is missing", hdr, hdr))
      all_arrays[[hdr]] <- convert_df_to_array(obj, dimensions[[hdr]], value_cols[[hdr]], lowercase)
    } else {
      all_arrays[[hdr]] <- obj
    }
  }
  if (!is.null(dim_rename)) {
    for (hdr in names(dim_rename)) {
      if (hdr %in% names(all_arrays)) all_arrays[[hdr]] <- rename_array_dims(all_arrays[[hdr]], dim_rename[[hdr]])
    }
  }
  unique_sets <- if (export_sets && length(all_arrays) > 0) extract_unique_sets(all_arrays) else NULL
  explicit_sets <- char_headers[header_types[char_headers] == "set"]
  if (!is.null(unique_sets)) unique_sets <- unique_sets[!(names(unique_sets) %in% explicit_sets)]
  if (!is.null(unique_sets) && !is.null(dim_order_map)) {
    for (set_name in names(unique_sets)) {
      if (set_name %in% names(dim_order_map)) {
        current_vals <- unique_sets[[set_name]]
        desired_order <- dim_order_map[[set_name]]
        common_vals <- intersect(desired_order, current_vals)
        extra_vals <- setdiff(current_vals, desired_order)
        unique_sets[[set_name]] <- if (length(common_vals) > 0) c(common_vals, sort(extra_vals)) else sort(current_vals)
      } else {
        unique_sets[[set_name]] <- sort(unique_sets[[set_name]])
      }
    }
  }
  if (!is.null(unique_sets)) all_arrays <- lapply(all_arrays, reorder_array_by_sets, set_list = unique_sets)
  out_dir <- dirname(file_path)
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  con <- file(file_path, "wb")
  on.exit(close(con), add = TRUE)
  n_sets <- 0
  n_mappings <- 0
  n_data <- 0
  written_headers <- character(0)
  for (hdr in char_headers) {
    obj <- data_list[[hdr]]
    if (header_types[[hdr]] == "set") {
      if (!(hdr %in% written_headers)) {
        write_string(con, hdr, obj, description = get_long_desc(hdr, long_desc, paste0("Set ", hdr)))
        n_sets <- n_sets + 1
        written_headers <- c(written_headers, hdr)
      }
    } else {
      map_result <- prepare_mapping_values(hdr, obj, mappings[[hdr]], data_list)
      from_set <- map_result$from_set
      to_set <- map_result$to_set
      if (!(from_set %in% written_headers)) {
        write_string(con, from_set, map_result$from_elements, description = get_long_desc(from_set, long_desc, paste0("Set ", from_set)))
        n_sets <- n_sets + 1
        written_headers <- c(written_headers, from_set)
      }
      if (!(to_set %in% written_headers)) {
        write_string(con, to_set, map_result$to_elements, description = get_long_desc(to_set, long_desc, paste0("Set ", to_set)))
        n_sets <- n_sets + 1
        written_headers <- c(written_headers, to_set)
      }
      default_desc <- sprintf("Mapping %s2%s from %s(%d) to %s(%d)", from_set, to_set, from_set, length(map_result$from_elements), to_set, length(map_result$to_elements))
      write_string(con, hdr, map_result$values, description = get_long_desc(hdr, long_desc, default_desc))
      n_mappings <- n_mappings + 1
      written_headers <- c(written_headers, hdr)
    }
  }
  if (!is.null(unique_sets)) {
    for (nm in names(unique_sets)) {
      if (!(nm %in% written_headers)) {
        write_string(con, nm, unique_sets[[nm]], description = get_long_desc(nm, long_desc, paste0("Set ", nm)))
        n_sets <- n_sets + 1
        written_headers <- c(written_headers, nm)
      }
    }
  }
  for (hdr in int_headers) {
    write_2ifull(con, hdr, data_list[[hdr]], description = get_long_desc(hdr, long_desc, hdr))
    n_data <- n_data + 1
    written_headers <- c(written_headers, hdr)
  }
  for (hdr in re_headers) {
    arr <- all_arrays[[hdr]]
    desc <- get_long_desc(hdr, long_desc, hdr)
    coef <- get_long_desc(hdr, coefficients, hdr)
    use_sparse <- header_types[[hdr]] == "sparse" || (!is.null(force_sparse) && hdr %in% force_sparse)
    if (!use_sparse && is.numeric(arr) && length(arr) > 0 && sum(arr == 0) / length(arr) > 0.5) {
      use_sparse <- TRUE
      message(sprintf("Doing sparse: %s", hdr))
    }
    if (use_sparse) write_sparse(con, hdr, arr, description = desc, coefficient = coef, maxSize = max_chunk)
    else write_matrix(con, hdr, arr, description = desc, coefficient = coef, maxSize = max_chunk)
    n_data <- n_data + 1
    written_headers <- c(written_headers, hdr)
  }
  close(con)
  on.exit(NULL, add = FALSE)
  n_total <- n_sets + n_data + n_mappings
  cat(sprintf("\nSuccessfully wrote %d header(s) to HAR file\n", n_total))
  cat(sprintf("  Set headers (1C type): %d\n", n_sets))
  cat(sprintf("  Mapping headers (1C type): %d\n", n_mappings))
  cat(sprintf("  Data headers (RE/2I type): %d\n", n_data))
  if (!is.null(dim_order_map) && length(dim_order_map) > 0) {
    cat("\nDimension ordering applied:\n")
    for (dm in names(dim_order_map)) cat(sprintf("  %s: %d prioritized values, remaining A-Z\n", dm, length(dim_order_map[[dm]])))
  } else if (length(all_arrays) > 0) {
    cat("\nAll dimensions sorted A-Z (no custom mapping provided)\n")
  }
  cat(sprintf("\nOutput file: %s\n", normalizePath(file_path)))
  cat(sprintf("File size: %s bytes\n\n", format(file.info(file_path)$size, big.mark = ",")))
  invisible(list(file_path = normalizePath(file_path), headers_written = written_headers, n_sets = n_sets, n_data = n_data, n_mappings = n_mappings))
}

#' @keywords internal
#' @noRd
normalize_header_names <- function(x) {
  if (is.null(x)) return(NULL)
  toupper(substr(as.character(x), 1, 4))
}

#' @keywords internal
#' @noRd
normalize_named_spec <- function(x) {
  if (is.null(x)) return(NULL)
  if (is.atomic(x) && !is.null(names(x))) x <- as.list(x)
  if (!is.list(x)) stop("Named specification arguments must be named lists or named vectors")
  if (is.null(names(x)) || any(names(x) == "")) stop("Named specification arguments must have names")
  names(x) <- normalize_header_names(names(x))
  x
}

#' @keywords internal
#' @noRd
normalize_header_type_value <- function(x) {
  if (is.null(x)) return(NULL)
  z <- tolower(as.character(x)[1])
  if (z %in% c("auto", "")) return(NULL)
  if (z %in% c("set", "str", "string", "1c", "1cfull")) return("set")
  if (z %in% c("mapping", "map")) return("mapping")
  if (z %in% c("real", "re", "refull", "numeric", "dense")) return("real")
  if (z %in% c("sparse", "respse")) return("sparse")
  if (z %in% c("integer", "int", "2i", "2ifull")) return("integer")
  stop("Unknown header_type value: ", as.character(x)[1])
}

#' @keywords internal
#' @noRd
detect_header_type <- function(hdr, obj, header_type, mappings) {
  if (!is.null(mappings) && hdr %in% names(mappings)) return("mapping")
  if (!is.null(header_type) && hdr %in% names(header_type)) {
    explicit <- normalize_header_type_value(header_type[[hdr]])
    if (!is.null(explicit)) return(explicit)
  }
  if (is.character(obj)) return("set")
  if (is.matrix(obj) && is.integer(obj)) return("integer")
  if (is.data.frame(obj)) return("real")
  if (is.numeric(obj) || is.matrix(obj) || is.array(obj)) return("real")
  "unknown"
}

#' @keywords internal
#' @noRd
validate_mapping_specs <- function(mappings, data_list) {
  if (is.null(mappings)) return(invisible(TRUE))
  for (hdr in names(mappings)) {
    spec <- normalize_header_names(mappings[[hdr]])
    if (length(spec) != 2) stop(sprintf("mappings[['%s']] must be c(source_set, destination_set)", hdr))
    if (!(hdr %in% names(data_list))) stop(sprintf("Mapping header '%s' is not in data_list", hdr))
    if (!(spec[1] %in% names(data_list)) || !is.character(data_list[[spec[1]]])) stop(sprintf("Source set '%s' for mapping '%s' must be a character vector in data_list", spec[1], hdr))
    if (!(spec[2] %in% names(data_list)) || !is.character(data_list[[spec[2]]])) stop(sprintf("Destination set '%s' for mapping '%s' must be a character vector in data_list", spec[2], hdr))
    mappings[[hdr]] <- spec
  }
  invisible(TRUE)
}

#' @keywords internal
#' @noRd
get_long_desc <- function(hdr, desc_list, default) {
  if (!is.null(desc_list) && hdr %in% names(desc_list) && !is.null(desc_list[[hdr]])) return(as.character(desc_list[[hdr]])[1])
  default
}

#' @keywords internal
#' @noRd
prepare_mapping_values <- function(hdr, obj, spec, data_list) {
  spec <- normalize_header_names(spec)
  from_set <- spec[1]
  to_set <- spec[2]
  from_elements <- as.character(unname(data_list[[from_set]]))
  to_elements <- as.character(unname(data_list[[to_set]]))
  obj <- as.character(obj)
  if (!is.null(names(obj)) && any(names(obj) != "")) {
    obj_names <- as.character(names(obj))
    values <- setNames(rep("0", length(from_elements)), from_elements)
    common <- intersect(from_elements, obj_names)
    if (length(common) > 0) values[common] <- unname(obj[match(common, obj_names)])
    missing_source <- setdiff(from_elements, obj_names)
    extra_source <- setdiff(obj_names[obj_names != ""], from_elements)
    if (length(missing_source) > 0) warning(sprintf("Header '%s' has missing source elements written as '0': %s", hdr, paste(missing_source, collapse = ", ")))
    if (length(extra_source) > 0) warning(sprintf("Header '%s' has source elements not in '%s' and they are ignored: %s", hdr, from_set, paste(extra_source, collapse = ", ")))
    map_values <- unname(values)
  } else {
    if (length(obj) != length(from_elements)) stop(sprintf("Header '%s' has %d mapping values but source set '%s' has %d elements", hdr, length(obj), from_set, length(from_elements)))
    map_values <- obj
  }
  map_values[is.na(map_values) | trimws(map_values) == ""] <- "0"
  bad <- !(map_values %in% c(to_elements, "0"))
  if (any(bad)) {
    warning(sprintf("Header '%s' has invalid mapping targets written as '0': %s", hdr, paste(unique(map_values[bad]), collapse = ", ")))
    map_values[bad] <- "0"
  }
  list(from_set = from_set, to_set = to_set, from_elements = from_elements, to_elements = to_elements, values = map_values)
}

#' @keywords internal
#' @noRd
write_string <- function(con, hdr_name, elements, description = NULL) {
  hdr_name <- substr(paste0(hdr_name, "    "), 1, 4)
  elements <- as.character(elements)
  if (length(elements) == 0) stop(sprintf("Header '%s' has no character elements", trimws(hdr_name)))
  desc <- substr(paste0(if (is.null(description)) hdr_name else description, strrep(" ", 70)), 1, 70)
  max_len <- max(12, max(nchar(elements)))
  dimensions <- c(length(elements), max_len)
  rec1 <- charToRaw(hdr_name)
  writeBin(as.integer(4), con, size = 4)
  writeBin(rec1, con)
  writeBin(as.integer(4), con, size = 4)
  rec2 <- c(charToRaw("    "), charToRaw("1CFULL"), charToRaw(desc), writeBin(as.integer(2), raw(), size = 4), writeBin(as.integer(dimensions), raw(), size = 4))
  writeBin(as.integer(length(rec2)), con, size = 4)
  writeBin(rec2, con)
  writeBin(as.integer(length(rec2)), con, size = 4)
  padded_elements <- vapply(elements, function(e) {
    e <- substr(e, 1, max_len)
    paste0(e, strrep(" ", max_len - nchar(e)))
  }, character(1), USE.NAMES = FALSE)
  contents <- charToRaw(paste0(padded_elements, collapse = ""))
  rec3 <- c(charToRaw("    "), writeBin(as.integer(1), raw(), size = 4), writeBin(as.integer(length(elements)), raw(), size = 4), writeBin(as.integer(length(elements)), raw(), size = 4), contents)
  writeBin(as.integer(length(rec3)), con, size = 4)
  writeBin(rec3, con)
  writeBin(as.integer(length(rec3)), con, size = 4)
}

#' @keywords internal
#' @noRd
write_2ifull <- function(con, hdr_name, arr, description = NULL) {
  hdr_name <- substr(paste0(hdr_name, "    "), 1, 4)
  if (is.null(description)) description <- hdr_name
  desc <- substr(paste0(description, strrep(" ", 70)), 1, 70)
  dims <- dim(arr)
  if (is.null(dims) || length(dims) != 2) stop("2IFULL requires a two-dimensional integer matrix")
  rec1 <- charToRaw(hdr_name)
  writeBin(as.integer(4), con, size = 4)
  writeBin(rec1, con)
  writeBin(as.integer(4), con, size = 4)
  rec2 <- c(charToRaw("    "), charToRaw("2IFULL"), charToRaw(desc), writeBin(as.integer(2), raw(), size = 4), writeBin(as.integer(dims), raw(), size = 4))
  writeBin(as.integer(length(rec2)), con, size = 4)
  writeBin(rec2, con)
  writeBin(as.integer(length(rec2)), con, size = 4)
  rec3 <- c(charToRaw("    "), writeBin(1L, raw(), size = 4), writeBin(as.integer(dims[1]), raw(), size = 4), writeBin(as.integer(dims[2]), raw(), size = 4), writeBin(1L, raw(), size = 4), writeBin(as.integer(dims[1]), raw(), size = 4), writeBin(1L, raw(), size = 4), writeBin(as.integer(dims[2]), raw(), size = 4), writeBin(as.integer(arr), raw(), size = 4))
  writeBin(as.integer(length(rec3)), con, size = 4)
  writeBin(rec3, con)
  writeBin(as.integer(length(rec3)), con, size = 4)
}

#' @keywords internal
#' @noRd
write_matrix <- function(con, hdr_name, arr, description = NULL, coefficient = NULL, maxSize = 2e6) {
  hdr_name <- substr(paste0(hdr_name, "    "), 1, 4)
  if (is.null(description)) description <- hdr_name
  description <- substr(paste0(description, strrep(" ", 70)), 1, 70)
  if (is.null(coefficient)) coefficient <- trimws(hdr_name)
  coefficient <- substr(paste0(coefficient, strrep(" ", 12)), 1, 12)
  dimensions <- dim(arr)
  if (is.null(dimensions)) dimensions <- c(length(arr))
  used_dimensions <- length(dimensions)
  dimensions <- c(dimensions, rep(1, 7 - length(dimensions)))
  message(sprintf("%s with maxsize %s", trimws(hdr_name), maxSize))
  rec1 <- charToRaw(hdr_name)
  writeBin(as.integer(4), con, size = 4)
  writeBin(rec1, con)
  writeBin(as.integer(4), con, size = 4)
  rec2 <- c(charToRaw("    "), charToRaw("REFULL"), charToRaw(description), writeBin(as.integer(7), raw(), size = 4), writeBin(as.integer(dimensions), raw(), size = 4))
  writeBin(as.integer(length(rec2)), con, size = 4)
  writeBin(rec2, con)
  writeBin(as.integer(length(rec2)), con, size = 4)
  if (!is.null(dimnames(arr)) && length(names(dimnames(arr))) > 0) {
    dim_names <- names(dimnames(arr))
    set_names_raw <- unlist(lapply(dim_names, function(nm) charToRaw(substr(paste0(nm, strrep(" ", 12)), 1, 12))))
    set_names <- c(set_names_raw, as.raw(rep(0x6b, length(dim_names))), as.raw(rep(0x00, 4 + 4 * length(dim_names))))
    defined_dims <- length(unique(dim_names))
    used_dims <- length(dim_names)
  } else {
    set_names <- as.raw(c(0x00, 0x00, 0x00, 0x00))
    defined_dims <- 0
    used_dims <- 0
  }
  rec3 <- c(charToRaw("    "), writeBin(as.integer(defined_dims), raw(), size = 4), as.raw(c(0xff, 0xff, 0xff, 0xff)), writeBin(as.integer(used_dims), raw(), size = 4), charToRaw(coefficient), as.raw(c(0xff, 0xff, 0xff, 0xff)), set_names)
  writeBin(as.integer(length(rec3)), con, size = 4)
  writeBin(rec3, con)
  writeBin(as.integer(length(rec3)), con, size = 4)
  if (!is.null(dimnames(arr)) && length(dimnames(arr)) > 0) {
    for (ud in unique(names(dimnames(arr)))) {
      ele <- dimnames(arr)[[which(names(dimnames(arr)) == ud)[1]]]
      padded_ele <- vapply(ele, function(e) {
        e <- substr(e, 1, 12)
        paste0(e, strrep(" ", 12 - nchar(e)))
      }, character(1), USE.NAMES = FALSE)
      element_data <- unlist(lapply(padded_ele, charToRaw))
      rec_ele <- c(charToRaw("    "), writeBin(as.integer(1), raw(), size = 4), writeBin(as.integer(length(ele)), raw(), size = 4), writeBin(as.integer(length(ele)), raw(), size = 4), element_data)
      writeBin(as.integer(length(rec_ele)), con, size = 4)
      writeBin(rec_ele, con)
      writeBin(as.integer(length(rec_ele)), con, size = 4)
    }
  }
  total_elements <- prod(dim(arr)[1:used_dimensions])
  if (total_elements <= maxSize) {
    slice_size <- total_elements
    num_data_records <- 1
  } else {
    arr2 <- arr
    arr2[] <- FALSE
    arr2[maxSize] <- TRUE
    breaks <- which(arr2 == TRUE, arr.ind = TRUE)
    cut_dim <- 1
    for (cc in length(dim(arr)):1) if (breaks[cc] > 1) { cut_dim <- cc; break }
    cutPoint <- rep(1, length(dim(arr)))
    cutPoint[cut_dim] <- 2
    arr2[] <- FALSE
    arr2[matrix(cutPoint, nrow = 1)] <- TRUE
    slice_size <- which(arr2 == TRUE) - 1
    if (length(slice_size) == 0 || slice_size < 1) slice_size <- total_elements
    num_data_records <- ceiling(total_elements / slice_size)
  }
  rec_frame <- c(charToRaw("    "), writeBin(as.integer(1 + num_data_records * 2), raw(), size = 4), writeBin(as.integer(7), raw(), size = 4), writeBin(as.integer(dimensions), raw(), size = 4))
  writeBin(as.integer(length(rec_frame)), con, size = 4)
  writeBin(rec_frame, con)
  writeBin(as.integer(length(rec_frame)), con, size = 4)
  arr_vector <- as.numeric(as.vector(arr))
  for (dr in seq_len(num_data_records)) {
    from_element <- (dr - 1) * slice_size + 1
    to_element <- min(dr * slice_size, total_elements)
    arr2 <- arr
    arr2[] <- FALSE
    arr2[from_element] <- TRUE
    arr2[to_element] <- TRUE
    from_to_indices <- as.matrix(which(arr2 == TRUE, arr.ind = TRUE))
    if (nrow(from_to_indices) == 1) from_to_indices <- rbind(from_to_indices, from_to_indices)
    from_to_vector <- c(as.vector(from_to_indices), rep(c(1, 1), 7 - length(dim(arr))))
    rec_bounds <- c(charToRaw("    "), writeBin(as.integer(num_data_records * 2 - dr * 2 + 2), raw(), size = 4), writeBin(as.integer(from_to_vector), raw(), size = 4))
    writeBin(as.integer(length(rec_bounds)), con, size = 4)
    writeBin(rec_bounds, con)
    writeBin(as.integer(length(rec_bounds)), con, size = 4)
    data_chunk <- arr_vector[from_element:to_element]
    rec_data <- c(charToRaw("    "), writeBin(as.integer(num_data_records * 2 - dr * 2 + 1), raw(), size = 4), writeBin(as.numeric(data_chunk), raw(), size = 4))
    writeBin(as.integer(length(rec_data)), con, size = 4)
    writeBin(rec_data, con)
    writeBin(as.integer(length(rec_data)), con, size = 4)
  }
}

#' @keywords internal
#' @noRd
write_sparse <- function(con, hdr_name, arr, description = NULL, coefficient = NULL, maxSize = 2e6) {
  hdr_name <- substr(paste0(hdr_name, "    "), 1, 4)
  if (is.null(description)) description <- hdr_name
  description <- substr(paste0(description, strrep(" ", 70)), 1, 70)
  if (is.null(coefficient)) coefficient <- trimws(hdr_name)
  coefficient <- substr(paste0(coefficient, strrep(" ", 12)), 1, 12)
  dimensions <- dim(arr)
  if (is.null(dimensions)) dimensions <- c(length(arr))
  used_dimensions <- length(dimensions)
  dimensions <- c(dimensions, rep(1, 7 - length(dimensions)))
  message(sprintf("%s with maxsize %s", trimws(hdr_name), maxSize))
  rec1 <- charToRaw(hdr_name)
  writeBin(as.integer(4), con, size = 4)
  writeBin(rec1, con)
  writeBin(as.integer(4), con, size = 4)
  rec2 <- c(charToRaw("    "), charToRaw("RESPSE"), charToRaw(description), writeBin(as.integer(7), raw(), size = 4), writeBin(as.integer(dimensions), raw(), size = 4))
  writeBin(as.integer(length(rec2)), con, size = 4)
  writeBin(rec2, con)
  writeBin(as.integer(length(rec2)), con, size = 4)
  if (!is.null(dimnames(arr)) && length(names(dimnames(arr))) > 0) {
    dim_names <- names(dimnames(arr))
    set_names_raw <- unlist(lapply(dim_names, function(nm) charToRaw(substr(paste0(nm, strrep(" ", 12)), 1, 12))))
    set_names <- c(set_names_raw, as.raw(rep(0x6b, length(dim_names))), as.raw(rep(0x00, 4 + 4 * length(dim_names))))
    defined_dims <- length(unique(dim_names))
    used_dims <- length(dim_names)
  } else {
    set_names <- as.raw(c(0x00, 0x00, 0x00, 0x00))
    defined_dims <- 0
    used_dims <- 0
  }
  rec3 <- c(charToRaw("    "), writeBin(as.integer(defined_dims), raw(), size = 4), as.raw(c(0xff, 0xff, 0xff, 0xff)), writeBin(as.integer(used_dims), raw(), size = 4), charToRaw(coefficient), as.raw(c(0xff, 0xff, 0xff, 0xff)), set_names)
  writeBin(as.integer(length(rec3)), con, size = 4)
  writeBin(rec3, con)
  writeBin(as.integer(length(rec3)), con, size = 4)
  if (!is.null(dimnames(arr)) && length(dimnames(arr)) > 0) {
    for (ud in unique(names(dimnames(arr)))) {
      ele <- dimnames(arr)[[which(names(dimnames(arr)) == ud)[1]]]
      padded_ele <- vapply(ele, function(e) {
        e <- substr(e, 1, 12)
        paste0(e, strrep(" ", 12 - nchar(e)))
      }, character(1), USE.NAMES = FALSE)
      element_data <- unlist(lapply(padded_ele, charToRaw))
      rec_ele <- c(charToRaw("    "), writeBin(as.integer(1), raw(), size = 4), writeBin(as.integer(length(ele)), raw(), size = 4), writeBin(as.integer(length(ele)), raw(), size = 4), element_data)
      writeBin(as.integer(length(rec_ele)), con, size = 4)
      writeBin(rec_ele, con)
      writeBin(as.integer(length(rec_ele)), con, size = 4)
    }
  }
  val <- as.vector(arr)
  pos <- seq_along(val)
  nonzeros <- val != 0
  nzval <- val[nonzeros]
  nzpos <- pos[nonzeros]
  sparse_frame <- c(charToRaw("    "), writeBin(as.integer(length(nzval)), raw(), size = 4), writeBin(4L, raw(), size = 4), writeBin(4L, raw(), size = 4), charToRaw(substr(strrep(" ", 80), 1, 80)))
  writeBin(as.integer(length(sparse_frame)), con, size = 4)
  writeBin(sparse_frame, con)
  writeBin(as.integer(length(sparse_frame)), con, size = 4)
  if (length(nzval) == 0) {
    rec_sparse <- c(charToRaw("    "), writeBin(as.integer(1), raw(), size = 4), writeBin(as.integer(0), raw(), size = 4), writeBin(as.integer(0), raw(), size = 4))
    writeBin(as.integer(length(rec_sparse)), con, size = 4)
    writeBin(rec_sparse, con)
    writeBin(as.integer(length(rec_sparse)), con, size = 4)
    return(invisible(NULL))
  }
  numberDataRecords <- if (length(nzval) <= maxSize / 2) 1 else ceiling(2 * length(nzval) / maxSize)
  chunk_size <- ceiling(length(nzval) / numberDataRecords)
  for (dr in seq_len(numberDataRecords)) {
    fromElement <- (dr - 1) * chunk_size + 1
    toElement <- min(dr * chunk_size, length(nzval))
    positions <- nzpos[fromElement:toElement]
    values <- nzval[fromElement:toElement]
    rec_sparse <- c(charToRaw("    "), writeBin(as.integer(numberDataRecords + 1 - dr), raw(), size = 4), writeBin(as.integer(length(nzval)), raw(), size = 4), writeBin(as.integer(length(values)), raw(), size = 4), writeBin(as.integer(positions), raw(), size = 4), writeBin(as.double(values), raw(), size = 4))
    writeBin(as.integer(length(rec_sparse)), con, size = 4)
    writeBin(rec_sparse, con)
    writeBin(as.integer(length(rec_sparse)), con, size = 4)
  }
}

#' @keywords internal
#' @importFrom stats complete.cases
#' @noRd
convert_df_to_array <- function(df, dim_cols, val_col, lowercase) {
  dim_cols <- as.character(dim_cols)
  val_col <- as.character(val_col)[1]
  needed <- c(dim_cols, val_col)
  missing_cols <- setdiff(needed, names(df))
  if (length(missing_cols) > 0) stop("Missing column(s): ", paste(missing_cols, collapse = ", "))
  df <- df[complete.cases(df[, needed]), , drop = FALSE]
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
rename_array_dims <- function(arr, rename_map) {
  if (is.null(dimnames(arr)) || is.null(names(dimnames(arr)))) return(arr)
  old_names <- names(dimnames(arr))
  new_names <- old_names
  for (i in seq_along(old_names)) if (old_names[i] %in% names(rename_map)) new_names[i] <- rename_map[[old_names[i]]]
  names(dimnames(arr)) <- new_names
  arr
}

#' @keywords internal
#' @noRd
reorder_array_by_sets <- function(arr, set_list) {
  if (is.null(dimnames(arr)) || is.null(set_list)) return(arr)
  dim_names <- names(dimnames(arr))
  for (i in seq_along(dim_names)) {
    dim_key <- toupper(substr(dim_names[i], 1, 4))
    if (dim_key %in% names(set_list)) {
      current_vals <- dimnames(arr)[[i]]
      desired_order <- set_list[[dim_key]]
      common_vals <- intersect(desired_order, current_vals)
      if (length(common_vals) > 0 && !identical(current_vals, common_vals)) {
        idx_list <- vector("list", length(dim(arr)))
        for (j in seq_along(idx_list)) idx_list[[j]] <- if (j == i) match(common_vals, current_vals) else seq_len(dim(arr)[j])
        arr <- do.call(`[`, c(list(arr), idx_list, drop = FALSE))
      }
    }
  }
  arr
}

#' @keywords internal
#' @noRd
extract_unique_sets <- function(arrays) {
  all_sets <- list()
  for (arr in arrays) {
    dn <- names(dimnames(arr))
    for (i in seq_along(dn)) {
      hdr <- toupper(substr(dn[i], 1, 4))
      if (hdr %in% names(all_sets)) all_sets[[hdr]] <- unique(c(all_sets[[hdr]], dimnames(arr)[[i]]))
      else all_sets[[hdr]] <- dimnames(arr)[[i]]
    }
  }
  lapply(all_sets, sort)
}

#' @keywords internal
#' @importFrom utils read.csv
#' @noRd
process_dim_order <- function(dim_order, lowercase) {
  if (is.null(dim_order)) return(NULL)
  if (is.character(dim_order) && length(dim_order) == 1) {
    if (grepl("\\.xlsx?$", dim_order, ignore.case = TRUE)) {
      if (!requireNamespace("openxlsx", quietly = TRUE)) stop("Package 'openxlsx' needed for Excel files. Install with: install.packages('openxlsx')")
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
    return(order_map[sapply(order_map, length) > 0])
  }
  if (is.list(dim_order)) {
    order_map <- lapply(names(dim_order), function(nm) {
      vals <- dim_order[[nm]]
      vals <- vals[!is.na(vals) & nchar(trimws(as.character(vals))) > 0]
      vals <- unique(as.character(vals))
      if (lowercase) tolower(vals) else vals
    })
    names(order_map) <- toupper(substr(names(dim_order), 1, 4))
    return(order_map[sapply(order_map, length) > 0])
  }
  stop("dim_order must be NULL, data frame, list, or file path")
}
