#' @title Create Initial Value Configuration
#'
#' @description
#' Defines the configuration for loading the initial dataset, including path, format,
#' variable header, and value column name. Used as input for \code{\link{shock_calculate}}
#' and \code{\link{shock_calculate_uniform}}.
#'
#' @details
#' - Supports `HAR` and `SL4` file formats
#' - Specifies the header name to extract from the dataset
#' - Allows custom naming for the value column (`Value` by default)
#'
#' @param path Path to the initial data file.
#' @param format File format of the initial dataset. Must be "har" or "sl4".
#' @param header Header name within the HAR or SL4 file to extract.
#' @param value_col Name of the column containing numeric values. Default is "Value".
#'
#' @return A list containing:
#'    - \code{path}: input file path
#'    - \code{format}: file format ("har" or "sl4")
#'    - \code{header}: target header name
#'    - \code{value_col}: column name for numeric values
#'
#' @author Pattawee Puangchit
#'
#' @seealso \code{\link{create_target_config}}, \code{\link{create_calc_config}},
#'   \code{\link{shock_calculate}}, \code{\link{shock_calculate_uniform}}
#'
#' @export
#'
#' @examples
#' # Example: Define Initial Configuration
#' initial <- create_initial_config(
#'   path   = "D:/Data/taxrates_2017.har",
#'   format = "har",
#'   header = "rTMS"
#' )
create_initial_config <- function(path, format, header, value_col = "Value") {
  if (!format %in% c("sl4", "har")) stop("format must be either 'sl4' or 'har'")
  list(path = path, format = tolower(format), header = header, value_col = value_col)
}

#' @title Create Target Value Configuration
#'
#' @description
#' Defines the configuration for loading the target dataset, which represents
#' post-adjustment or comparative rate values. Supports multiple file formats.
#'
#' @details
#' - Supports `HAR`, `SL4`, `CSV`, and `XLSX` file formats
#' - Can also represent a uniform numeric target value when no file path is provided
#' - Used in combination with \code{\link{create_initial_config}} for shock computation
#'
#' @param path Optional path to the target data file.
#' @param type Optional file type for the target dataset ("har", "sl4", "csv", or "xlsx").
#' @param header Optional header name within the HAR or SL4 file to extract.
#' @param value_col Column name containing numeric target values. Default is "Value".
#'
#' @return A list containing:
#'    - \code{path}: file path to target data
#'    - \code{type}: file format (lowercase)
#'    - \code{header}: header name in HAR/SL4 file
#'    - \code{value_col}: column name for target values
#'
#' @author Pattawee Puangchit
#'
#' @seealso \code{\link{create_initial_config}}, \code{\link{create_calc_config}},
#'   \code{\link{shock_calculate}}, \code{\link{shock_calculate_uniform}}
#'
#' @export
#'
#' @examples
#' # Example: Define Target Configuration
#' target <- create_target_config(
#'   path   = "D:/Data/taxrates_2019.har",
#'   type   = "har",
#'   header = "rTMS"
#' )
create_target_config <- function(path = NULL, type = NULL, header = NULL, value_col = "Value") {
  if (!is.null(type) && !type %in% c("sl4", "har", "csv", "xlsx")) {
    stop("type must be 'sl4', 'har', 'csv', 'xlsx', or NULL")
  }
  list(path = path, type = if(!is.null(type)) tolower(type) else NULL, header = header, value_col = value_col)
}

#' @title Create Calculation Configuration
#'
#' @description
#' Defines calculation settings for generating shock values, including variable mapping,
#' timeline sequence, and exclusion criteria. Used as input for both
#' \code{\link{shock_calculate}} and \code{\link{shock_calculate_uniform}}.
#'
#' @details
#' - Controls column alignment between initial and target datasets
#' - Supports numeric or range-based timeline definitions (e.g., "1-10")
#' - Excludes self-trade or specified region/sector pairs if configured
#' - Provides core metadata for shock calculation functions
#'
#' @param column_mapping Optional named vector mapping initial and target columns
#'   (e.g., \code{c(COMM = "COMM", REG = "REG", REG.1 = "REG.1")}).
#' @param timeline Numeric or character range defining simulation periods.
#'   - Example: "1-5" expands to 1:5.
#' @param exclude_self_trade Logical; if TRUE, removes intra-region records. Default is FALSE.
#' @param exclusion_values Optional named list of elements to exclude from calculation.
#'
#' @return A list containing:
#'    - \code{column_mapping}: variable mapping between datasets
#'    - \code{timeline}: expanded numeric sequence of simulation periods
#'    - \code{exclude_self_trade}: logical flag
#'    - \code{exclusion_values}: exclusion list by dimension
#'
#' @author Pattawee Puangchit
#'
#' @seealso \code{\link{create_initial_config}}, \code{\link{create_target_config}},
#'   \code{\link{shock_calculate}}, \code{\link{shock_calculate_uniform}}
#'
#' @export
#'
#' @examples
#' # Example: Define Calculation Configuration
#' calc <- create_calc_config(
#'   column_mapping      = c(COMM = "COMM", REG = "REG", REG.1 = "REG.1"),
#'   timeline            = "1-5",
#'   exclude_self_trade  = TRUE
#' )
create_calc_config <- function(column_mapping = NULL, timeline = 1, 
                               exclude_self_trade = FALSE, exclusion_values = NULL) {
  if (is.character(timeline) && grepl("-", timeline)) {
    range_parts <- as.numeric(strsplit(timeline, "-")[[1]])
    timeline_seq <- seq(range_parts[1], range_parts[2])
  } else {
    timeline_seq <- as.numeric(timeline)
  }
  list(column_mapping = column_mapping, timeline = timeline_seq, 
       exclude_self_trade = exclude_self_trade, exclusion_values = exclusion_values)
}

#' @title Calculate Shocks with Uniform Adjustment
#'
#' @description
#' Generates GEMPACK-style percentage shocks using a uniform proportional adjustment
#' applied to all values in the initial dataset. The function supports additive or
#' multiplicative adjustments, single or multi-period configurations, and direct HAR export.
#'
#' @details
#' - Applies uniform adjustment to all base rates across defined dimensions
#' - Supports additive ("+", "-") and proportional ("*", "/") adjustments
#' - Computes compounded shocks following the "power of tax" formulation
#' - Handles multiple time periods as defined by `timeline` in \code{calc_config}
#' - Excludes self-trade or specified region/sector pairs if configured
#' - Outputs results as multi-header HAR file (one per timeline period)
#' - Optionally generates new baseline rate file showing post-adjustment values
#'
#' @param initial_config A list created by \code{\link{create_initial_config}}, defining:
#'    - Input file path, format, and variable header
#'    - Column name for initial rate values ("Value_ini")
#' @param adjustment_value Numeric scalar specifying the uniform adjustment to apply.
#'    - Interpreted according to \code{calculation_method}
#'    - For example, 0.5 with method "*" halves the base rate
#' @param calculation_method Operator defining the adjustment method:
#'    - "*" multiply (default)
#'    - "/" divide
#'    - "+" add
#'    - "-" subtract
#' @param calc_config A list created by \code{\link{create_calc_config}}, specifying:
#'    - \code{timeline}: sequence of simulation periods (e.g., "1-10")
#'    - \code{exclude_self_trade}: logical, whether to omit intra-region pairs
#'    - \code{exclusion_values}: optional list defining excluded elements
#' @param output_path Path to the output HAR file where calculated shocks will be written.
#' @param long_desc Optional text for header description. Default is "Uniform shock adjustment".
#' @param dim_order Optional dimension ordering specification. Can be:
#'    - NULL (default): alphabetical A-Z ordering
#'    - a named list defining preferred order per dimension
#'    - a data frame or path to Excel/CSV with explicit order definitions
#' @param lowercase Logical; if TRUE, converts all dimension elements to lowercase. Default is FALSE.
#' @param new_baseline Logical; if TRUE, generates an additional HAR file with the new baseline rates
#'    (target values where available, initial values otherwise). The output file will have "_baseline"
#'    appended to the filename. Default is FALSE.
#'
#' @return Invisibly returns a list containing summary metadata:
#'    - \code{n_observations}: total records processed
#'    - \code{n_included}: records included in shock computation
#'    - \code{n_excluded}: records excluded by configuration
#'    - \code{output_path}: normalized path to the generated HAR file
#'    - \code{baseline_path}: normalized path to baseline file (if new_baseline = TRUE)
#'
#' @author Pattawee Puangchit
#'
#' @seealso \code{\link{shock_calculate}}, \code{\link{create_initial_config}},
#'   \code{\link{create_calc_config}}, \code{\link{save_har}}
#'
#' @export
#'
#' @examples
#' # Example 1: Uniform Shock (50% Reduction) with New Baseline
#' har_path <- system.file("extdata", "baserate.har", package = "HARplus")
#'
#' # Sorting Column
#' mapping <- list(
#'   REG = c("USA", "EU", "ROW")
#' )
#'
#' # Initial File
#' initial <- create_initial_config(
#'   path   = har_path,
#'   format = "har",
#'   header = "rTMS"
#' )
#'
#' # Calculation Setup
#' calc <- create_calc_config(
#'   timeline           = "1-10",
#'   exclude_self_trade = TRUE
#' )
#'
#' # Compute Uniform 50% Reduction (Value_tar = Value_ini * 0.5)
#' # Also generate new baseline file showing the adjusted rates
#' shock_calculate_uniform(
#'   initial_config     = initial,
#'   adjustment_value   = 0.5,
#'   calculation_method = "*",
#'   calc_config        = calc,
#'   output_path        = file.path(tempdir(), "output_uniform.har"),
#'   dim_order          = mapping,
#'   new_baseline       = TRUE
#' )
shock_calculate_uniform <- function(initial_config, adjustment_value, 
                                    calculation_method = "*", calc_config,
                                    output_path, long_desc = "Uniform shock adjustment",
                                    dim_order = NULL, lowercase = FALSE, new_baseline = FALSE) {
  
  if (!calculation_method %in% c("+", "-", "*", "/")) {
    stop("calculation_method must be one of: '+', '-', '*', '/'")
  }
  
  initial_df <- load_data_simple(initial_config$path, initial_config$format, initial_config$header)
  
  if (!initial_config$value_col %in% names(initial_df)) {
    stop(sprintf("Value column '%s' not found", initial_config$value_col))
  }
  
  meta_cols <- c("Value", "Variable", "Dimension", "Experiment", "Subtotal")
  dim_cols <- setdiff(names(initial_df), meta_cols)
  
  if (!is.null(calc_config$exclusion_values)) {
    for (col in names(calc_config$exclusion_values)) {
      if (!col %in% names(initial_df)) {
        stop(sprintf("Exclusion column '%s' not found", col))
      }
    }
  }
  
  keep_mask <- rep(TRUE, nrow(initial_df))
  
  if (calc_config$exclude_self_trade && length(dim_cols) > 1) {
    for (i in 1:(length(dim_cols) - 1)) {
      for (j in (i + 1):length(dim_cols)) {
        self_mask <- initial_df[[dim_cols[i]]] == initial_df[[dim_cols[j]]]
        self_mask[is.na(self_mask)] <- FALSE
        keep_mask <- keep_mask & !self_mask
      }
    }
  }
  
  if (!is.null(calc_config$exclusion_values)) {
    for (col in names(calc_config$exclusion_values)) {
      if (col %in% names(initial_df)) {
        ex_mask <- initial_df[[col]] %in% calc_config$exclusion_values[[col]]
        ex_mask[is.na(ex_mask)] <- FALSE
        keep_mask <- keep_mask & !ex_mask
      }
    }
  }
  
  timeline_labels <- c("ONEY", "TWOY", "THRY", "FOUR", "FIVE", "SIXY", "SEVN", 
                       "EIGT", "NINE", "TENY", "ELVN", "TWLV", "THTN", "FRTN", "FFTN",
                       "SXTN", "SVTN", "EGTN", "NNTN", "TWTY", "TONE", "TTWO", "TTRE",
                       "TFOR", "TFIV", "TSIX", "TSEV", "TEIG", "TNIN", "THTY", "TONE",
                       "TTWO", "TTRE", "TFOR", "TFIV", "TSIX", "TSEV", "TEIG", "TNIN",
                       "FRTY", "FONE", "FTWO", "FTRE", "FFOR", "FFIV", "FSIX", "FSEV",
                       "FEIG", "FNIN", "FFTY", "FONE", "FTWO", "FTRE", "FFOR", "FFIV",
                       "FSIX", "FSEV", "FEIG", "FNIN", "SXTY", "SONE", "STWO", "STRE",
                       "SFOR", "SFIV", "SSIX", "SSEV", "SEIG", "SNIN", "SVTY", "VONE",
                       "VTWO", "VTRE", "VFOR", "VFIV", "VSIX", "VSEV", "VEIG", "VNIN",
                       "EGTY", "EONE", "ETWO", "ETRE", "EFOR", "EFIV", "ESIX", "ESEV",
                       "EEIG", "ENIN", "NNTY", "NONE", "NTWO", "NTRE", "NFOR", "NFIV",
                       "NSIX", "NSEV", "NEIG", "NNIN", "HUND")
  output_headers <- timeline_labels[1:length(calc_config$timeline)]
  
  shock_list <- list()
  
  for (i in seq_along(calc_config$timeline)) {
    period <- calc_config$timeline[i]
    shock_df <- initial_df
    
    Value_ini <- shock_df[[initial_config$value_col]]
    
    # Calculate target values
    Value_tar <- switch(calculation_method,
                        "+" = Value_ini + adjustment_value,
                        "-" = Value_ini - adjustment_value,
                        "*" = Value_ini * adjustment_value,
                        "/" = Value_ini / adjustment_value
    )
    
    # Calculate compound shock using power-of-tax formula
    denominator <- 1 + Value_ini/100
    shock_df$Value <- ifelse(abs(denominator) < 1e-10, 0,
                             100 * (((1 + Value_tar/100) / denominator)^(1/period) - 1))
    shock_df$Value[is.na(shock_df$Value)] <- 0
    shock_df$Value[is.infinite(shock_df$Value)] <- 0
    shock_df$Value[!keep_mask] <- 0
    
    keep_cols <- c(dim_cols, "Value")
    if ("Subtotal" %in% names(shock_df)) keep_cols <- c(keep_cols, "Subtotal")
    
    shock_list[[output_headers[i]]] <- shock_df[, keep_cols, drop = FALSE]
  }
  
  dimension_list <- lapply(shock_list, function(x) setdiff(names(x), c("Value", "Subtotal")))
  names(dimension_list) <- output_headers
  
  dim_rename_list <- lapply(output_headers, function(header) {
    dims <- dimension_list[[header]]
    setNames(gsub("\\.\\d+$", "", dims), dims)
  })
  names(dim_rename_list) <- output_headers
  
  unique_dim_names <- unique(unlist(lapply(dim_rename_list, function(x) unique(as.character(x)))))
  
  processed_dim_order <- process_dim_order(dim_order, lowercase)
  if (!is.null(processed_dim_order)) {
    expanded_dim_order <- list()
    for (base_name in unique_dim_names) {
      if (base_name %in% names(processed_dim_order)) {
        expanded_dim_order[[base_name]] <- processed_dim_order[[base_name]]
      }
    }
    processed_dim_order <- if (length(expanded_dim_order) > 0) expanded_dim_order else NULL
  }
  
  long_desc_vec <- setNames(
    paste0(long_desc, " (Year ", calc_config$timeline, ")"),
    output_headers
  )
  
  save_har(
    data_list = shock_list,
    file_path = output_path,
    dimensions = dimension_list,
    value_cols = setNames(rep("Value", length(shock_list)), output_headers),
    long_desc = long_desc_vec,
    dim_rename = dim_rename_list,
    export_sets = TRUE,
    lowercase = lowercase,
    dim_order = processed_dim_order
  )
  
  # Generate new baseline file if requested
  if (new_baseline) {
    baseline_path <- sub("\\.har$", "_baseline.har", output_path, ignore.case = TRUE)
    if (baseline_path == output_path) {
      baseline_path <- paste0(tools::file_path_sans_ext(output_path), "_baseline.har")
    }
    
    # Create single baseline dataframe (not time-dependent)
    baseline_df <- initial_df
    
    Value_ini <- baseline_df[[initial_config$value_col]]
    
    # Calculate new baseline rates (target values)
    Value_tar <- switch(calculation_method,
                        "+" = Value_ini + adjustment_value,
                        "-" = Value_ini - adjustment_value,
                        "*" = Value_ini * adjustment_value,
                        "/" = Value_ini / adjustment_value
    )
    
    # For new baseline, use target value where included, initial value where excluded
    baseline_df$Value <- ifelse(keep_mask, Value_tar, Value_ini)
    baseline_df$Value[is.na(baseline_df$Value)] <- Value_ini[is.na(baseline_df$Value)]
    baseline_df$Value[is.infinite(baseline_df$Value)] <- Value_ini[is.infinite(baseline_df$Value)]
    
    keep_cols <- c(dim_cols, "Value")
    if ("Subtotal" %in% names(baseline_df)) keep_cols <- c(keep_cols, "Subtotal")
    
    # Single header using initial header name
    baseline_header <- toupper(substr(initial_config$header, 1, 4))
    baseline_list <- list()
    baseline_list[[baseline_header]] <- baseline_df[, keep_cols, drop = FALSE]
    
    baseline_dimension_list <- list()
    baseline_dimension_list[[baseline_header]] <- setdiff(names(baseline_list[[baseline_header]]), c("Value", "Subtotal"))
    
    baseline_dim_rename <- list()
    dims <- baseline_dimension_list[[baseline_header]]
    baseline_dim_rename[[baseline_header]] <- setNames(gsub("\\.\\d+$", "", dims), dims)
    
    save_har(
      data_list = baseline_list,
      file_path = baseline_path,
      dimensions = baseline_dimension_list,
      value_cols = setNames("Value", baseline_header),
      long_desc = setNames("New baseline rates", baseline_header),
      dim_rename = baseline_dim_rename,
      export_sets = TRUE,
      lowercase = lowercase,
      dim_order = processed_dim_order
    )
    
    message(sprintf("\nGenerated new baseline file: %s", normalizePath(baseline_path)))
  }
  
  invisible(list(
    n_observations = nrow(initial_df),
    n_included = sum(keep_mask),
    n_excluded = sum(!keep_mask),
    output_path = normalizePath(output_path),
    baseline_path = if(new_baseline) normalizePath(baseline_path) else NULL
  ))
}

#' @title Calculate Shocks from Initial and Target Values
#'
#' @description
#' Computes compounded GEMPACK-style percentage shocks between initial and target values,
#' producing multi-period shock series for dynamic simulation models. The function
#' automatically aligns dimensions across datasets and exports results in HAR format.
#'
#' @details
#' - Computes percentage shocks using compounded "power of tax" formula
#' - Supports multiple periods defined via `timeline` configuration
#' - Compatible with HAR, SL4, CSV, or XLSX input formats
#' - Excludes self-trade or specified region-sector pairs when configured
#' - Exports results as multi-header HAR file (one header per timeline period)
#' - Optionally generates new baseline rate file showing post-adjustment values
#'
#' @param initial_config A list created by \code{\link{create_initial_config}}, defining:
#'    - Input path, file format, and variable header for the initial dataset
#'    - Column name of the initial value field ("Value_ini")
#' @param target_config A list created by \code{\link{create_target_config}}, defining:
#'    - Path, format, and variable header for the target dataset
#'    - Target value column ("Value_tar") or numeric target for uniform shock
#' @param calc_config A list created by \code{\link{create_calc_config}}, specifying:
#'    - \code{column_mapping}: mapping between initial and target columns
#'    - \code{timeline}: sequence of years or periods (e.g., "1-5")
#'    - \code{exclude_self_trade}: logical, whether to drop self-pairs
#'    - \code{exclusion_values}: list of region/sector values to omit
#' @param output_path Path to the output HAR file where calculated shocks will be written.
#' @param long_desc Optional text for header description. Default is "Calculated shock values".
#' @param dim_order Optional dimension ordering specification. Can be:
#'    - NULL (default): alphabetical A-Z ordering
#'    - a named list defining order for each dimension (e.g., REG, COMM)
#'    - a data frame or path to Excel/CSV file containing order definitions
#' @param lowercase Logical; if TRUE, converts dimension elements to lowercase. Default is FALSE.
#' @param new_baseline Logical; if TRUE, generates an additional HAR file with the new baseline rates
#'    (target values where available, initial values otherwise). The output file will have "_baseline"
#'    appended to the filename. Default is FALSE.
#'
#' @return Invisibly returns a list containing summary metadata:
#'    - \code{n_observations}: total records processed
#'    - \code{n_included}: records included in shock computation
#'    - \code{n_excluded}: records excluded by configuration
#'    - \code{output_path}: normalized path to output HAR file
#'    - \code{baseline_path}: normalized path to baseline file (if new_baseline = TRUE)
#'
#' @author Pattawee Puangchit
#'
#' @seealso \code{\link{shock_calculate_uniform}}, \code{\link{create_initial_config}},
#'   \code{\link{create_target_config}}, \code{\link{create_calc_config}}, \code{\link{save_har}}
#'
#' @export
#'
#' @examples
#' # Example 1: Target-Based Shock Calculation with New Baseline
#' har_path <- system.file("extdata", "baserate.har", package = "HARplus")
#'
#' # Sorting Column
#' mapping <- list(
#'   REG = c("USA", "EU", "ROW")
#' )
#'
#' # Initial File
#' initial <- create_initial_config(
#'   path   = har_path,
#'   format = "har",
#'   header = "rTMS"
#' )
#'
#' # Target File
#' target <- create_target_config(
#'   path   = har_path,
#'   type   = "har",
#'   header = "rTMS"
#' )
#'
#' # Calculation Setup with Column Mapping
#' calc <- create_calc_config(
#'   column_mapping      = c(TRAD_COMM = "TRAD_COMM", REG = "REG", REG.1 = "REG.1"),
#'   timeline            = "1-5",
#'   exclude_self_trade  = TRUE
#' )
#'
#' # Compute Shock Based on Initial and Target Values
#' # Also generate new baseline file
#' shock_calculate(
#'   initial_config = initial,
#'   target_config  = target,
#'   calc_config    = calc,
#'   output_path    = file.path(tempdir(), "output_target.har"),
#'   dim_order      = mapping,
#'   new_baseline   = TRUE
#' )
shock_calculate <- function(initial_config, target_config, calc_config,
                            output_path, long_desc = "Calculated shock values",
                            dim_order = NULL, lowercase = FALSE, new_baseline = FALSE) {
  
  if (is.null(calc_config$column_mapping)) {
    stop("column_mapping is required for target file shocks")
  }
  
  initial_df <- load_data_simple(initial_config$path, initial_config$format, initial_config$header)
  
  if (!initial_config$value_col %in% names(initial_df)) {
    stop(sprintf("Value column '%s' not found in initial", initial_config$value_col))
  }
  
  initial_cols <- names(calc_config$column_mapping)
  target_cols <- unname(calc_config$column_mapping)
  
  missing_initial <- setdiff(initial_cols, names(initial_df))
  if (length(missing_initial) > 0) {
    stop(sprintf("Initial columns not found: %s", paste(missing_initial, collapse = ", ")))
  }
  
  initial_df <- initial_df[, c(initial_cols, initial_config$value_col), drop = FALSE]
  names(initial_df)[names(initial_df) == initial_config$value_col] <- "Value_ini"
  
  if (is.null(target_config$type)) {
    if (!is.numeric(target_config$value_col)) {
      stop("For uniform target, value_col must be numeric")
    }
    initial_df$Value_tar <- target_config$value_col
    merged_df <- initial_df
  } else {
    target_df <- load_data_simple(target_config$path, target_config$type, target_config$header)
    
    if (!target_config$value_col %in% names(target_df)) {
      stop(sprintf("Value column '%s' not found in target", target_config$value_col))
    }
    
    missing_target <- setdiff(target_cols, names(target_df))
    if (length(missing_target) > 0) {
      stop(sprintf("Target columns not found: %s", paste(missing_target, collapse = ", ")))
    }
    
    target_df <- target_df[, c(target_cols, target_config$value_col), drop = FALSE]
    names(target_df)[names(target_df) == target_config$value_col] <- "Value_tar"
    
    names(target_df)[match(target_cols, names(target_df))] <- initial_cols
    
    merged_df <- merge(initial_df, target_df, by = initial_cols, all.x = TRUE)
  }
  
  keep_mask <- rep(TRUE, nrow(merged_df))
  
  if (calc_config$exclude_self_trade && length(initial_cols) > 1) {
    for (i in 1:(length(initial_cols) - 1)) {
      for (j in (i + 1):length(initial_cols)) {
        self_mask <- merged_df[[initial_cols[i]]] == merged_df[[initial_cols[j]]]
        self_mask[is.na(self_mask)] <- FALSE
        keep_mask <- keep_mask & !self_mask
      }
    }
  }
  
  if (!is.null(calc_config$exclusion_values)) {
    for (col in names(calc_config$exclusion_values)) {
      if (col %in% names(merged_df)) {
        ex_mask <- merged_df[[col]] %in% calc_config$exclusion_values[[col]]
        ex_mask[is.na(ex_mask)] <- FALSE
        keep_mask <- keep_mask & !ex_mask
      }
    }
  }
  
  timeline_labels <- c("ONEY", "TWOY", "THRY", "FOUR", "FIVE", "SIXY", "SEVN",
                       "EIGT", "NINE", "TENY", "ELVN", "TWLV", "THTN", "FRTN", "FFTN",
                       "SXTN", "SVTN", "EGTN", "NNTN", "TWTY", "TONE", "TTWO", "TTRE",
                       "TFOR", "TFIV", "TSIX", "TSEV", "TEIG", "TNIN", "THTY", "TONE",
                       "TTWO", "TTRE", "TFOR", "TFIV", "TSIX", "TSEV", "TEIG", "TNIN",
                       "FRTY", "FONE", "FTWO", "FTRE", "FFOR", "FFIV", "FSIX", "FSEV",
                       "FEIG", "FNIN", "FFTY", "FONE", "FTWO", "FTRE", "FFOR", "FFIV",
                       "FSIX", "FSEV", "FEIG", "FNIN", "SXTY", "SONE", "STWO", "STRE",
                       "SFOR", "SFIV", "SSIX", "SSEV", "SEIG", "SNIN", "SVTY", "VONE",
                       "VTWO", "VTRE", "VFOR", "VFIV", "VSIX", "VSEV", "VEIG", "VNIN",
                       "EGTY", "EONE", "ETWO", "ETRE", "EFOR", "EFIV", "ESIX", "ESEV",
                       "EEIG", "ENIN", "NNTY", "NONE", "NTWO", "NTRE", "NFOR", "NFIV",
                       "NSIX", "NSEV", "NEIG", "NNIN", "HUND")
  output_headers <- timeline_labels[1:length(calc_config$timeline)]
  
  shock_list <- list()
  
  for (i in seq_along(calc_config$timeline)) {
    period <- calc_config$timeline[i]
    shock_df <- merged_df
    
    denominator <- 1 + shock_df$Value_ini/100
    shock_df$Value <- ifelse(abs(denominator) < 1e-10, 0,
                             100 * (((1 + shock_df$Value_tar/100) / denominator)^(1/period) - 1))
    shock_df$Value[is.na(shock_df$Value)] <- 0
    shock_df$Value[is.infinite(shock_df$Value)] <- 0
    shock_df$Value[!keep_mask] <- 0
    
    keep_cols <- c(initial_cols, "Value")
    shock_list[[output_headers[i]]] <- shock_df[, keep_cols, drop = FALSE]
  }
  
  dimension_list <- lapply(shock_list, function(x) setdiff(names(x), c("Value", "Subtotal")))
  names(dimension_list) <- output_headers
  
  dim_rename_list <- lapply(output_headers, function(header) {
    dims <- dimension_list[[header]]
    setNames(gsub("\\.\\d+$", "", dims), dims)
  })
  names(dim_rename_list) <- output_headers
  
  unique_dim_names <- unique(unlist(lapply(dim_rename_list, function(x) unique(as.character(x)))))
  
  processed_dim_order <- process_dim_order(dim_order, lowercase)
  if (!is.null(processed_dim_order)) {
    expanded_dim_order <- list()
    for (base_name in unique_dim_names) {
      if (base_name %in% names(processed_dim_order)) {
        expanded_dim_order[[base_name]] <- processed_dim_order[[base_name]]
      }
    }
    processed_dim_order <- if (length(expanded_dim_order) > 0) expanded_dim_order else NULL
  }
  
  long_desc_vec <- setNames(
    paste0(long_desc, " (Year ", calc_config$timeline, ")"),
    output_headers
  )
  
  save_har(
    data_list = shock_list,
    file_path = output_path,
    dimensions = dimension_list,
    value_cols = setNames(rep("Value", length(shock_list)), output_headers),
    long_desc = long_desc_vec,
    dim_rename = dim_rename_list,
    export_sets = TRUE,
    lowercase = lowercase,
    dim_order = processed_dim_order
  )
  
  # Generate new baseline file if requested
  if (new_baseline) {
    baseline_path <- sub("\\.har$", "_baseline.har", output_path, ignore.case = TRUE)
    if (baseline_path == output_path) {
      baseline_path <- paste0(tools::file_path_sans_ext(output_path), "_baseline.har")
    }
    
    # Create single baseline dataframe (not time-dependent)
    baseline_df <- merged_df
    
    # For new baseline, use target value where available and included, initial value otherwise
    baseline_df$Value <- ifelse(keep_mask & !is.na(baseline_df$Value_tar), 
                                baseline_df$Value_tar, 
                                baseline_df$Value_ini)
    baseline_df$Value[is.na(baseline_df$Value)] <- baseline_df$Value_ini[is.na(baseline_df$Value)]
    baseline_df$Value[is.infinite(baseline_df$Value)] <- baseline_df$Value_ini[is.infinite(baseline_df$Value)]
    
    keep_cols <- c(initial_cols, "Value")
    
    # Single header using initial header name
    baseline_header <- toupper(substr(initial_config$header, 1, 4))
    baseline_list <- list()
    baseline_list[[baseline_header]] <- baseline_df[, keep_cols, drop = FALSE]
    
    baseline_dimension_list <- list()
    baseline_dimension_list[[baseline_header]] <- setdiff(names(baseline_list[[baseline_header]]), c("Value", "Subtotal"))
    
    baseline_dim_rename <- list()
    dims <- baseline_dimension_list[[baseline_header]]
    baseline_dim_rename[[baseline_header]] <- setNames(gsub("\\.\\d+$", "", dims), dims)
    
    save_har(
      data_list = baseline_list,
      file_path = baseline_path,
      dimensions = baseline_dimension_list,
      value_cols = setNames("Value", baseline_header),
      long_desc = setNames("New baseline rates", baseline_header),
      dim_rename = baseline_dim_rename,
      export_sets = TRUE,
      lowercase = lowercase,
      dim_order = processed_dim_order
    )
    
    message(sprintf("\nGenerated new baseline file: %s", normalizePath(baseline_path)))
  }
  
  invisible(list(
    n_observations = nrow(merged_df),
    n_included = sum(keep_mask),
    n_excluded = sum(!keep_mask),
    output_path = normalizePath(output_path),
    baseline_path = if(new_baseline) normalizePath(baseline_path) else NULL
  ))
}

#' @keywords internal
#' @noRd
load_data_simple <- function(path, format, header = NULL) {
  if (!file.exists(path)) stop(sprintf("ERROR: File not found: '%s'", path))
  
  if (format == "sl4") {
    if (is.null(header) || header == "") stop("ERROR: Header required for SL4")
    data_obj <- load_sl4x(path, lowercase = FALSE)
    if (!header %in% names(data_obj$data)) {
      stop(sprintf("ERROR: Header '%s' not found\nAvailable: %s",
                   header, paste(names(data_obj$data), collapse = ", ")))
    }
    result <- get_data_by_var(header, data_obj)
    return(result[[1]][[header]])
  } else if (format == "har") {
    if (is.null(header) || header == "") stop("ERROR: Header required for HAR")
    data_obj <- load_harx(path, lowercase = FALSE)
    if (!header %in% names(data_obj$data)) {
      stop(sprintf("ERROR: Header '%s' not found\nAvailable: %s",
                   header, paste(names(data_obj$data), collapse = ", ")))
    }
    result <- get_data_by_var(header, data_obj)
    return(result[[1]][[header]])
  } else if (format == "csv") {
    return(read.csv(path, stringsAsFactors = FALSE))
  } else if (format == "xlsx") {
    if (is.null(header)) stop("ERROR: Sheet required for XLSX")
    if (!requireNamespace("openxlsx", quietly = TRUE)) stop("openxlsx package required")
    return(openxlsx::read.xlsx(path, sheet = header))
  }
  stop("Unsupported format")
}

#' @keywords internal
#' @noRd
process_dim_order <- function(dim_order, lowercase = FALSE) {
  if (is.null(dim_order)) return(NULL)
  
  if (is.data.frame(dim_order)) {
    order_map <- lapply(names(dim_order), function(col) {
      vals <- dim_order[[col]]
      vals <- vals[!is.na(vals) & nchar(trimws(as.character(vals))) > 0]
      vals <- unique(trimws(as.character(vals)))
      if (lowercase) tolower(vals) else vals
    })
    names(order_map) <- names(dim_order)
    return(order_map[sapply(order_map, length) > 0])
  }
  
  if (is.list(dim_order)) {
    order_map <- lapply(dim_order, function(vals) {
      vals <- vals[!is.na(vals) & nchar(trimws(as.character(vals))) > 0]
      vals <- unique(trimws(as.character(vals)))
      if (lowercase) tolower(vals) else vals
    })
    return(order_map[sapply(order_map, length) > 0])
  }
  
  return(NULL)
}