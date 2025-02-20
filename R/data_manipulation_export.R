#' @title Rename Dimensions in SL4 or HAR Objects
#' @description Renames dimension and list names in structured SL4 or HAR objects.
#'
#' @details
#' - Replaces old dimension names with new ones as specified in `mapping_df`.
#' - If `rename_list_names = TRUE`, renames list element names as well.
#' - Ensures consistency across SL4 and HAR datasets.
#'
#' @param data_obj A structured SL4 or HAR object.
#' @param mapping_df A two-column data frame where the first column (`old`) contains the current names, 
#'        and the second column (`new`) contains the new names.
#' @param rename_list_names Logical. If `TRUE`, renames list element names. Default is `FALSE`.
#'
#' @return The modified SL4 or HAR object with updated dimension names and, optionally, updated list names.
#'
#' @author Pattawee Puangchit
#' 
#' @seealso \code{\link{get_data_by_var}}, \code{\link{get_data_by_dims}}
#'
#' @export
#'
#' @examples
#' # Import sample data:
#' sl4_data <- load_sl4x(system.file("extdata", "TAR10.sl4", package = "HARplus"))
#' 
#' # Define a renaming map
#' mapping_df <- data.frame(
#'   old = c("REG", "COMM"),
#'   new = c("Region", "Commodity")
#' )
#'
#' # Rename columns in the dataset
#' rename_dims(sl4_data, mapping_df)
#' 
#' # Rename both columns and list names
#' rename_dims(sl4_data, mapping_df, rename_list_names = TRUE)
#' 
rename_dims <- function(data_obj, mapping_df, rename_list_names = FALSE) {
  if (!is.list(data_obj)) {
    stop("Invalid input: Expected a list object.")
  }
  if (!is.data.frame(mapping_df) || ncol(mapping_df) != 2) {
    stop("Invalid mapping_df: Expected a two-column data frame with old and new names.")
  }
  
  old_names <- mapping_df[[1]]
  new_names <- mapping_df[[2]]
  rename_cols <- setNames(new_names, old_names)
  
  rename_parts <- function(str, mapping) {
    parts <- strsplit(str, "\\*")[[1]]
    new_parts <- sapply(parts, function(part) {
      if (part %in% names(mapping)) {
        mapping[[part]]
      } else {
        part
      }
    })
    paste(new_parts, collapse = "*")
  }
  
  if (rename_list_names) {
    old_names <- names(data_obj)
    new_names <- sapply(old_names, function(name) {
      rename_parts(name, rename_cols)
    })
    names(data_obj) <- new_names
  }
  
  for (var_name in names(data_obj)) {
    df <- data_obj[[var_name]]
    
    if (!is.data.frame(df)) next  
    
    for (old_name in names(rename_cols)) {
      matching_cols <- which(names(df) == old_name)
      if (length(matching_cols) > 0) {
        for (i in seq_along(matching_cols)) {
          new_name <- if (i == 1) rename_cols[old_name] else paste0(rename_cols[old_name], i - 1)
          names(df)[matching_cols[i]] <- new_name
        }
      }
    }
    
    data_obj[[var_name]] <- df 
  }
  
  return(data_obj)  
}


#' @title Pivot Data from SL4 or HAR Objects
#' 
#' @description Transforms long-format SL4 or HAR data into wide format by pivoting selected columns.
#' Supports both single data frames and nested lists.
#'
#' @details
#' - Uses `tidyr::pivot_wider()` internally to reshape data.
#' - Allows multiple columns to be pivoted simultaneously.
#' - Recursively processes nested lists, ensuring all data frames are transformed.
#'
#' @param data_obj A list or data frame. The SL4 or HAR data to pivot.
#' @param pivot_cols Character vector. Column names to use as pivot keys.
#' @param name_repair Character. Method for handling duplicate column names (`"unique"`, `"minimal"`, `"universal"`). Default is `"unique"`.
#'
#' @return A transformed data object where the specified `pivot_cols` are pivoted into wide format.
#'
#' @importFrom tidyr pivot_wider
#'
#' @author Pattawee Puangchit
#' 
#' @seealso \code{\link{get_data_by_var}}, \code{\link{get_data_by_dims}}
#'
#' @export
#'
#' @examples
#' # Import sample data:
#' sl4_data <- load_sl4x(system.file("extdata", "TAR10.sl4", package = "HARplus"))
#' 
#' # Extract multiple variables
#' data_multiple <- get_data_by_var(c("qo", "qxs"), sl4_data)
#' 
#' # Pivot a single column
#' pivoted_data <- pivot_data(data_multiple, pivot_cols = "REG")
#'
#' # Pivot multiple columns
#' pivoted_data_multi <- pivot_data(data_multiple, pivot_cols = c("REG", "COMM"))
#' 
pivot_data <- function(data_obj, pivot_cols, name_repair = "unique") {
  pivot_df <- function(df, cols) {
    if (!inherits(df, "data.frame")) return(df)
    
    available_cols <- cols[cols %in% names(df)]
    if (length(available_cols) == 0) return(df)
    
    non_pivot_cols <- setdiff(names(df), c(available_cols, "Value"))
    
    names_prefix <- paste(available_cols, collapse = "_")
    
    tidyr::pivot_wider(df,
                       id_cols = tidyselect::all_of(non_pivot_cols),
                       names_from = tidyselect::all_of(available_cols),
                       values_from = "Value",
                       names_sep = "_",
                       names_repair = name_repair,
                       names_prefix = paste0(names_prefix, "_"))
  }
  
  process_nested <- function(obj, cols) {
    if (is.data.frame(obj)) {
      return(pivot_df(obj, cols))
    } else if (is.list(obj)) {
      return(lapply(obj, function(x) process_nested(x, cols)))
    }
    return(obj)
  }
  
  process_nested(data_obj, pivot_cols)
}


#' @title Export Data to Various Formats (CSV/STATA/TEXT/RDS/XLSX)
#' 
#' @description Exports structured SL4 or HAR data to multiple file formats, including CSV, Stata, TXT, RDS, and XLSX.
#' Supports nested lists, automatic subfolder creation, and multi-sheet Excel exports.
#'
#' @details
#' - Supports exporting data in `"csv"`, `"stata"`, `"txt"`, `"rds"`, and `"xlsx"` formats.
#' - Handles nested lists and exports each data frame individually.
#' - Optionally creates subfolders for each format (`create_subfolder = TRUE`).
#' - Customizes file names using `prefix`.
#' - When multi_sheet_xlsx = TRUE, all exported data is stored in a **single Excel workbook**, with each dataset as a separate sheet.
#'
#' @param data A list or data frame. The SL4 or HAR data to export.
#' @param output_path Character. The base output directory or file path.
#' @param format Character. The export format (`"csv"`, `"stata"`, `"txt"`, `"rds"`, `"xlsx"`). Default is `"csv"`.
#' @param prefix Character. An optional prefix added to exported file names. Default is `""` (empty).
#' @param create_subfolder Logical. If `TRUE`, creates a subfolder for each format. Default is `FALSE`.
#' @param multi_sheet_xlsx Logical. **Only applicable when `format = "xlsx"`**. If `TRUE`, stores all data in a **single Excel workbook with multiple sheets**. Default is `FALSE`.
#' @param xlsx_filename Character. The name for the Excel file when using multi_sheet_xlsx. If NULL, uses the name of the dataset. Default is `NULL`.
#' 
#' @return A vector of file paths for the exported data.
#' 
#' @importFrom utils write.csv write.table
#' @importFrom haven write_dta
#' @importFrom openxlsx createWorkbook addWorksheet writeData saveWorkbook
#' @importFrom tools file_ext
#'
#' @author Pattawee Puangchit
#' 
#' @seealso \code{\link{pivot_data}}, \code{\link{get_data_by_var}}, \code{\link{get_data_by_dims}}
#'
#' @export
#' 
#' @examples
#' \dontrun{
#' # Import sample data:
#' sl4_data <- load_sl4x(system.file("extdata", "TAR10.sl4", package = "HARplus"))
#' 
#' # Extract data
#' data_multiple <- get_data_by_var(c("qo", "pca"), sl4_data)
#' 
#' # Export
#' export_data(data_multiple, file.path(tempdir(), "output_directory"), 
#'            format = c("csv", "xlsx", "stata", "txt", "rds"),
#'            create_subfolder = TRUE,
#'            multi_sheet_xlsx = TRUE)
#' }
#' 
export_data <- function(data, output_path, format = "csv", prefix = "", 
                        create_subfolder = FALSE, multi_sheet_xlsx = FALSE,
                        xlsx_filename = NULL) {
  
  default_filename <- if (is.null(xlsx_filename)) {
    basename(tools::file_path_sans_ext(output_path))
  } else {
    xlsx_filename
  }
  
  supported_formats <- c("csv", "stata", "txt", "rds", "xlsx")
  if (!all(format %in% supported_formats)) {
    invalid_formats <- format[!format %in% supported_formats]
    stop("Unsupported format(s): ", paste(invalid_formats, collapse = ", "))
  }
  
  output_dir <- if (tools::file_ext(output_path) == "") {
    output_path
  } else {
    dirname(output_path)
  }
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  exported_files <- list()
  
  for (fmt in format) {
    format_dir <- if (create_subfolder) {
      dir_path <- file.path(output_dir, fmt)
      dir.create(dir_path, showWarnings = FALSE, recursive = TRUE)
      dir_path
    } else {
      output_dir
    }
    
    export_single <- function(df, name) {
      ext <- if(fmt == "stata") "dta" else fmt
      file_path <- file.path(format_dir, paste0(prefix, name, ".", ext))
      
      if (fmt == "xlsx") {
        wb <- openxlsx::createWorkbook()
        sheet_name <- make.names(substr(name, 1, 31))
        openxlsx::addWorksheet(wb, sheet_name)
        openxlsx::writeData(wb, sheet_name, df)
        openxlsx::saveWorkbook(wb, file_path, overwrite = TRUE)
      } else {
        switch(fmt,
               "csv" = utils::write.csv(df, file_path, row.names = FALSE),
               "stata" = haven::write_dta(df, file_path),
               "txt" = utils::write.table(df, file_path, sep = "\t", 
                                          row.names = FALSE, quote = FALSE),
               "rds" = base::saveRDS(df, file_path))
      }
      return(file_path)
    }
    
    process_list <- function(lst, parent = "") {
      if (is.data.frame(lst)) {
        name <- if (parent == "") "data" else parent
        return(export_single(lst, name))
      }
      
      if (fmt == "xlsx" && multi_sheet_xlsx) {
        wb <- openxlsx::createWorkbook()
        sheets_added <- FALSE
        
        for (name in names(lst)) {
          if (name == "report") next
          
          item <- lst[[name]]
          current_name <- if (parent == "") name else paste(parent, name, sep = "_")
          current_name <- make.names(substr(gsub("\\*", "_", current_name), 1, 31))
          
          if (is.data.frame(item)) {
            openxlsx::addWorksheet(wb, current_name)
            openxlsx::writeData(wb, current_name, item)
            sheets_added <- TRUE
          } else if (is.list(item)) {
            for (subname in names(item)) {
              if (is.data.frame(item[[subname]])) {
                sheet_name <- make.names(substr(paste(current_name, subname, sep = "_"), 1, 31))
                openxlsx::addWorksheet(wb, sheet_name)
                openxlsx::writeData(wb, sheet_name, item[[subname]])
                sheets_added <- TRUE
              }
            }
          }
        }
        
        if (sheets_added) {
          file_path <- file.path(format_dir, paste0(prefix, default_filename, ".xlsx"))
          openxlsx::saveWorkbook(wb, file_path, overwrite = TRUE)
          return(file_path)
        }
        return(NULL)
      } else {
        exported <- lapply(names(lst), function(name) {
          if (name == "report") return(NULL)
          
          item <- lst[[name]]
          current_name <- if (parent == "") name else paste(parent, name, sep = "_")
          current_name <- gsub("\\*", "_", current_name)
          
          if (is.data.frame(item)) {
            export_single(item, current_name)
          } else if (is.list(item)) {
            process_list(item, current_name)
          }
        })
        return(unlist(exported))
      }
    }
    
    result <- process_list(data)
    if (!is.null(result)) {
      exported_files[[fmt]] <- result
      message(sprintf("Exported %d file(s) to %s format in %s", 
                      length(result), fmt, normalizePath(format_dir)))
    }
  }
  
  invisible(exported_files)
}



# Pivot Data Hirarchy -----------------------------------------------------

#' @title Create Hierarchical Pivot Table from SL4 or HAR Objects
#' 
#' @description Creates hierarchical pivot tables from structured SL4 or HAR data, with optional Excel export.
#' Supports both single data frames and nested lists, preserving dimension hierarchies.
#'
#' @details
#' - Transforms data into hierarchical pivot format with nested column headers.
#' - Supports multiple pivot columns in specified order (e.g., REG > COMM).
#' - Handles both single data frames and nested list structures.
#' - Optional direct export to Excel with formatted hierarchical headers.
#' - Uses efficient data processing with tidyr and openxlsx.
#'
#' @param data_obj A list or data frame. The SL4 or HAR data to pivot.
#' @param pivot_cols Character vector. Column names to use as pivot keys in order of hierarchy.
#' @param name_repair Character. Method for handling duplicate column names 
#'        (`"unique"`, `"minimal"`, `"universal"`). Default is `"unique"`.
#' @param export Logical. If `TRUE`, exports result to Excel. Default is `FALSE`.
#' @param file_path Character. Required if export = TRUE. The path for Excel export.
#' @param xlsx_filename Character. The name for the Excel file when using multi_sheet_xlsx. If NULL, uses the name of the dataset. Default is `NULL`.
#' 
#' @return A pivoted data object with hierarchical structure:
#' - If input is a data frame: Returns a hierarchical pivot table.
#' - If input is a list: Returns a nested list of hierarchical pivot tables.
#' - If export = TRUE: Invisibly returns the pivoted object after Excel export.
#'
#' @author Pattawee Puangchit
#' 
#' @seealso \code{\link{pivot_data}}
#'
#' @importFrom tidyr pivot_wider
#' @importFrom tidyselect all_of
#' @importFrom openxlsx createWorkbook addWorksheet writeData createStyle addStyle saveWorkbook
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Import sample data:
#' sl4_data <- load_sl4x(system.file("extdata", "TAR10.sl4", package = "HARplus"))
#' 
#' # Extract data
#' data_multiple <- get_data_by_var(c("qo", "pca"), sl4_data)
#' 
#' # Create hierarchical pivot without export
#' pivot_hier <- pivot_data_hierarchy(data_multiple, 
#'                                    pivot_cols = c("REG", "COMM"))
#' 
#' # Create and export to Excel in one step
#' pivot_export <- pivot_data_hierarchy(data_multiple, 
#'                                      pivot_cols = c("REG", "COMM"),
#'                                      export = TRUE,
#'                                      file_path = file.path(tempdir(), "pivot_output.xlsx"))
#' }
#' 
pivot_data_hierarchy <- function(data_obj, pivot_cols, name_repair = "unique", 
                                 export = FALSE, file_path = NULL,
                                 xlsx_filename = NULL) {
  if (export && is.null(xlsx_filename)) {
    xlsx_filename <- basename(tools::file_path_sans_ext(file_path))
  }
  if (!is.list(data_obj) && !is.data.frame(data_obj)) {
    stop("data_obj must be a list or data frame")
  }
  if (!is.character(pivot_cols) || length(pivot_cols) == 0) {
    stop("pivot_cols must be a non-empty character vector")
  }
  if (export && (is.null(file_path) || !is.character(file_path))) {
    stop("file_path must be provided when export = TRUE")
  }
  
  pivot_df <- function(df, cols) {
    if (!inherits(df, "data.frame")) return(df)
    
    available_cols <- cols[cols %in% names(df)]
    if (length(available_cols) == 0) return(df)
    
    non_pivot_cols <- setdiff(names(df), c(available_cols, "Value"))
    
    # Create pivot using tidyr
    pivoted <- tidyr::pivot_wider(df,
                                  id_cols = tidyselect::all_of(non_pivot_cols),
                                  names_from = tidyselect::all_of(available_cols),
                                  values_from = "Value",
                                  names_sep = ".",
                                  names_repair = name_repair)
    
    # Create hierarchical header matrix
    col_names <- colnames(pivoted)
    pivot_cols <- col_names[!col_names %in% non_pivot_cols]
    
    if (length(pivot_cols) > 0) {
      split_names <- strsplit(pivot_cols, "\\.")
      max_levels <- max(sapply(split_names, length))
      
      header_matrix <- matrix("", nrow = max_levels, ncol = length(col_names))
      
      # Fill non-pivot columns
      for (i in seq_along(non_pivot_cols)) {
        header_matrix[1, i] <- non_pivot_cols[i]
      }
      
      # Fill pivot columns
      for (i in seq_along(pivot_cols)) {
        parts <- split_names[[i]]
        col_idx <- which(col_names == pivot_cols[i])
        for (j in seq_along(parts)) {
          header_matrix[j, col_idx] <- parts[j]
        }
      }
      
      attr(pivoted, "header_matrix") <- header_matrix
      class(pivoted) <- c("hierarchical_pivot", class(pivoted))
    }
    
    return(pivoted)
  }
  
  # Process data object
  result <- if (is.data.frame(data_obj)) {
    pivot_df(data_obj, pivot_cols)
  } else {
    nested_result <- lapply(data_obj, function(nested) {
      if (is.list(nested)) {
        lapply(nested, function(df) pivot_df(df, pivot_cols))
      } else {
        pivot_df(nested, pivot_cols)
      }
    })
    class(nested_result) <- c("hierarchical_pivot_list", class(nested_result))
    nested_result
  }
  
  # Handle export if requested
  if (export) {
    export_hierarchy_to_excel(result, file_path)
  }
  
  return(result)
}


#' @title Export Hierarchical Pivot Table to Excel (Internal)
#'
#' @description Internal function to export hierarchical pivot tables to Excel with formatted headers.
#'
#' @param pivot_df A hierarchical pivot object created by pivot_data_hierarchy().
#' @param file_path Character. The file path for Excel export.
#' @param xlsx_filename Character. The name for the Excel file when using multi_sheet_xlsx. If NULL, uses the name of the dataset. Default is `NULL`.
#' 
#' @return Invisibly returns NULL.
#'
#' @keywords internal
#'
export_hierarchy_to_excel <- function(pivot_df, file_path, xlsx_filename = NULL) {
  if (is.null(xlsx_filename)) {
    xlsx_filename <- basename(tools::file_path_sans_ext(file_path))
  }
  
  wb <- openxlsx::createWorkbook()
  
  write_sheet <- function(df, sheet_name) {
    sheet_name <- make.names(substr(gsub("[^[:alnum:]]", "", sheet_name), 1, 31))
    
    openxlsx::addWorksheet(wb, sheet_name)
    
    if (inherits(df, "hierarchical_pivot")) {
      header_matrix <- attr(df, "header_matrix")
      if (!is.null(header_matrix)) {
        openxlsx::writeData(wb, sheet_name, header_matrix, startRow = 1, colNames = FALSE)
        openxlsx::writeData(wb, sheet_name, df, 
                            startRow = nrow(header_matrix) + 1, 
                            colNames = FALSE)
        
        header_style <- openxlsx::createStyle(textDecoration = "bold")
        openxlsx::addStyle(wb, sheet_name, style = header_style,
                           rows = 1:nrow(header_matrix),
                           cols = 1:ncol(header_matrix),
                           gridExpand = TRUE)
      } else {
        openxlsx::writeData(wb, sheet_name, df)
      }
    } else {
      openxlsx::writeData(wb, sheet_name, df)
    }
  }
  
  process_list <- function(lst, prefix = "") {
    for (name in names(lst)) {
      current_obj <- lst[[name]]
      current_name <- if (prefix == "") name else paste(prefix, name, sep = "_")
      
      if (inherits(current_obj, "hierarchical_pivot")) {
        write_sheet(current_obj, current_name)
      } else if (is.list(current_obj)) {
        for (subname in names(current_obj)) {
          if (inherits(current_obj[[subname]], "hierarchical_pivot")) {
            sheet_name <- paste(current_name, subname, sep = "_")
            write_sheet(current_obj[[subname]], sheet_name)
          }
        }
      }
    }
  }
  
  process_list(pivot_df)
  
  if (!grepl("\\.xlsx$", file_path)) {
    file_path <- paste0(file_path, "/", xlsx_filename, ".xlsx")
  } else {
    file_path <- gsub("data\\.xlsx$", paste0(xlsx_filename, ".xlsx"), file_path)
  }
  
  openxlsx::saveWorkbook(wb, file_path, overwrite = TRUE)
  
  invisible(NULL)
}