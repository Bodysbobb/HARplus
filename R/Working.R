# Adding Unit Column ------------------------------------------------------


# Adding Unit Column (Value: all capitalization; otherwise, Percent)
add_unit_col <- function(data_list) {
  # Helper function to check if string is all uppercase
  is_all_caps <- function(x) {
    return(x == toupper(x))
  }
  
  # Process each dimension group
  result <- lapply(data_list, function(dim_group) {
    # Process each variable group within dimension
    processed_group <- lapply(dim_group, function(var_data) {
      if (is.null(var_data) || !is.data.frame(var_data)) return(var_data)
      
      # Check if "Variable" column exists
      if ("Variable" %in% names(var_data)) {
        # Get unique variables
        unique_vars <- unique(var_data$Variable)
        
        # Create unit mapping based on case
        unit_mapping <- sapply(unique_vars, function(x) {
          if (is_all_caps(x)) "Value" else "Percent"
        })
        
        # Add unit column based on Variable
        var_data$Unit <- unit_mapping[match(var_data$Variable, names(unit_mapping))]
      } else {
        # If no Variable column, assign unit based on group name
        var_name <- deparse(substitute(var_data))
        var_data$Unit <- if(is_all_caps(var_name)) "Value" else "Percent"
      }
      
      return(var_data)
    })
    
    # Maintain original names
    names(processed_group) <- names(dim_group)
    return(processed_group)
  })
  
  # Maintain original names and class
  names(result) <- names(data_list)
  class(result) <- class(data_list)
  
  return(result)
}

