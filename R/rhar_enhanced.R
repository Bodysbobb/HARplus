rhar_enhanced <- function(con, useCoefficientsAsNames = FALSE, toLowerCase = TRUE, headersToRead = NULL) {
  # Open the file
  if (is.character(con)) {
    con <- file(con, 'rb')
  }
  
  # Read file into a raw vector
  cf <- raw()
  while (length(a <- readBin(con, raw(), n = 1e9)) > 0) {
    cf <- c(cf, a)
  }
  close(con)
  
  # Initialize headers list
  headers <- list()
  i <- 1
  while (i < length(cf)) {
    toRead <- readBin(cf[i:(i + 3)], 'integer', size = 4)
    
    if (toRead == 4) {
      header_name <- trimws(rawToChar(cf[(i + 4):(i + 3 + toRead)]))
      if (header_name != "") {
        headers[[header_name]] <- list(start = i)
      }
    }
    
    i <- i + 3 + toRead + 1
    hasRead <- readBin(cf[i:(i + 3)], 'integer', size = 4)
    if (hasRead != toRead) {
      warning(sprintf('Mismatch in record length at %d', i))
    }
    i <- i + 4
  }
  
  # Extract headers and binary data
  for (h in names(headers)) {
    headers[[h]]$binary <- cf[headers[[h]]$start:ifelse(h < length(headers), headers[[h + 1]]$start - 1, length(cf))]
  }
  
  # Process headers
  for (h in names(headers)) {
    headers[[h]]$type <- rawToChar(headers[[h]]$binary[5:10])
    headers[[h]]$description <- rawToChar(headers[[h]]$binary[11:80])
    headers[[h]]$numberOfDimensions <- readBin(headers[[h]]$binary[81:84], 'integer', size = 4)
    headers[[h]]$dimensions <- integer(0)
    
    for (i in seq_len(headers[[h]]$numberOfDimensions)) {
      headers[[h]]$dimensions <- c(headers[[h]]$dimensions, readBin(headers[[h]]$binary[(85 + (i - 1) * 4):(85 + i * 4)], 'integer', size = 4))
    }
  }
  
  # Read data based on header type
  for (h in names(headers)) {
    if (headers[[h]]$type == '2RFULL') {
      headers[[h]]$data <- array(readBin(headers[[h]]$binary[33:length(headers[[h]]$binary)], 'double', size = 4), dim = headers[[h]]$dimensions)
    } else if (headers[[h]]$type == '1CFULL') {
      raw_data <- rawToChar(headers[[h]]$binary[17:length(headers[[h]]$binary)], multiple = TRUE)
      headers[[h]]$data <- trimws(raw_data)
    }
  }
  
  # Convert header names to coefficients if required
  toRet <- lapply(headers, function(h) h$data)
  if (useCoefficientsAsNames) {
    names(toRet) <- sapply(headers, function(h) trimws(h$description))
  }
  
  if (toLowerCase) {
    names(toRet) <- tolower(names(toRet))
  }
  
  # Filter specific headers if provided
  if (!is.null(headersToRead)) {
    toRet <- toRet[names(toRet) %in% headersToRead]
  }
  
  return(toRet)
}
