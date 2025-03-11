#' @title Load and Process GEMPACK HAR Files (Internal)
#'
#' @description Reads a GEMPACK HAR file and efficiently extracts structured data while maintaining 
#' compatibility with standard HAR formats. This implementation builds upon the foundational 
#' work of the **HARr** package, reorganizing the process for improved execution speed, 
#' memory management, and handling of sparse data structures.
#'
#' @details
#' - **Efficient File Reading:** Reads large HAR files in chunks for better performance.
#' - **Optimized Memory Usage:** Reduces unnecessary allocations and improves cleanup.
#' - **Streamlined Header Processing:** Ensures accurate extraction of dimension metadata.
#' - **Supports Sparse Data Structures:** Handles `RESPSE` and `REFULL` headers efficiently.
#'
#' **Supported HAR Header Types:**
#' - `1CFULL`: Character headers
#' - `2IFULL`: Integer headers
#' - `2RFULL`: Real headers
#' - `REFULL`: Real headers with extended metadata
#' - `RESPSE`: Sparse real headers
#'
#' @param con Character or connection. The file path to the HAR file or an open binary connection.
#' @param coefAsname Logical. If `TRUE`, replaces four-letter headers with coefficient names when available. Default is `FALSE`.
#' @param lowercase Logical. If `TRUE`, converts all string values to lowercase. Default is `TRUE`.
#' @param select_header Character vector. Specific headers to extract; if `NULL`, reads all headers.
#'
#' @return A structured list where:
#' - Each element corresponds to a header in the HAR file.
#' - Names are either header names or coefficient names (if `coefAsname = TRUE`).
#' - Data maintains its original dimensions and attributes.
#'
#' @author Pattawee Puangchit
#' 
#' @seealso \code{\link{load_sl4x}}, \code{\link{load_harx}}
#'
#' @keywords internal
#'
load_harplus <- function(con, coefAsname = FALSE, lowercase = TRUE, select_header = NULL) {
  if(is.character(con)) {
    con <- file(con, 'rb')
    on.exit(close(con))
  }
  
  cf <- raw()
  while (length(chunk <- readBin(con, raw(), n=1e6)) > 0) {
    cf <- c(cf, chunk)
  }
  
  headers <- list()
  
  if (cf[1] == 0xfd) {
    currentHeader <- ""
    i <- 2
    while (i < length(cf)) {
      fb <- cf[i]
      i <- i + 1
      
      bitsLength <- as.integer(rawToBits(fb))[3:8]
      toRead <- as.integer(rawToBits(fb))[1:2]
      toReadBytes <- sum(2^((1:2)-1) * toRead)
      
      if (toReadBytes > 0) {
        bitsLength <- c(bitsLength, rawToBits(cf[i:(i + toReadBytes - 1)]))
        i <- i + toReadBytes
        i <- i + 1
      }
      
      recordLength <- sum(2^((1:length(bitsLength))-1) * bitsLength)
      
      if (recordLength == 4) {
        currentHeader <- trimws(rawToChar(cf[i:(i + recordLength - 1)]))
      }
      if (is.null(headers[[currentHeader]])) {
        headers[[currentHeader]] <- list()
      }
      if (is.null(headers[[currentHeader]]$records)) {
        headers[[currentHeader]]$records <- list()
      }
      
      headers[[currentHeader]]$records[[length(headers[[currentHeader]]$records) + 1]] <- 
        cf[i:(i + recordLength - 1)]
      
      i <- i + recordLength
      
      totalLength <- recordLength + 1 + toReadBytes
      endingBits <- intToBits(totalLength)
      maxPosition <- max(which(endingBits == 1))
      needEnd <- if(maxPosition <= 6) 0 else ceiling((maxPosition - 6) / 8)
      
      expectedEnd <- packBits(c(intToBits(needEnd)[1:2], 
                                intToBits(totalLength))[1:(8 * (needEnd + 1))], 'raw')
      expectedEnd <- rev(expectedEnd)
      
      if (!identical(cf[i:(i + length(expectedEnd) - 1)], expectedEnd)) {
        stop("Unexpected end of record")
      }
      i <- i + length(expectedEnd)
    }
  } else {
    i <- 1
    while (i < length(cf)) {
      toRead <- readBin(cf[i:(i + 3)], 'integer', size = 4)
      if (toRead == 4) {
        if (!all(cf[(i + 4):(i + 3 + toRead)] == 0x20)) {
          headerName <- trimws(rawToChar(cf[(i + 4):(i + 3 + toRead)]))
          headers[[headerName]] <- list(start = i)
        }
      }
      i <- i + 3 + toRead + 1
      hasRead <- readBin(cf[i:(i + 3)], 'integer', size = 4)
      i <- i + 4
    }
    
    for (h in names(headers)) {
      start <- headers[[h]]$start
      end <- if(h == tail(names(headers), 1)) length(cf) else headers[[names(headers)[which(names(headers) == h) + 1]]]$start - 1
      headers[[h]]$binary <- cf[start:end]
      
      headers[[h]]$records <- list()
      i <- 1
      while (i < length(headers[[h]]$binary)) {
        toRead <- readBin(headers[[h]]$binary[i:(i + 3)], 'integer', size = 4)
        i <- i + 4
        headers[[h]]$records[[length(headers[[h]]$records) + 1]] <- headers[[h]]$binary[i:(i + toRead - 1)]
        i <- i + toRead
        i <- i + 4
      }
    }
  }
  
  if(length(select_header) > 0) {
    headers <- headers[select_header]
  }
  
  for (h in names(headers)) {
    if (!is.null(headers[[h]])) {
      headers[[h]]$type <- rawToChar(headers[[h]]$records[[2]][5:10])
      headers[[h]]$description <- rawToChar(headers[[h]]$records[[2]][11:80])
      headers[[h]]$numberOfDimensions <- readBin(headers[[h]]$records[[2]][81:84], 'integer', size = 4)
      headers[[h]]$dimensions <- readBin(headers[[h]]$records[[2]][85:(85 + headers[[h]]$numberOfDimensions * 4 - 1)], 
                                         'integer', size = 4, n = headers[[h]]$numberOfDimensions)
    }
  }
  
  for (h in names(headers)) {
    if (headers[[h]]$type == '1CFULL') {
      contents <- Reduce(function(a, f) c(a, f[17:length(f)]), 
                         headers[[h]]$records[3:length(headers[[h]]$records)], c())
      contents[contents == 0x00] <- as.raw(0x20)
      m <- matrix(strsplit(rawToChar(contents), '')[[1]], 
                  nrow = headers[[h]]$dimensions[2], 
                  ncol = headers[[h]]$dimensions[1])
      
      headers[[h]]$data <- if(tolower(h) == 'xxhs') {
        apply(m, 2, paste, collapse = '')
      } else {
        trimws(apply(m, 2, paste, collapse = ''))
      }
      
      if(lowercase) {
        headers[[h]]$data <- tolower(headers[[h]]$data)
      }
    }
    
    if (headers[[h]]$type == '2IFULL') {
      contents <- Reduce(function(a, f) c(a, f[33:length(f)]), 
                         headers[[h]]$records[3:length(headers[[h]]$records)], c())
      headers[[h]]$data <- matrix(readBin(contents, 'integer', size = 4, 
                                          n = prod(headers[[h]]$dimensions)),
                                  nrow = headers[[h]]$dimensions[1],
                                  ncol = headers[[h]]$dimensions[2])
    }
    
    if (headers[[h]]$type == '2RFULL') {
      contents <- Reduce(function(a, f) c(a, f[33:length(f)]), 
                         headers[[h]]$records[3:length(headers[[h]]$records)], c())
      headers[[h]]$data <- array(readBin(contents, 'double', size = 4,
                                         n = prod(headers[[h]]$dimensions)),
                                 dim = headers[[h]]$dimensions)
    }
    
    if (headers[[h]]$type %in% c('REFULL', 'RESPSE')) {
      headers[[h]]$definedDimensions <- readBin(headers[[h]]$records[[3]][5:8], 'integer', size = 4)
      headers[[h]]$usedDimensions <- readBin(headers[[h]]$records[[3]][13:16], 'integer', size = 4)
      headers[[h]]$coefficient <- rawToChar(headers[[h]]$records[[3]][17:28])
      
      if (headers[[h]]$usedDimensions > 0) {
        dnames <- matrix(strsplit(rawToChar(headers[[h]]$records[[3]][33:(33 + headers[[h]]$usedDimensions * 12 - 1)]), '')[[1]],
                         nrow = 12)
        dnames <- apply(dnames, 2, paste, collapse = '')
        
        actualDimsNames <- headers[[h]]$records[[3]][(33 + headers[[h]]$usedDimensions * 12) + 0:6] == 0x6b
        uniqueDimNames <- unique(dnames[actualDimsNames])
        dimNames <- vector("list", headers[[h]]$usedDimensions)
        
        if (length(uniqueDimNames) > 0) {
          for (d in seq_along(uniqueDimNames)) {
            nele <- readBin(headers[[h]]$records[[3 + d]][13:16], 'integer', size = 4)
            m <- matrix(strsplit(rawToChar(headers[[h]]$records[[3 + d]][17:(17 + nele * 12 - 1)]), '')[[1]],
                        nrow = 12)
            
            for (dd in which(dnames == uniqueDimNames[d])) {
              dimNames[[dd]] <- trimws(apply(m, 2, paste, collapse = ''))
              names(dimNames)[dd] <- trimws(uniqueDimNames[d])
            }
          }
        }
        
        dataStart <- 3 + length(uniqueDimNames) + 1
        
        if (headers[[h]]$type == 'REFULL') {
          numberOfFrames <- readBin(headers[[h]]$records[[dataStart]][5:8], 'integer')
          numberOfDataFrames <- (numberOfFrames - 1) / 2
          dataFrames <- (dataStart) + 1:numberOfDataFrames * 2
          
          dataBytes <- Reduce(function(a, f) c(a, headers[[h]]$records[[f]][9:length(headers[[h]]$records[[f]])]), 
                              dataFrames, c())
          
          headers[[h]]$data <- array(readBin(dataBytes, 'double', size = 4,
                                             n = prod(headers[[h]]$dimensions)),
                                     dim = headers[[h]]$dimensions[1:headers[[h]]$usedDimensions],
                                     dimnames = dimNames)
        } else {
          elements <- readBin(headers[[h]]$records[[dataStart]][5:8], 'integer', size = 4)
          dataVector <- numeric(prod(headers[[h]]$dimensions))
          
          for (rr in (dataStart + 1):length(headers[[h]]$records)) {
            dataBytes <- headers[[h]]$records[[rr]][17:length(headers[[h]]$records[[rr]])]
            currentPoints <- length(dataBytes) / 8
            
            locations <- readBin(dataBytes[1:(4 * currentPoints)], 'integer', size = 4,
                                 n = currentPoints)
            values <- readBin(dataBytes[(4 * currentPoints + 1):(8 * currentPoints)],
                              'double', size = 4, n = currentPoints)
            
            dataVector[locations] <- values
          }
          
          headers[[h]]$data <- array(dataVector,
                                     dim = headers[[h]]$dimensions[1:headers[[h]]$usedDimensions],
                                     dimnames = dimNames)
        }
      } else {
        lastRecord <- headers[[h]]$records[[length(headers[[h]]$records)]]
        headers[[h]]$data <- array(readBin(lastRecord[9:length(headers[[h]]$records[[3]])],
                                           'double', size = 4,
                                           n = prod(headers[[h]]$dimensions)),
                                   dim = headers[[h]]$dimensions[1:headers[[h]]$usedDimensions])
      }
    }
  }
  
  result <- lapply(headers, function(f) {
    data <- f$data
    if (lowercase && !is.null(dimnames(data))) {
      names(dimnames(data)) <- tolower(names(dimnames(data)))
      dimnames(data) <- lapply(dimnames(data), tolower)
    }
    data
  })
  
  if (coefAsname) {
    for (h in names(headers)) {
      if (!is.null(headers[[h]]$coefficient)) {
        names(result)[which(names(result) == h)] <- trimws(headers[[h]]$coefficient)
      }
    }
  }
  
  if (lowercase) {
    names(result) <- tolower(names(result))
  }
  
  result
}