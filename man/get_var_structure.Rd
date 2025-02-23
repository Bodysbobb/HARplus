% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_summary.R
\name{get_var_structure}
\alias{get_var_structure}
\title{Get Variable Structure Summary from SL4 and HAR Objects}
\usage{
get_var_structure(variables = NULL, ..., include_col_size = FALSE)
}
\arguments{
\item{variables}{Character vector. Variable names to summarize. Use \code{NULL} or \code{"ALL"} to summarize all variables.}

\item{...}{One or more SL4 or HAR objects created using \code{load_sl4x()} or \code{load_harx()}.}

\item{include_col_size}{Logical. If \code{TRUE}, includes column and observation counts. Default is \code{FALSE}.}
}
\value{
A named list, where each element contains a data frame with:
\itemize{
\item \code{Variable}: The variable name.
\item \code{Dimensions}: The associated dimensions.
\item \code{DimSize}: The number of dimensions.
\item \code{DataShape}: The shape of the data (e.g., \verb{10x20x30}).
\item \code{No.Col}: (Optional) The number of columns.
\item \code{No.Obs}: (Optional) The number of observations.
}
}
\description{
Generates a summary of the variables within one or more SL4 or HAR objects, listing their
dimension sizes, structures, and optionally, column and observation counts.
}
\details{
\itemize{
\item Extracts dimension structures for variables in one or more SL4 or HAR datasets.
\item If \code{include_col_size = TRUE}, adds column and observation counts.
\item Supports multiple datasets and returns results as a named list, with each dataset’s summary stored separately.
\item Can summarize specific variables or \code{"ALL"}.
}
}
\examples{
# Import data sample:
sl4_data <- load_sl4x(system.file("extdata", "TAR10.sl4", package = "HARplus"))
sl4_data1 <- load_sl4x(system.file("extdata", "SUBT10.sl4", package = "HARplus"))

# Get summary for all variables in a single dataset
get_var_structure(data_obj = sl4_data)

# Get summary for specific variables
get_var_structure(c("gdp", "trade"), sl4_data)

# Include column and observation counts
get_var_structure("ALL", sl4_data, include_col_size = TRUE)

# Compare structures across multiple datasets
get_var_structure("ALL", sl4_data, sl4_data1)

# Include column and observation counts across multiple datasets
get_var_structure("ALL", sl4_data, sl4_data1, include_col_size = TRUE)

}
\seealso{
\code{\link{get_dim_patterns}}, \code{\link{get_dim_elements}}
}
\author{
Pattawee Puangchit
}
