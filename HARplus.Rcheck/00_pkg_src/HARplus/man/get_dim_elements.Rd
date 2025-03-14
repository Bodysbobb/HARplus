% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_summary.R
\name{get_dim_elements}
\alias{get_dim_elements}
\title{Get Dimension Elements from SL4 and HAR Objects}
\usage{
get_dim_elements(..., keep_unique = FALSE)
}
\arguments{
\item{...}{One or more structured SL4 or HAR objects containing dimension information.}

\item{keep_unique}{Logical. If \code{TRUE}, returns only unique dimension elements across inputs. Default is \code{FALSE}.}
}
\value{
A data frame containing unique dimension elements.
}
\description{
Extracts and lists unique dimension elements (e.g., \code{REG}, \code{COMM}, \code{ACTS}) from one or more datasets.
}
\examples{
# Import sample data:
sl4_data1 <- load_sl4x(system.file("extdata", "TAR10.sl4", package = "HARplus"))
sl4_data2 <- load_sl4x(system.file("extdata", "SUBT10.sl4", package = "HARplus"))


# Extract dimension elements from a single dataset
get_dim_elements(sl4_data1)

# Extract dimension elements from multiple datasets
get_dim_elements(sl4_data1, sl4_data2)

# Extract unique dimension elements across datasets
get_dim_elements(sl4_data1, sl4_data2, keep_unique = TRUE)

}
\seealso{
\code{\link{get_dim_patterns}}, \code{\link{get_var_structure}}
}
\author{
Pattawee Puangchit
}
