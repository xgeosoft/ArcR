% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/modal_value.R
\name{modal_value}
\alias{modal_value}
\alias{modal_value,vector-method}
\title{Modal Value (S4)}
\usage{
modal_value(x, na.rm = TRUE)

\S4method{modal_value}{vector}(x, na.rm = TRUE)
}
\arguments{
\item{x}{A \code{vector} object.}

\item{na.rm}{Logical, whether to remove missing values before computation. Defaults to \code{TRUE}.}
}
\value{
The most frequent (modal) value in the vector.
}
\description{
Computes the mode (most frequent value) of a \code{vector} object.
}
\section{Functions}{
\itemize{
\item \code{modal_value(vector)}: Method

}}
\examples{
v <- c(1, 2, 2, 3, 3, 3, 4)
modal_value(v)
}
