% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utility.R
\name{na_fill}
\alias{na_fill}
\title{Fill in NAs}
\usage{
na_fill(x)
}
\arguments{
\item{x}{Vector in which NAs should be removed}
}
\value{
Copy of x in which NAs have been replaced by nearest available value.
}
\description{
Replace NA entries in a vector by the last non-NA value. If the first
entry of the vector is NA, it is replaced by the first non-NA value. If the
vector passed as input doesn't contain NAs, it is returned as is.
}
