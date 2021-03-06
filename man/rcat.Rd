% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rcat.R
\name{rcat}
\alias{rcat}
\title{Random categorical variables}
\usage{
rcat(n, levels, p)
}
\arguments{
\item{n}{number of samples}

\item{levels}{vector of categories to sample from}

\item{p}{vector of probabilities}
}
\value{
a \code{character} vector
}
\description{
Random categorical variables
}
\examples{
#' rcat(n=10, levels=c("a","b"), p=c(0.2,0.8))
}
