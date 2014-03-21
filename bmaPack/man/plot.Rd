\docType{methods}
\name{plot}
\alias{plot}
\alias{plot,BMA-method}
\title{Plotting outputs from fitBMA()}
\usage{
\S4method{plot}{BMA}(x)
}
\arguments{
  \item{x}{A \code{BMA} object}
}
\description{
Returns the output of the posterior expected coefficients
and posterior probability that the coefficient s non-zero
from fitBMA()
}
\note{
This is a plot method for the class BMA
}
\examples{
set.seed(0520)
myY <- matrix(sample(1:20, 50, replace=TRUE), 50, 1)
myX <- matrix(c(runif(50), runif(50), rnorm(50), rnorm(50)), 50, 4)
plot(fitBMA(myY, myX))
}
\author{
Taeyong Park
}

