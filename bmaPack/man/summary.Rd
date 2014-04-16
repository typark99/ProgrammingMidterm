\name{summary}
\alias{summary}
\alias{summary,BMA-method}
\title{Summarizing outputs from fitBMA()}
\arguments{
  \item{object}{A \code{BMA} object}
}
\value{
  NULL
}
\description{
  Returns the output of the posterior expected coefficients
  and posterior probability that the coefficient is
  non-zero from fitBMA()
}
\note{
  This is a summary method for the class BMA
}
\examples{
set.seed(0520)
myY <- matrix(sample(1:20, 50, replace=TRUE), 50, 1)
myX <- matrix(c(runif(50), runif(50), rnorm(50), rnorm(50)), 50, 4)
summary(fitBMA(myY, myX))
}
\author{
  Taeyong Park
}

