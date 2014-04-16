\name{fitBMA}
\alias{BMA,ANY-method}
\alias{fitBMA}
\title{Fitting BMA}
\arguments{
  \item{Y}{A matrix object; The number of columns is one;
  The number of rows depends on the data}

  \item{X}{A matrix object; The number of rows is the same
  as that of \code{Y}; The number of columns depends on the
  data}
}
\value{
  An object of class Regressions containing
  \item{output}{Output includes coefficients and the value
  of R.squared}
}
\description{
  Performs Bayesian Model Average given the linear
  regressions for every combination of the input covariates
  with no constant
}
\examples{
set.seed(0520)
myY <- matrix(sample(1:20, 50, replace=TRUE), 50, 1)
myX <- matrix(c(runif(50), runif(50), rnorm(50), rnorm(50)), 50, 4)
fitBMA(Y=myY, X=myX)
}
\author{
  Taeyong Park
}
\seealso{
  \code{\link{summary}}
}

