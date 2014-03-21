\docType{class}
\name{BMA-class}
\alias{BMA-class}
\alias{getBMA}
\alias{getBMA,BMA-method}
\alias{initialize,BMA-method}
\title{A Bayesian Modeling Average object}
\usage{
getBMA(object = "BMA")
}
\description{
Object of class \code{BMA} are created by the
\code{fitBMA}, \code{summary} and \code{plot} functions
}
\details{
An object of the class `BMA' has the following slots:
\itemize{ \item \code{Y} A depedent variable \item \code{X}
Covariates \item \code{output} Output includes
coefficients, R.squared, postOdds, postCoef, probSig }
}
\author{
Taeyong Park: \email{typark99@gmail.com}
}

