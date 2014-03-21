#' Returning output from fitBMA()
#'
#' Returns the output of the posterior expected coefficients and posterior probability that the coefficient s non-zero from fitBMA()
#'
#' @param x A \code{RegStats} object
#' 
#' @return NULL
#' @author Taeyong Park
#' @name summary
#' @examples
#' set.seed(0520)
#' myY <- matrix(sample(1:20, 50, replace=TRUE), 50, 1)
#' myX <- matrix(c(runif(50), runif(50), rnorm(50), rnorm(50)), 50, 4)
#' summary(fitBMA(myY, myX))
#' @note This is a summary method for the class BMA
#' @aliases summary,BMA-method
#' @rdname summary
#' @export
setMethod(f="print",  # Since print is a built-in function we do not set a generic function, and define the print function used under the Regressions class  
          signature="BMA",
          definition=function(x){
            cat("*** Start Summary (Class-BMA) ***", "\n", "\n")
            cat("1) Posterior expected value of each coefficient", "\n")
            for(i in 1:length(x@output$postCoef)){  
              cat("Coefficient", i, ":", x@output$postCoef[i], "\n") # Of the output of fitBMA(), postCoef indicates the posterior expected value of each coefficient
            }
            cat("2) The posterior probability that the coefficient is non-zero", "\n")
            for(i in 1:length(x@output$probSig)){ # for loop enables to separate each model 
              cat("Coefficient", i, ":", x@output$probSig[i], "\n") # Of the output of fitBMA(), probSig indicates The posterior probability that the coefficient is non-zero
            }
            cat("\n","**** End Summary (Class-BMA) ****")
          }
)