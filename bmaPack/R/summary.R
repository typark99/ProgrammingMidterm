#' Summarizing outputs from fitBMA()
#'
#' Returns the output of the posterior expected coefficients and posterior probability that the coefficient is non-zero from fitBMA()
#'
#' @param object A \code{BMA} object
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
setMethod(f="summary",  # Since a generic function for summary is available, we do not set a generic function
          signature="BMA", # Clarify BMA class
          definition=function(object){ # "object" is from fitBMA() under the BMA class
            cat("*** Start Summary (Class-BMA) ***", "\n", "\n")
            cat("1) Posterior expected value of each coefficient", "\n")
            for(i in 1:length(object@output$postCoef)){    
              cat("Coefficient", i, ":", round(object@output$postCoef[i],4), "\n") # Of the output of fitBMA(), postCoef indicates the posterior expected value of each coefficient
            }
            cat("\n", "2) Posterior probability that the coefficient is non-zero", "\n", sep="")
            for(i in 1:length(object@output$probSig)){ # for loop enables to separate each model 
              cat("Coefficient", i, ":", round(object@output$probSig[i],4), "\n") # Of the output of fitBMA(), probSig indicates The posterior probability that the coefficient is non-zero
            }
            cat("\n","**** End Summary (Class-BMA) ****", sep="")
          }
)