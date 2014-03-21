#' Plotting outputs from fitBMA()
#'
#' Returns the output of the posterior expected coefficients and posterior probability that the coefficient s non-zero from fitBMA()
#'
#' @param x A \code{BMA} object
#' 
#' @return NULL
#' @author Taeyong Park
#' @name plot
#' @examples
#' set.seed(0520)
#' myY <- matrix(sample(1:20, 50, replace=TRUE), 50, 1)
#' myX <- matrix(c(runif(50), runif(50), rnorm(50), rnorm(50)), 50, 4)
#' plot(fitBMA(myY, myX))
#' @note This is a plot method for the class BMA
#' @aliases plot,BMA-method
#' @rdname plot
#' @export
setMethod(f="plot",   # Since a generic function for plot is available, we do not set a generic function
          signature="BMA", # Clarify BMA class
          definition=function(x){  
            par(mfrow=c(1,2))
            plot(1:length(x@output$postCoef), x@output$postCoef, # This will plot postCoef for each coefficient
                 pch="+", col="darkred", cex=2,
                 xlim=c(0.5, length(x@output$postCoef)+0.5), # This is just for a better-looking
                 xlab="Coefficient", ylab="Posterior expected value",
                 main="The posterior expected value \n of coefficient")
            plot(1:length(x@output$probSig), x@output$probSig, # This will plot probSig for each coefficient
                 pch="+", col="darkred", cex=2,
                 xlim=c(0.5, length(x@output$probSig)+0.5), # This is just for a better-looking
                 xlab="Coefficient", ylab="Probability",
                 main="The posterior probability \n of non-zero coefficient")
          }
)