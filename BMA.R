#' A Bayesian Modeling Average object 
#' 
#' Object of class \code{BMA} are created by the \code{fitBMA}, \code{summary} and \code{plot} functions
#'
#' 
#' An object of the class `BMA' has the following slots:
#' \itemize{
#' \item \code{Y} A depedent variable
#' \item \code{X} Covariates
#' \item \code{output} Output includes coefficients, R.squared, postOdds, postCoef, probSig
#' }
#'
#' @author Taeyong Park: \email{typark99@@gmail.com}
#' @aliases BMA-class initialize,BMA-method getBMA,BMA-method 
#' @rdname BMA
#' @export
setClass(Class="BMA", 
         slot = list(
           Y = "matrix", # Input
           X = "matrix", # Input
           output = "list" # Output: This will include the output from fitBMA() 
         ),
         prototype = prototype(
           Y = matrix(nrow=0, ncol=0), # An empty matrix
           X = matrix(nrow=0, ncol=0), # An empty matrix
           output = list() # An empty list
         )
)

#' @export
setMethod("initialize", "BMA", # We begin by initializing BMA class
          function(.Object, ...){
            value=callNextMethod()
            return(value)
          }
) 

#' @rdname BMA
#' @export 
setGeneric("getBMA", # setGeneric sets a generic function
           function(object="BMA")  {
             standardGeneric("getBMA")
           }
)

#' @export
setMethod(f="getBMA", # setMethod specifies the function getBMA()
          signature="BMA", 
          function(object){ 
            return(output=object@output)
          }
) 