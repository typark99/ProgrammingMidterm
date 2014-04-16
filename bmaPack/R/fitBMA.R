#' Fitting BMA
#'
#' Performs Bayesian Model Average given the linear regressions for every combination of the input covariates with no constant
#'
#' @param Y A matrix object; The number of columns is one; The number of rows depends on the data 
#' @param X A matrix object; The number of rows is the same as that of \code{Y}; The number of columns depends on the data
### This documentation has several problems.  First, these are not all of the slots the function takes.  Second,
### the output is not specific enough and odes not match up with the actual output.
#'  
#' @return An object of class Regressions containing
#'  \item{output}{Output includes coefficients and the value of R.squared}
#' @author Taeyong Park
#' 
#' @examples
#' set.seed(0520)
#' myY <- matrix(sample(1:20, 50, replace=TRUE), 50, 1) 
#' myX <- matrix(c(runif(50), runif(50), rnorm(50), rnorm(50)), 50, 4)
#' fitBMA(Y=myY, X=myX)
#' @seealso \code{\link{summary}}
#' @rdname fitBMA
#' @aliases BMA,ANY-method  
#' @export
setGeneric(name="fitBMA",  # setGeneric sets a generic function
           def=function(Y, X, ...)
           {standardGeneric("fitBMA")}
)

#' @export
setMethod(f="fitBMA",  # setMethod specifies the function fitBMA()
          definition= function(Y, X, g=3, parallel=FALSE, ...){ # g=3 is default for the calculation of the posterior expected value; parallel=FALSE is default for running function in parallel
            Y <- (Y-mean(Y))/sd(Y) # Standardize dependent variable
            ## This is not right.  Need to do the standardization by column (each variable standardized to itself rather than the whole matrix.
            X <- (X-mean(X))/sd(X) # Standardize covariates
            data <- data.frame(cbind(Y,X)) # Create a data set
            p <- ncol(X) # p indicates the number of covariates of the model under consideration
            id <- unlist(lapply(1:p, function(z) combn(1:p, z, simplify=F)), recursive=FALSE) # This ensures that Z will contain every combination of the covariates.
            colNam <- names(data)[-1] # Define the name of columns of data
            ## The above makes the names start with X2

            
## This should go into the DESCRIPTION file
            require(plyr)

            ## Can use the regression 
            formula <- llply(id, function(f) paste(names(data)[1],"~", paste(colNam[f], collapse="+"),"-1", sep=""), .parallel=parallel) # formula will contain every combination of regressions. It is important to include "-1" to ensure that we will drop the intercept when we run regressions using this formula 
            formula[[2^p]] <- paste(names(data)[1],"~", "1", sep="")   # The last element of formula will be the null model (i.e., The model including only the intercept) Since we standardized the data input, the regression coefficient of this intercept will be zero
            beta <- matrix(NA, nrow=length(formula), ncol=ncol(X))   # This will contain the coefficients. Since we will not include the intercept, the number of columns of this matrix is the same as the number of colums of the data
            colnames(beta) <- c(names(data)[-1]) 
            R2 <- eBetaModel <- bayesF <- numeric() # Empty numerics for several statistics 
            for (i in 1:length(formula)){ # This for loop ensures that we run regressions of every possible combination
              fit = lm(formula(formula[[i]]), data)  
              coefficients <- coef(fit) 
              beta[i, colnames(beta) %in% names(coefficients)] <- coefficients[names(coefficients) %in% colnames(beta)] # The rows of beta contain coefficients for each iteration
              R2[i] <- summary(fit)$r.squared # This returns the value of R^2
            }
            
            for (k in 1:length(formula)){
              p=p  # p indicates the number of covariates of the model under consideration
              n=nrow(X)  # n indicates the number of rows of input data for explnatory variables
              bayesF[k] <- (1+g)^((n-p-1)/2)*(1+g*(1-R2[k]))^(-(n-1)/2) # This returns Bayes's factor for the models; This is the posterior model odds for each model
            }

## Loops.  No apply.  Does not work in parallel
            
            beta[is.na(beta)] <- 0 # This is to make it possible to caluclate eBetaModel below by replacing NA with 0
            eBetaModel <- (g/(g+1))*beta # This returns E(\beta_j|M_k) from Slide 3
            postModel <- bayesF/sum(bayesF) # Posterior probability of the model; The total weight assigned to all models that include each variable; This gives us the posterior probability that the coefficient is non-zero
            postCoef <- postModel*eBetaModel # Posterior expected value of each coefficient

            ## Nope.  Should be matrix multiplication

            
            output <- list(coefficients, R2, bayesF, postCoef, postModel)  
            names(output) <- c("coefficients", "R.squared", "postOdds", "postCoef", "probSig")  # postOdds from bayesF; probSig from postModel
            return((new("BMA", Y=Y, X=X, output=output)))
          }
)


