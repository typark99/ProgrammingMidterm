## Load libraries and set working directory
library(devtools)
library(roxygen2)
setwd("~/GitHub/ProgrammingMidterm") # Set my working directory

## At this point put the *.R files into the correct directories and edit the DESCRIPTION file

## The R directory is in this order:

# BMA.R
# bmaPack.r
# fitBMA.R
# plot.R
# summary.R
# z.exampleData.R


## This can be run many times as the code is updates
current.code <- as.package("bmaPack")
load_all(current.code) # Load all of the functions so you can use them
document(current.code) # Make the help files
check(current.code) # Run the R checks
install(pkg=current.code, local=TRUE) # Install the package

