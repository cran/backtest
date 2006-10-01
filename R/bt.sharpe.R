################################################################################
##
## $Id: bt.sharpe.R 351 2006-10-02 00:22:20Z enos $
##
## Returns a one-dimensional array of sharpe ratios
##
################################################################################

.bt.sharpe <- function(object){

 stopifnot(object@natural)

 ## creates the array to store the sharpe ratios

 sr <- array(dim = c(1, length(object@in.var)),
             dimnames = list("sharpe", object@in.var))

 ## handles single or multiple "in.var"

 for(i in object@in.var){
   x <- object@results[object@ret.var, i, , ,"means"]
   spreads <- x[,"high"] - x[,"low"]
   sr[1,i] <- mean(spreads, na.rm = TRUE)/sd(spreads, na.rm = TRUE)
 }
 sr
}
