################################################################################
##
## $Id: $
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

   short <- -colSums(x)[1]
   long <- colSums(x)[object@buckets[2]]

   sr[1,i] <- (short + long)/object@ret.stats[1,"sd"]
 }
 sr
}
