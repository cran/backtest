################################################################################
##
## $Id: bt.mean.R 1227 2007-10-01 18:50:15Z enos $
##
## Calculates the means by column and returns a
## 1 x length(x) array where the values are the means
## of the columns.
##
################################################################################

.bt.mean <- function(x){
  
  stopifnot(
            is.array(x)
            )

  x <- array(colMeans(x, na.rm = TRUE), dim = c(1, ncol(x)),
             dimnames = list("MEAN", dimnames(x)[[2]]))

  x
}

