################################################################################
##
## $Id: categorize.R 342 2006-10-01 05:02:50Z enos $
##
## Returns a factor corresponding to vector "x"
##
################################################################################

## Not sure if we will keep the is.date parameter here.

categorize <- function(x, n, is.date = FALSE){

  ## "new.factor" is a factor grouping "x" by "in.var". If x[[in.var]]
  ## is numeric, groups into "n" buckets

  ## We want to be able to use date-time classes as by.var's as well,
  ## so include a check for them here.
  
  if(is.numeric(x) && !is.date &&
     !inherits(x, "Date") && !inherits(x, "POSIXt")){
    new.factor <- factor(cut(x, breaks = quantile(x, seq(0, 1, 1/n),
                                  na.rm = TRUE),
                             include.lowest = TRUE, labels = FALSE))
  }
  else{
    new.factor <- factor(as.character(x), ordered = FALSE)
  }

  invisible(new.factor)
}
