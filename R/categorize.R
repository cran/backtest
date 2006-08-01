################################################################################
##
## $Id: $
##
## Returns a factor corresponding to vector "x"
##
################################################################################

categorize <- function(x, n, is.date = FALSE){

  ## "new.factor" is a factor grouping "x" by "in.var". If x[[in.var]]
  ## is numeric, groups into "n" buckets
    
  if(is.numeric(x) && !is.date){
    new.factor <- factor(cut(x, breaks = quantile(x, seq(0, 1, 1/n),
                                  na.rm = TRUE),
                             include.lowest = TRUE, labels = FALSE))
  }
  else{
    new.factor <- factor(as.character(x), ordered = FALSE)
  }

  invisible(new.factor)
}
