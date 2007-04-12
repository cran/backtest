################################################################################
##
## $Id: backtest.function.R 398 2007-04-12 17:10:04Z enos $
##
## Returns an object of class backtest
##
################################################################################

## Returns a backtest object.  "x" is a data frame containing all raw
## data.  "in.var", "ret.var", "by.var", and "date.var" are character
## strings corresponding to column names in "x".  "in.var" and
## "ret.var" can be vectors containing multiple character strings.
## Only one "by.var" or "date.var" is allowed.  "date.var" currently
## acts the same as a "by.var".  "buckets" is a numeric specifying the
## number of buckets in which to group in.var and by.var,
## respectively.  "universe" is an expression for selecting a subset
## of "x".  "id.var" is only used in combination with "date.var"; it
## is a character string corresponding to a variable in "x" to be used
## to uniquely identify stocks across dates.

backtest <- function(x,
                     in.var,
                     ret.var,
                     universe,
                     by.var   = NULL,
                     date.var = NULL,
                     id.var   = NULL,
                     buckets  = 5,
                     natural  = FALSE){

  ## Corner Case: only one by.var allowed
  
  if(length(by.var) > 1){
    stop("Only one by.var allowed per backtest")
  }
  
  ## Corner Case: only one by.var allowed when using multiple ret.var

  if(length(ret.var) > 1 && (!is.null(by.var) || !is.null(date.var))){
    warning("Specifying by.var with multiple ret.vars is not supported. Proceed with caution.")
  }

  ## Must provide minimum of one in.var and one ret.var

  if(length(in.var) < 1 || length(ret.var) < 1){
    stop("At least one in.var and ret.var required")
  }
  
  ## Natural backtests must have dates and ids.
  
  if(natural && (is.null(date.var) || is.null(id.var))){
    stop("Must specify date.var and id.var for a natural backtest.")
  }

  ## in.var and ret.var columns must be numeric

  stopifnot(
            all(sapply(x[in.var], class) == "numeric"),
            all(sapply(x[ret.var], class) == "numeric")
            )

  ## Check "buckets"

  if(is.null(by.var)){
    buckets[2] <- 1
  }
  else{
    if(length(buckets) == 1){
      buckets[2] <- buckets[1]
    }
  }
 
  
  ## At a minimum, the length of in.var must be greater than the
  ## number of buckets.  Otherwise, we will get an error when we try
  ## to create more quantiles then there are observations.

  if(any(sapply(x[in.var], function(x){sum(!is.na(x))}) < (buckets[1] - 5))){
    stop("The number of non-NA in.var must be at least 5 more than the number of buckets.") 
  }
  
  ## The length of by.var must also be greater than the number of
  ## buckets to properly create quantiles
  
  if(!is.null(by.var) && sum(!is.na(x[by.var])) < (buckets[2] - 5)){
    stop("The number of non-NA by.var must be at least 5 more than the number of buckets.")
  }
  
  ## Save by.var for by.var slot
  
  by.specified <- by.var

  ## Use by.var or date.var?
  
  if(!is.null(date.var)){

    if(!is.null(by.var)){
      stop("Cannot specify both by.var and date.var.")
    }
    else{
      by.var <- date.var
    }
  }
  
  ## Evaluate "universe"
  
  if(!missing(universe)){
    univ <- eval(substitute(universe), x, parent.frame())
    univ <- univ & !is.na(univ)
    x <- x[univ,]
  }

  ## Function for bucketing NA values

  na.count <- function(x){
    return(sum(is.na(x)))
  }
  
  ## Attach "by.var" factor to "x"
  
  if(is.null(by.var)){
    x$by.factor <- rep(factor(1), times=nrow(x))
  }
  else{
    if(is.null(date.var)){
      x$by.factor <- categorize(x[[by.var]], n = buckets[2])
    }
    else{
      x$by.factor <- categorize(x[[by.var]], n = buckets[2], is.date = TRUE)
    }
  }

  
  ## Create array for storing turnover.  Dimensions signify:
  ## 1. date
  ## 2. in.var

  if(natural){
    turnover <- array(dim = c(length(levels(x$by.factor)), length(in.var)),
                      dimnames = list(levels(x$by.factor), in.var))
  }
  else
    turnover <- array()
  
  ## Construct by.var row names

  by.names <- levels(x$by.factor)
  if(!is.null(by.var) && is.numeric(x[[by.var]]) && is.null(date.var)){
    by.names[1] <- "low"
    by.names[length(by.names)] <- "high"
  }
  
  ## Construct in.var column names

  in.names <- 1:buckets[1]
  in.names[1] <- "low"
  in.names[length(in.names)] <- "high"

  ## Construct the empty array.  The dimensions are ordered as follows:
  ## 1: ret.var(s)
  ## 2: in.var(s)
  ## 3: by.var buckets
  ## 4: in.var buckets
  ## 5: means/counts/trim.means/NAs

  results <- array(dim = c(length(ret.var), length(in.var),
                     length(levels(x$by.factor)), buckets[1], 4),
                   dimnames = list(ret.var, in.var, by.names,
                     in.names, c("means", "counts", "trim.means", "NAs")))

  ## Construct ret.stats array.

  ret.stats <- array(dim = c(length(ret.var), 6), dimnames =
                     list(ret.var, c("min", "max", "mean", "median",
                                     "sd", "NA")))
  
  ## Select ret.var
  
  for(r in ret.var){

    ## Trim most extreme .5% of ret.var values
    
    trim.range <- quantile(x[[r]], c(0.0025, 0.9975), na.rm=TRUE)
    trim.x <- subset(x, trim.range[[1]] < x[[r]] &
                     x[[r]] < trim.range[[2]])

    ## Store ret.stats
    
    ret.stats[r,"min"] <- min(x[[r]], na.rm = TRUE)
    ret.stats[r,"max"] <- max(x[[r]], na.rm = TRUE)
    ret.stats[r,"mean"] <- mean(x[[r]], na.rm = TRUE)
    ret.stats[r,"median"] <- median(x[[r]], na.rm = TRUE)
    ret.stats[r,"sd"] <- sd(x[[r]], na.rm = TRUE)
    ret.stats[r,"NA"] <- sum(is.na(x[[r]]))

    ## Select in.var
    
    for(i in in.var){

      ## Construct "in.var" factors for "x" and "trim.x"

      in.factor <- categorize(x[[i]], n = buckets[1])
      trim.in.factor <- categorize(trim.x[[i]], n = buckets[1])

      if(length(levels(in.factor)) != buckets[1] ||
         length(levels(trim.in.factor)) != buckets[1]){
        stop("Encountered quantiles with no observations.  This can occur with very little data or very regular (usually synthesized) data.")
      }
      
      ## Bucketize means
      
      results[r,i, , ,"means"] <- bucketize(x[[r]], x.factor = in.factor,
                                            y.factor = x$by.factor,
                                            compute = mean, na.rm = TRUE)
      
      ## Bucketize counts
      
      results[r,i, , ,"counts"] <- bucketize(x[[r]], x.factor = in.factor,
                                             y.factor = x$by.factor,
                                             compute = length)
      
      ## Bucketize trim.means

      results[r,i, , ,"trim.means"] <- bucketize(trim.x[[r]], x.factor =
                                                 trim.in.factor, y.factor =
                                                 trim.x$by.factor, compute
                                                 = mean, na.rm = TRUE)

      ## Bucketize NAs

      results[r,i, , ,"NAs"] <- bucketize(x[[r]], x.factor = in.factor,
                                          y.factor = x$by.factor,
                                          compute = na.count)
      
      ## Calculate Turnover

      if(natural){
        turnover[, i] <- calc.turnover(x[[id.var]],
                                       portfolio.factor = in.factor,
                                       date.factor = x$by.factor)
      }
    }
  }
    
  ## Create and return backtest object
  
  invisible(new("backtest", in.var = in.var, ret.var = ret.var, by.var =
                as.character(by.specified), date.var = as.character(date.var),
                buckets = buckets, results = results, ret.stats = ret.stats,
                turnover = turnover, natural = natural))
}
