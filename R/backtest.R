################################################################################
##
## $Id: $
##
## Result object for a backtest
##
################################################################################

## "in.var", "ret.var", "by.var", and "date.var" are character
## strings, often used as labels by the accessor method.  Because the
## original data from which the backtest object was constructed is not
## preserved, they do not refer to actual variables.  "buckets" is a
## numeric vector of length 2, specifying the number of in.var and
## by.var buckets.  "results" is a five dimensional array storing the
## results of the backtest.

setClass("backtest", representation(in.var        = "character",
                                    ret.var       = "character",
                                    by.var        = "character",
                                    date.var      = "character",
                                    buckets       = "numeric",
                                    results       = "array",
                                    summary.stats = "data.frame",
                                    ret.stats     = "array",
                                    turnover      = "array",
                                    natural       = "logical"),
         
         prototype(in.var        = "in.var",
                   ret.var       = "ret.var",
                   by.var        = character(),
                   date.var      = character(),
                   buckets       = numeric(),
                   results       = array(dim = c(1,1,1,1,1)),
                   summary.stats = data.frame(),
                   ret.stats     = array(dim = c(1,6)),
                   turnover      = array(),
                   natural       = FALSE),

         validity = function(object){

           if(length(object@in.var) == 0){
             return("in.var not specified")
           }

           if(length(object@ret.var) == 0){
             return("ret.var not specified")
           }

           if(length(object@by.var) > 0 && length(object@date.var) > 0){
             return("Both by.var and date.var specified")
           }

           if(length(dim(object@results)) != 5){
             return("Incorrect number of dimensions in results slot")
           }

           return(TRUE)
         }
         )

## Lists the .vars used in the backtest

setMethod("show",
          signature(object = "backtest"),
          function(object){
            
            cat("An object of class backtest:\nin.vars: ",
                paste(object@in.var, collapse = ", "), " (buckets: ",
                object@buckets[1], ")\n",
                sep = "")
            
            if(length(object@by.var) > 0){
              cat("by.var: ", object@by.var, " (buckets: ",
                  object@buckets[2], ")\n", sep = "")
            }

            if(length(object@date.var) > 0){
              cat("date.var: ", object@date.var, " (buckets: ",
                  object@buckets[2], ")\n", sep = "")
            }
            
            cat("ret.vars: ", paste(object@ret.var, collapse = ", "),
                "\n", sep = "")
          }
          )

## Prints a header listing the .vars, then prints the table returned
## by the "spreads" method

setMethod("summary",
          signature(object = "backtest"),
          function(object, ...){
            
            ## Header
            
            cat("Backtest conducted with:\n\n",
                length(object@in.var), ifelse(length(object@in.var) == 1,
                                              " in.var: ", " in.vars: "),
                paste(object@in.var, collapse = ", "), ";\n",
                length(object@ret.var), ifelse(length(object@ret.var) == 1,
                                               " ret.var: ", " ret.vars: "),
                paste(object@ret.var, collapse = ", "), ";\nand ",
                ifelse(length(object@by.var) > 0,
                       paste("by.var: ", object@by.var, sep = ""), "no by.var"),
                ".\n\n", sep = "")

            ## Matrix

            print(summaryStats(object)[, !names(summaryStats(object))
                                       %in% c("CI(low)",
                                              "CI(high)",
                                              "TURNOVER")])
            cat("\n")

            if(object@natural){
              if(length(object@in.var) == 1){
                cat("average turnover: ", .bt.mean(turnover(object)),
                    "\nmean spread: ", mean(summaryStats(object)[, "spread"]),
                    "\nsd spread: ", sd(summaryStats(object)[, "spread"]),
                    "\nraw sharpe ratio: ", .bt.sharpe(object), "\n\n",
                    sep = "")
              }
              else{
                
                for(i in object@in.var){

                  x <- summaryStats(object)
                  
                  cat("summary stats for in.var = ", i,
                      ":\n\naverage turnover: ",
                      .bt.mean(turnover(object))[1,i],
                      "\nmean spread: ",
                      colMeans(summaryStats(object), na.rm = TRUE)[i],
                      "\nsd spread: ", sd(x[,i], na.rm = TRUE),
                      "\nraw sharpe ratio: ", .bt.sharpe(object)[,i],
                      "\n\n", sep = "")
                }
              }
            }
            
          }
          )


## Returns a matrix summarizing the results of the backtest.  The
## cells of the matrix contain means in cases 1, 2, and 4, and
## spreads in cases 3 and 5.  When a table of means is returned, the
## ".bt.spread" function is called and summary spread data is attached to
## the right side of the matrix.  If a date.var is used, ".bt.mean"
## is called and mean summary data are attached to bottom of the matrix.

setMethod("summaryStats",
          signature(object = "backtest"),
          function(object, mean = FALSE){

            num.in <- length(object@in.var)
            num.ret <- length(object@ret.var)

            if(length(object@by.var > 0)){
              by.present <- TRUE
              date.present <- FALSE
            }
            else{
              by.present <- FALSE
              if(length(object@date.var > 0)){
                date.present <- TRUE
              }
              else{
                date.present <- FALSE
              }
            }
            
            ## Case 1: 1 in.var, 1 ret.var
            
            if(num.in == 1 && num.ret == 1){

              output <- object@results[1,1, , ,"means"]

              n <- object@results[1,1, , ,"counts"]

              ## When one dimension of a 2D matrix has length 1,
              ## the subsets above return vectors. 
              
              if(is.null(dim(output))){
                output <- array(output, dim = c(1, length(output)),
                                dimnames = list("pooled", names(output)))

                n <- array(n, dim = dim(output),
                           dimnames = dimnames(output))
              }
              
              spread <- .bt.spread(output, n, object@ret.stats[1,"sd"])

              output <- cbind(output, spread)
              
              if(date.present){
                turnover <- object@turnover
                dimnames(turnover)[[2]] <- "TURNOVER"
                output <- cbind(output, turnover)
                
                output <- rbind(output, .bt.mean(output))
              }

            }

            ## Case 2: multiple in.vars, no by.var

            if(num.in > 1 && num.ret == 1 && !by.present &&
               !date.present){
              
              output <- object@results[1, ,1, ,"means"]

              n <- object@results[1, ,1, ,"counts"]

              spread <- .bt.spread(output, n, object@ret.stats[1,"sd"])
              
              output <- cbind(output, spread)
            }
            
            ## Case 3: multiple in.vars, with by.var or date.var

            if(num.in > 1 && num.ret == 1 && (by.present || date.present)){

              output <- t(object@results[1, , ,dim(object@results)[4],1] -
                          object@results[1, , ,1,1])

              if(date.present && mean){
                output <- rbind(output, .bt.mean(output))
              }
              
            }

            ## Case 4: single in.var, multiple ret.vars

            if(num.in == 1 && num.ret > 1){

              output <- object@results[ ,1,1, ,"means"]

              n <- object@results[ ,1,1, ,"counts"]
              
              spread <- .bt.spread(output, n, object@ret.stats[ ,"sd"])

              output <- cbind(output, spread)
            }

            ## Case 5: multiple in.vars and ret.vars

            if(num.in > 1 && num.ret > 1){
              
              output <- object@results[ , ,1,dim(object@results)[4],1] -
                object@results[ , ,1,1,1]
            }

            x <- data.frame(output)
            names(x) <- dimnames(output)[[2]]
            x
          }
          )

## Returns a list of matrices, with one matrix for each in.var,
## containing the means data.

setMethod("means",
          signature(object = "backtest"),
          function(object){

            mean.list <- list()

            for(i in object@in.var){
              mean.list <- append(mean.list,
                                  list(object@results[ ,i, , ,"means"]))
            }

            names(mean.list) <- object@in.var
            
            mean.list
          }
          )

## Returns a list of matrices, one matrix for each in.var, containing
## the counts data

setMethod("counts",
          signature(object = "backtest"),
          function(object){

            count.list <- list()
            
            for(i in object@in.var){
              count.list <- append(count.list,
                                   list(object@results[1,i, , ,"counts"]))
            }
            
            names(count.list) <- object@in.var

            count.list
          }
          )

## Same as the "counts" method, except that the total counts for each
## row and column are added to the margins.

setMethod("marginals",
          signature(object = "backtest"),
          function(object){

            body <- counts(object)

            for(i in 1:length(body)){
              
              if(is.null(dim(body[[i]]))){
                total <- sum(body[[i]], na.rm = TRUE)
                body[[i]] <- append(body[[i]], total)
                names(body[[i]])[length(body[[i]])] <- "TOTAL"
              }
              else{
                total <- rowSums(body[[i]], na.rm = TRUE)
                body[[i]] <- cbind(body[[i]], TOTAL = total)  
                
                total <- colSums(body[[i]], na.rm = TRUE)
                body[[i]] <- rbind(body[[i]], TOTAL = total)
              }
            }
           
            body
          }
          )

## Returns a list of matrices, one matrix for each in.var, containing
## the NAs data

setMethod("naCounts",
          signature(object = "backtest"),
          function(object){

            na.list <- list()

            for(i in object@in.var){
              na.list <- append(na.list, list(object@results[ ,i, , ,"NAs"]))
           } 
            
            names(na.list) <- object@in.var
            
            na.list
          }
          )

## Accessor Methods

## Returns the turnover for natural portfolios.  Passing a "mean"
## argument will append the mean of the turnover(s) as the last row of
## the matrix

setMethod("turnover",
          signature(object = "backtest"),
          function(object, mean = FALSE){
            
            if(!object@natural){
              stop("Cannot calculate turnover if not a natural backtest.")
            }

            if(mean){
              return(rbind(object@turnover, .bt.mean(turnover(object))))
            }

            object@turnover

          }
          )

## Returns the confidence intervals for each spread if for all cases
## except multiple in.var and a by.var or multiple in.var and multiple
## ret.var.  Must vectorize or loop to get these.

setMethod("ci",
          signature(object = "backtest"),
          function(object){
            
##          array(dim = c(1, length(object@ret.var), length(object@in.var)))

            if((length(object@in.var) == 1 && length(object@ret.var) == 1)
               || (length(object@in.var) == 1 && length(object@ret.var > 1))){
              
              summaryStats(object)[, c("CI(low)", "spread", "CI(high)")]
            }
            
          }
          )

## Plotting methods

setMethod("plot",
          signature(x = "backtest", y = "missing"),
          function(x, type = "return", ...){

            if(!x@natural){
              stop("Can only plot natural backtests")
            }

            if(type == "return"){
              btplot <- .plot.returns(x, ...)
            }
            else if(type == "turnover"){
              btplot <- .plot.turnover(x, ...)
            }
            else if(type == "cumreturn"){
              btplot <- .fanplot(x, ...)
            }
            else{
              stop("Unknown type specified.")
            }

            invisible(btplot)

          }
          )

## Plots turnover for each period specified by "date.var"

.plot.turnover <- function(object, main = "Backtest Turnover",
                          xlab = "Date", ylab = "Turnover", ...){
  
  turnovers <- object@turnover[-1,]

  turnovers <- as.data.frame(turnovers)

  datevar <- as.Date(rownames(turnovers))
  
  invars <- names(turnovers)[1]
  
  if(length(names(turnovers)) > 1){
    for(i in 2:length(names(turnovers))){
      invars <- paste(invars, "+",  names(turnovers)[i])
    }
  }
  
  form <- as.formula(paste(invars, "~", "datevar"))
  
  btplot <- xyplot(form, turnovers, type = "b", horizontal = FALSE,
                  auto.key = TRUE, scales = list(tick.number = 10),
          ylim = c(0, 1), main = main, xlab = xlab, ylab = ylab, ...)

  print(btplot)

  return(btplot)
}

## Plots returns for each period specified by "date.var"

.plot.returns <- function(object, main = "Backtest Returns",
                         xlab = "Date", ylab = "Spread of Mean Returns", ...){
  
  spreads <- object@results[1, , ,dim(object@results)[4],1] -
    object@results[1, , ,1,1]

  ## Fix for 1D array
  
  if(is.null(dim(spreads))){
    spreads <- array(spreads, dim = c(1, length(spreads)),
                     dimnames = list(object@in.var,
                       names(spreads)))
  }
  
  spreads <- data.frame(t(spreads))

  datevar <- rownames(spreads)
  
  invars <- names(spreads)[1]
  
  if(length(names(spreads)) > 1){
    for(i in 2:length(names(spreads))){
      invars <- paste(invars, "+",  names(spreads)[i])
    }
  }
  
  form <- as.formula(paste(invars, "~", "datevar"))
  
  btplot <- barchart(form, spreads, horizontal = FALSE, stack = FALSE,
                     auto.key = TRUE, scale = list(y = list(tick.number = 10),
                                        x = list(rot = 90)), origin = 0,
                     main = main, xlab = xlab, ylab = ylab, ...)
  
  print(btplot)

  return(btplot)
  
}

.fanplot <- function(object, main = "Backtest Fanplot", xlab = "Date",
                    ylab = "Cumulative Returns", ...){
  
  ## Function to compute cumsums of a matrix, by columns
  
  cum.matrix <- function(x){
    for(i in 1:dim(x)[2]){
      x[ ,i] <- cumsum(x[ ,i])
    }
    x
  }
    
  returns <- means(object)
  
  ## Calculate cumulative returns
  
  returns <- lapply(returns, cum.matrix)
  
  new.returns <- data.frame()
  
  for(i in 1:length(returns)){
    this.returns <- data.frame(returns[[i]])
    this.returns$group <- names(returns)[i]
    this.returns$date <- as.Date(rownames(this.returns))
    new.returns <- rbind(new.returns, this.returns)
  }

  invars <- names(new.returns)[1]
  
  for(i in 2:(length(names(new.returns)) - 2)){
    invars <- paste(invars, "+",  names(new.returns)[i])
  }
  
  form <- as.formula(paste(invars, "~ date | group"))
  
  btplot <- xyplot(form, new.returns, type = "b", horizontal = FALSE,
          auto.key = list(space = "right"), as.table = TRUE,
          main = main, xlab = xlab, ylab = ylab, ...)
  
  print(btplot)
  
  return(btplot)
  
}
