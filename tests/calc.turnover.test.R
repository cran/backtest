################################################################################
##
## $Id: calc.turnover.test.R 342 2006-10-01 05:02:50Z enos $
##
## Tests for function "calc.turnover"
##
################################################################################

library(backtest)

load("calc.turnover.test.RData")

## save(x.id, x.bucket, x.date, x.truth, file = "calc.turnover.test.RData", compress = TRUE)

x.result <- backtest:::calc.turnover(x.id, x.bucket, x.date)

stopifnot(
          isTRUE(all.equal(x.result, x.truth))
          )
