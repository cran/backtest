################################################################################
##
## $Id: calc.turnover.test.R 1227 2007-10-01 18:50:15Z enos $
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
