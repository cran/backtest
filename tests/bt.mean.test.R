################################################################################
##
## $Id: bt.mean.test.R 342 2006-10-01 05:02:50Z enos $
##
## Tests for function "bt.mean"
##
################################################################################

library(backtest)

load("bt.mean.test.RData")

## save(x, truth, file = "bt.mean.test.RData", compress = TRUE)

stopifnot(
          isTRUE(all.equal(backtest:::.bt.mean(x), truth))
        )
