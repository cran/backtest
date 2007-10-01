################################################################################
##
## $Id: bt.mean.test.R 1227 2007-10-01 18:50:15Z enos $
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
