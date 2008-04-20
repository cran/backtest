################################################################################
##
## $Id: bt.spread.test.R 342 2006-10-01 05:02:50Z enos $
##
## Tests for function "bt.spread"
##
################################################################################

library(backtest)

load("bt.spread.test.RData")

## save(m, n, sd, truth, file = "bt.spread.test.RData", compress = TRUE)

stopifnot(
          all(mapply(all.equal, backtest:::.bt.spread(m, n, sd), truth))
          )
