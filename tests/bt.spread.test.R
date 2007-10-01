################################################################################
##
## $Id: bt.spread.test.R 1227 2007-10-01 18:50:15Z enos $
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
