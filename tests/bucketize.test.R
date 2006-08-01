################################################################################
##
## $Id: $
##
## Tests for function "bucketize"
##
################################################################################

library(backtest)

load("bucketize.test.RData")

## save(tmp.1, tmp.1.x, tmp.1.y, truth.1, file = "bucketize.test.RData", compress = TRUE)

result.1 <- backtest:::bucketize(tmp.1, tmp.1.x, tmp.1.y, compute = length)

stopifnot(
          isTRUE(all.equal(result.1, truth.1))
        )
