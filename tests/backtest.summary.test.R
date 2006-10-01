################################################################################
##
## $Id: backtest.summary.test.R 342 2006-10-01 05:02:50Z enos $
##
## Tests to make sure the summary method doesn't crash
##
################################################################################

library(backtest)

load("backtest.summary.test.RData")

## save(x, file = "backtest.summary.test.RData", compress = TRUE)

bt.1 <- backtest(x, in.var = "in.var.1", ret.var = "ret.var.1")
bt.2 <- backtest(x, in.var = "in.var.1", ret.var = "ret.var.1", by.var = "country")
bt.3 <- backtest(x, id.var = "id", in.var = "in.var.1", ret.var = "ret.var.1", date.var = "date")
bt.4 <- backtest(x, in.var = c("in.var.1", "in.var.2"), ret.var = "ret.var.1", by.var = "country")
bt.5 <- backtest(x, in.var = c("in.var.1", "in.var.2"), ret.var = c("ret.var.1", "ret.var.2"))
bt.6 <- backtest(x, in.var = c("in.var.1"), ret.var = c("ret.var.1", "ret.var.2"))

summary(bt.1)
summary(bt.2)
summary(bt.3)
summary(bt.4)
summary(bt.5)
summary(bt.6)
