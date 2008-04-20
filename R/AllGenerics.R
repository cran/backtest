################################################################################
##
## $Id: AllGenerics.R 342 2006-10-01 05:02:50Z enos $
##
## Generic functions for the backtest class
##
################################################################################

if(!isGeneric("summaryStats"))
  setGeneric("summaryStats",
             function(object, ...) standardGeneric("summaryStats"))

if(!isGeneric("ci"))
  setGeneric("ci", function(object, ...) standardGeneric("ci"))

if(!isGeneric("turnover"))
  setGeneric("turnover", function(object, ...) standardGeneric("turnover"))

if(!isGeneric("means"))
  setGeneric("means", function(object, ...) standardGeneric("means"))

if(!isGeneric("counts"))
  setGeneric("counts", function(object, ...) standardGeneric("counts"))

if(!isGeneric("marginals"))
  setGeneric("marginals", function(object, ...) standardGeneric("marginals"))

if(!isGeneric("naCounts"))
  setGeneric("naCounts", function(object, ...) standardGeneric("naCounts"))
