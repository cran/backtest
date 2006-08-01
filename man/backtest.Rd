\name{backtest}
\alias{backtest}
\title{Creating an Object of Class Backtest}
\description{
  Conducts a backtest and returns the results as an object of class \code{backtest}.
}
\usage{
backtest(x, in.var, ret.var, by.var = NULL, date.var = NULL,
 id.var = NULL, buckets = 5, universe = NULL, natural = FALSE)
}
\arguments{
  \item{x}{a data frame containing the data to be analysed in the
    backtest.  The details of what this data frame must contain are given below.}
  \item{in.var}{a character vector which indicates the name of the
    column or columns in 'x' to be used as input variables.}
  \item{ret.var}{a character vector which indicates the name of the
    column or columns in 'x' to be used as return variables.}
  \item{by.var}{an optional character value, specifying a second
    variable in 'x' to be used for categorising the data.  The details
    of how categories are created are given below.}
  \item{id.var}{an optional character value which indicates the name of
  the column in 'x' containing a unique identifier for each
  observation.  'id.var' must be specified if 'natural' equals TRUE.}
  \item{date.var}{an optional character vector which indicates the name
    of the column in 'x' containing values of class 'Date' for the
    observations. 'date.var' must be specified if 'natural' equals TRUE.}
  \item{buckets}{an optional numeric vector which specifies how many
    quantiles to create according to 'in.var' and 'by.var'.}
  \item{universe}{An optional expression for selecting a subset of 'x'.
    The details of how this expression may be constructed are given below.
  }
  \item{natural}{An optional logical value.  If 'TRUE', the 'summary()'
    method returns additional information and the backtest object may be
    plotted.  The details of how a natural backtest differs from a pooled
    backtest are given below.
  }
}

\details{

Data frames for 'backtest' must, at a minimum, contain a column of class
numeric to be referenced by the 'in.var' and 'ret.var' arguments.  

The 'in.var' is the primary variable by which the backtest categorises
observations.  It must reference a numeric column in 'x'.  Using the
values in 'x', 'backtest' breaks the values into equal sized
quantiles, or 'buckets'.

The 'by.var' is the secondary variable by which the backtest categorises
observations.  When specifying both 'in.var' and 'by.var', 'backtest'
organises the observations into a 'n' by 'j' matrix where 'n' is the
number of quantiles or categories created for the 'by.var' and 'j' is
the number of quantiles created for the 'in.var'.  By default,
'backtest' creates 5 quantiles.

If 'natural' is TRUE, the data and arguments must meet certain
requirements.  First, the frequency of the observations and 'ret.var'
must be the same.  Second, a \code{id.var} and \code{date.var} are
required.  Third, a \texttt{by.var} is not allowed.  Note that the code
does not verify that the backtest is truly natural; \code{backtest}
accepts the value passed by the user as valid.

}

\value{
  'backtest' returns an object of class \code{backtest}.

  
  The functions \code{show} and \code{summary} are used to obtain and
  print a short description and longer summary of the results of the
  \code{backtest}.  The accessor functions \code{counts},
  \code{marginals}, \code{mean}, \code{naCounts}, and \code{turnover}
  extract different parts of the value returned by \code{backtest}.
}

\seealso{\code{\link{backtest-class}}}

\author{Kyle Campbell \email{kyle.w.campbell@williams.edu}}
\examples{

data(starmine)

## running a backtest with 1 'in.var' and 1 'ret.var'

bt <- backtest(starmine, in.var = "smi", ret.var = "ret.0.1.m")
summary(bt)

## running a backtest with 2 'in.var' values, 1 'ret.var', and a 'by.var'

bt <- backtest(starmine, in.var = c("smi", "cap.usd"), ret.var =
"ret.0.1.m", by.var = "sector")
summary(bt)

## running a backtest with 1 'in.var', 1 'by.var', and 1 'ret.var'.
## Number of buckets changed from default of 5 to 4.  Change in number
## of buckets only affects the 'in.var' because the 'by.var' column in
## 'starmine' contains character data. For each value in this column
## there is a unique category.

bt <- backtest(starmine, in.var = "smi", by.var = "sector", ret.var =
"ret.0.1.m", buckets = 4)
summary(bt)

## running a backtest with 1 'in.var', multiple 'ret.var', and a
## universe restriction

bt <- backtest(starmine, in.var = "smi",
ret.var = c("ret.0.1.m", "ret.0.6.m", "ret.1.12.m"),
universe = starmine$sector == "HiTec")
summary(bt)

## running a natural backtest with 1 'in.var', multiple 'ret.var' and 10
##buckets

bt <- backtest(starmine, in.var = "smi", ret.var = "ret.0.1.m",
date.var = "date", id.var = "id", buckets = 10, natural = TRUE)
summary(bt)

## draws a plot of turnover

plot(bt, type = "turnover")

## draws a plot of monthly spreads

plot(bt, type = "return")

## draws a fanplot of returns

plot(bt, type = "cumreturn")

}
\keyword{file}
