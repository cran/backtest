\name{starmine}
\alias{starmine}
\docType{data}
\title{StarMine Rankings, 1995}
\description{
  StarMine rankings of some stocks in 1995, with corresponding returns
  and other data.
}
\usage{data(starmine)}
\format{
  A data frame containing 53328 observations on the following 10 variables.
  \describe{
    \item{\code{date}}{Date on which the observation was recorded.  The
      dates have a monthly frequency.  Dates range from 1995-01-31 to 1995-11-30.}
    \item{\code{id}}{Unique identifier for each stock.}
    \item{\code{name}}{Full company name.}
    \item{\code{country}}{Country of the exchange on which the
      company is listed.}
    \item{\code{sector}}{Sector to which the stock belongs.}
    \item{\code{cap.usd}}{Market capitalisation of the company in USD.}
    \item{\code{size}}{cap.usd normalized to N(0,1).}
    \item{\code{smi}}{StarMine Indicator (smi) score
      for each security and date if a score was issued.}
    \item{\code{fwd.ret.1m}}{1 month forward return.}
    \item{\code{fwd.ret.6m}}{6 month forward return.}
  }
}

\details{

\code{starmine} contains selected attributes such as sector, market
capitalisation, country, and various measures of return for a universe
of approximately 6,000 stocks.  The data is on a monthly frequency from
January 31, 1995 to November 30, 1995.

}

\note{We would like to thank StarMine Corporation for allowing us to
  include this data in the backtest package.
}

\source{
  StarMine Corporation.  For more information, see \url{http://www.starmine.com}.
}

\examples{
data(starmine)
head(starmine)
}
\keyword{datasets}
