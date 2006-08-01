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
  A data frame containing 57767 observations on the following 13 variables.
  \describe{
    \item{\code{date}}{Date on which the observation was recorded.  The
      dates have a monthly frequency.  Dates range from 1995-01-31 to 1995-11-30.}
    \item{\code{id}}{Unique identifiers for each stock.}
    \item{\code{country}}{Country of the exchange on which the
      company is listed.}
    \item{\code{sector}}{Sector to which the stock belongs}
    \item{\code{name}}{Full name of the company}
    \item{\code{cap.usd}}{Market capitalisation of the company in US dollars}
    \item{\code{size}}{Market capitalisation normalized to N(0,1).}
    \item{\code{smi}}{contains the StarMine Indicator (SMI) score
      for each security and date if a score was issued.}
    \item{\code{ret.0.1.m}}{1 month forward return.}
    \item{\code{ret.0.5.d}}{5 day forward return.}
    \item{\code{ret.15.45.d}}{Return from day 15 through day 45.}
    \item{\code{ret.0.6.m}}{6 month forward return.}
    \item{\code{ret.1.12.m}}{Cumulative return from month 1 through
      month 12.}
  }
}

\details{

\code{starmine} contains selected attributes such as sector, market
capitalisation, country, and various measures of return for a universe
of approximately 6,000 stocks.  The data is on a monthly frequency from
January 31, 1995 to November 30, 1995.

Columns containing total returns, such as \code{ret.0.1.m} and
\code{ret.15.45.d}, have names of the form \code{ret.n.m.u}, where
\code{n} is the start instant, \code{m} is the end instant, and
\code{u} specifies the instants' calendar units.  The instants
\code{n} and \code{m} are relative to the end of the period
expressed by the observation date.  For example, the one month forward
return \code{ret.0.1.m} for date \code{1995-11-30} should be read
"the total return from 0 months from \code{1995-11-30} to 1 month from
\code{1995-11-30}", that is, the return for the month of December, 1995.
Similarly, \code{ret.15.45.d} for date 1995-11-30 contains the
30-calendar-day return from the close of business on \code{1995-12-15} to
close of business on \code{1996-01-14}.

}

\note{We would like to thank StarMine Corporation for allowing us to use
  their data.
}

\author{
Documentation contributed by Daniel Gerlanc.
}

\source{
\url{http://www.starmine.com}
}

\examples{
data(starmine)

head(starmine)
}
\keyword{datasets}
