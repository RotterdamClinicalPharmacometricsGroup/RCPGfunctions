% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cov_IBW.R
\name{cov_IBW}
\alias{cov_IBW}
\title{Calculate ideal body weight}
\usage{
cov_IBW(sex, height)
}
\arguments{
\item{sex}{1 for male and 0 for female, or "M/m" for male and "F/f" for female}

\item{height}{numeric meters}
}
\value{
None
}
\description{
Calculate ideal body weight
}
\examples{
  cov_IBW("m", 1.77)

}
