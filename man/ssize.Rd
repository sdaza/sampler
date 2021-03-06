% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/ssize.R
\name{ssize}
\alias{ssize}
\title{Estimation of sample sizes for a proportion}
\usage{
ssize(e, deff = 1, rr = 1, N = NULL, cl = 0.95, p = 0.5)
}
\arguments{
\item{e}{Expected margin of error.}

\item{deff}{Design effect (DEff). Generally, from studies and estimations. Values should be higher than 0.}

\item{rr}{Proportion representing expected response rate, it migth higher than 0 and lower than 1.}

\item{N}{Population size. If the population size is not specified, MOEs are estimated using an infinite population formula.}

\item{cl}{Confidence levels, e.g., .95}

\item{p}{Value or vector of values with the expected proportion.}
}
\description{
This function provides flexibility to estimate margins of errors of a proportion assuming different population sizes, confidence levels, design effects (DEff), and response rates.
}
\examples{
ssize(e = 0.02, deff = 1.5, rr = .8, N = 1000)
ssize(e = 0.05)
}

