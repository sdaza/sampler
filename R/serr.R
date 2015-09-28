#' @title Estimation of margins of errors (MOEs) for a proportion
#' @description This function provides flexibility to estimate margins of errors for a proportion assuming different population sizes, confidence levels, design effects (DEff), response rates. 
#' @param n Size of the sample.
#' @param p Value or vector of values with the expected proportion of each stratum.
#' @param deff Design effect (DEff) provided from previous studies or design estimations (e.g., 1.5).
#' @param rr Proportion representing expected response rate, it migth be 0 or more, and less than 1.
#' @param N Population size. If the population size is not specified, MOEs are estimated using the infinite population formula.
#' @param cl Confidence levels, e.g., .95
#' @param relative Logical parameter. Estimate a relative error if TRUE.
#' @examples
#' serr(n = 400, deff = 1.5, rr = .8, N = 1000)
serr <- function(n, deff = 1, rr = 1, N = NULL, cl = .95, p = 0.5, relative = FALSE)  {
 
# validation
 
if (sum(n==0)>=1) { 
  stop("n vector contains 0 values")
}
 
if (sum(deff<=0)>=1) { 
  stop("deff should be bigger than 0")
}
 
if (sum(rr<=0)>=1) { 
  stop("rr should be bigger than 0")
}
 
# get z value 
z <- round(abs(qnorm(( 1 - cl)/2)), 2)
 
# computation of sampling error 
if (is.null(N)) {
 
  n <- (n/deff)*rr
  e <- z*sqrt((p*(1-p))/n)
  e <- round(e, digits=4)
 
  if (relative==TRUE) {
    return(round(e/p, digits=4))
    }
  else {
    return(e)
    }
  }
 
else  {
 
  if (sum(N==0)>0) { 
  stop("N vector contains 0 values")
  }
 
  if (sum(n>N)>0) { 
  stop("n is bigger than N")
  }
 
  n <- (n/deff)*rr
  e <- z * sqrt((p * (1 - p)) / n) * sqrt(( N - n)/( N - 1))
  e <- round(e, digits=4)
 
  }
 
  if (relative == TRUE) {
  return(round(e / p, digits = 4))
  }
 
  else {
  return(e)
  }
 
}