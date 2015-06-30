#' @title Stratum allocation
#' @description This function allocates sample size to strata using different methods. 
#' @param N Population size. If the population size is not specified, MOEs are estimated using the infinite population formula.
#' @param method The allocation can be assigned using different methods. The default method is \strong{mixed}. 
#' \itemize{
#'   \item \strong{mixed}: Combine equal size  across strata (equal sample size by stratum) and proportional allociation. It needs to specify the weight of the proportional assignation by \code{wp}. If \code{wp = 1} (default value), the strata allocation is proportional. If \code{wp = 0}, the strata allocation is equal. If \code{wp} is any number between 0 and 1, the strata allocation will be the corresponding combination between equal size and proportionality across strata.
#'   \item \strong{error}: The sample size of strata is based on a margin of error. \code{e} needs to be specified.
#'   \item \strong{neyman}: It uses Neyman allocation. It assigns sample units within each stratum proportional to the product of the population stratum size and the within-stratum standard deviation, so that minimum variance for a population mean estimator can be achieved.
#'   \item \strong{root}: It proportionally assigns sample units using the square root of the population sizes. 
#'   \item \strong{stdev}: It assigns sample units proportional to the standard deviation across strata. 
#' }
#' @param wp Weight given to a proportional allocation. This is only need when method is mixed. If wp = 1 (default value), the strata allocation will be propotional. If wp = 0, the strata allocation will be equal. 
#' @param samplesize Total sample size expected. Not needed when method is error. 
#' @param e Margin of error. Only needed if method is error. 
#' @param min Minimum expected sample size per stratum. Default is 1. 
#' @param deff Design effect (DEff) provided from previous estimations (e.g., 1.5).
#' @param rr Value or vector of values of the expected response rate, it migth higher than 0 and lower than 1.
#' @param p Value or vector of values with the proportion to be estimated by stratum.  
#' @examples
#' astrata(200, c(200, 300, 500))
astrata <- function(samplesize, N, method = "mixed", min = 1, wp = 1, e = NULL, deff = 1, rr = 1, p = 0.5) { 
 
  # checks   
  if (length(p) > 1 & (length(N) != length(p))) {
    stop("p should have the same length as N")
  }

  if (length(rr) > 1 & (length(N) != length(rr))) {
    stop("rr should have the same length as N")
  }


  if (method == "error") {
  
  if (is.null(e)) {
    stop("When method = 'error', e should be a valid MOE")
  }

  if (length(e) > 1 & (length(N) != length(e))) {
    stop("e should have the same length as N")
  }
  n <- ssize(e, N = N, deff = deff, p = p, rr = rr)
  n <- ifelse(n < min & N > min, min, n)
  n <- ifelse(n > N, N, n)
 
  }
 
  else if (method == "root" ) {
  
  n <- round(samplesize * (sqrt(N) / sum(sqrt(N))), digits = 0)
  n <- round((n * deff) / rr, digits = 0)
  n <- ifelse(n < min & N > min, min, n)
  n <- ifelse(n > N, N, n)
 
  }
 
  else if (method == "neyman") {
  
  Sh <- sqrt(p * (1 - p))
  n <- round(samplesize * ((( N / sum(N)) * Sh)/ sum((N / sum(N)) * Sh)) , digits = 0)
  n <- round( (n * deff) / rr, digits = 0)
  n <- ifelse(n < min & N > min, min, n)
  n <- ifelse(n > N, N, n)
 
  }
 
  else if (method == "stdev") {
  
  Sh <- sqrt(p * (1 - p))
  n <- round(samplesize * (Sh / sum(Sh)) , digits = 0)
  n <- round((n * deff) / rr, digits = 0)
  n <- ifelse(n < min & N > min, min, n)
  n <- ifelse(n > N, N, n)
 
  }
 
 
  else if (method == "mixed") {
 
  ne <- round(samplesize / length(N), digits = 0)
  np <- round(samplesize * N / sum(N), digits = 0)
 
  we <- 1 - wp
  n <- round(ne * we + np * wp, digits = 0)
  n <- round((n * deff) / rr, digits = 0)
  n <- ifelse(n < min & N > min, min, n)
  n <- ifelse(n > N, N, n)
 
  }
 
  return(n)
 
}