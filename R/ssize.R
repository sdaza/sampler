#' @title Estimation of sample sizes for a proportion
#' @description This function provides flexibility to estimate margins of errors for a proportion assuming different population sizes, confidence levels, design effects (DEff) and response rates. 
#' @param e Expected margin of error.
#' @param deff Design effect (DEff) provided from previous estimations (e.g., 1.5).
#' @param rr Proportion representing expected response rate, it migth higher than 0 and lower than 1.
#' @param N Population size. If the population size is not specified, MOEs are estimated using the infinite population formula.
#' @param cl Confidence levels, e.g., .95
#' @param relative Logical parameter. Estimate a relative error if TRUE.
#' @examples
#' ssize(e = 0.02, deff = 1.5, rr = .8, N = 1000)
#' ssize(e = 0.05)
ssize <- function(e, deff = 1, rr = 1, N = NULL, cl = .95, p = 0.5)  {
 
# validation
 
  if (sum(e <= 0 | e >= 1) >= 1) { 
    stop("e must be between 0 and 1")
  }
 
  if (sum(deff <= 0) >= 1) { 
    stop("deff must be bigger than 0")
  }
 
  if (sum(rr <= 0 ) >= 1) { 
    stop("rr must be bigger than 0")
  }
 
  # define z
  z <- round(abs(qnorm((1 - cl)/ 2)), 2)
 
  if (!is.null(N)) {  
    if (sum( N == 0 ) >= 1) { 
      stop("N vector contains 0 values")
      }
 
    n <- (z ^ 2 * ( p * ( 1 - p ))) / ( e ^ 2 )
    n <- (((n * N)/(n + ( N - 1))) * deff) / rr
    n <- round(n, digits=0)
 
     }
 
  else {
    
    n <- (z ^ 2 * ( p * (1 - p)))/(e ^ 2)
    n <- (n * deff) / rr
    n <- round(n, digits = 0)
 
  } 
 
 
  if (sum(N < n) >= 1) {
  message("n is bigger than N in some rows: n = N")
  }
  
  n[N < n] <- N
  
  return(n)
 
}