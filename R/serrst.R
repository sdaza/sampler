#' @title Estimation of margins of errors (MOEs) for a proportion in a stratified sample
#' @description This function provides flexibility to estimate margins of errors for a proportion with a stratified sample assuming different population sizes, confidence levels, design effects (DEff), and response rates.
#' @param n Vector of sample size of each stratum. 
#' @param deff A value or vector of values with design effects (DEff) provided from previous studies.
#' @param rr Proportion representing expected response rate, it migth be 0 or more, and less than 1.
#' @param N Value or vector of values with the population size of each stratum.
#' @param W Proportion of each stratum in the target population. If the population size is not specified, MOEs are estimated using the infinite population formula.
#' @param cl Confidence levels, e.g., .95
#' @param relative Logical parameter. Estimate a relative error if TRUE.
#' @examples
#' serrst(n = c(100, 300, 100), N = c(500, 1000, 600))
serrst <- function(n, N = NULL, W = NULL, deff = 1, rr = 1, p = 0.5, cl=.95) {
 
# some checks
if (length(n) < 2 ) {
  stop("n should be a vector with more than an element, otherwise use fucntion serr()")
} 

if (length(n) != length(N)) {
  stop("n and N should have the same length")
}

if (length(rr) > 1 & length(rr) != length(n)) {
  stop("n and rr should have the same length")
}

if (!is.null(W) & length(W) != length(n)) {
  stop("n and W should have the same length")
}


# definition of z and q
z <- round(abs(qnorm((1 - cl)/2)),2)
q <- 1 - p
 
# definition of weigths and factor of correction
if (is.null(W)) {

w <- (N / sum(N))
f <- ((N - n) / (N - 1))

}

else {

w <- W
f <- 1 # infinite population
}
# effective sample size
neff <- (n / deff) * rr

# variance
var <- sum(w ^ 2 * ((p * q) / neff) * f)
sd <- sqrt(var)

return(round(sd * z, digits = 4))
 
}
 