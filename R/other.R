# DEFF
def <- function(nbar,k, delta) {
 
  d <- k * ( 1 + delta *(nbar - 1))
  return(d)
 
}
 
# ToCV
toCV <- function(e=.05, cl=.95, p = 0.5) {
 
z <- abs(qnorm((1 - cl) / 2))
CV <- (e / z) / p
return(CV)
 
}
 
 
 
# ToMOE
toMOE <- function(cv= .05, cl= .95, p = 0.5) {
 
z <- abs(qnorm((1 - cl)/ 2))
MOE <- z * cv* p
return(MOE)
 
}