### function to find the parameter of a beta distribution to have a mode in 0.9 and a cv of 0.1
cv <- function(a){
   b=(a+8)/9
  m= a / (a+b)
  v2 = a*b /( (a+b)^2 *(a+b+1))
  return( (sqrt(v2)/m -0.1)^2 ) 
  }