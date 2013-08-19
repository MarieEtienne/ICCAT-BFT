
### counts the number of same occurence in vect1
### is vect2 is not null, counts the number of occurence of each value of vect2 in vect1
nOccurrences <- function(vect1, vect2=NULL){
  counts <- table(vect1)
  if(!is.null(vect2)){
    ret<- rep(0, length(vect2))
    for( i in names(counts)){
      ret[which(vect2==i)] <- counts[[i]]
    }
  } else {
    ret <- as.numeric(counts)
  } 
  return(ret)
}


vonB <- function(age, linf=319,
                 k = 0.093,
                 t0 =-0.97)
{
  return(linf * ( 1 - exp( -k*(age -t0)) ))
}

weightAtAge <- function(age, sclw = 1.95e-5,   #scaler in length-weight allometry
                        plw   = 3.009     #power in length-weight allometry
                        )
{
  return(sclw * vonB(age)^plw)
}


sellogistic <- function(age, mu_a=6, sigma_a=2)
{
  return(1 / (1 + exp( -(age-mu_a)/sigma_a) ))
}

