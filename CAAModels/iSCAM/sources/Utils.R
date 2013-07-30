
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