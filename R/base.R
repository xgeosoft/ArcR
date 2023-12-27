#basics
standard_deviation = function(x,for.sample = TRUE){
  if(for.sample) return(sd(x))
  else return(sqrt(var(x)))
}
