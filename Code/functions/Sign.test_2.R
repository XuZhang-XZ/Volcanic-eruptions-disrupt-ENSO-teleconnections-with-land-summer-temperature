
## Sign.function
Sign.test = function(x1){
  
  a1 = length(which(x1>0))
  a2 = length(which(x1<0))
  a3 = max(a1,a2)
  Sig.ratio = a3/(a1 + a2)
  
  if (is.na(Sig.ratio)) {
    p.value = NA
  } else if (Sig.ratio >= 0.9) {
    p.value = 0.04
  } else if (Sig.ratio < 0.9) {
    p.value = 0.9
  }
  p.value
}

## Sign.function
Sign.test_2 = function(x1){
  
  ## Mean
  x1_mean = mean(x1,na.rm = TRUE)
  
  ## p value
  a1 = length(which(x1>0))
  a2 = length(which(x1<0))
  a3 = max(a1,a2)
  if (a3 >= 25) {
    p.value = 0.04
  } else {
    p.value = 0.9
  }
  
  ## Return
  return(c(x1_mean,p.value))
}
