

## T test
t.test.1 = function(a){
  
  ## NA Values
  aa = a[which(!is.na(a))]
  if(length(aa)<=3){
    return(c(NA,NA))
  }
  if(var(aa)<0.001){
    return(c(NA,NA))
  }
  
  ## T test
  temp = t.test(x = aa)
  pp = c(temp$estimate, temp$p.value)
  pp
}