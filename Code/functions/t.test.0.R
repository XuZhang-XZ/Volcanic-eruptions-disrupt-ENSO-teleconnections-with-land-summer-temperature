
## T test
t.test.series = function(a,x1,x2){
  
  ## NA Values
  # a = rnorm(100)
  x = a[x1];y = a[x2]
  xx = x[which(!is.na(x))]
  yy = y[which(!is.na(y))]
  if(length(xx)<=4|length(yy)<=4){
    return(NA)
  }
  if(var(xx)<0.001|var(yy)<0.001){
    return(NA)
  }
  
  ## T test
  temp = t.test(x = a[x1],y = a[x2])
  pp = temp$p.value
  pp
}


t.test.0 = function(a){
  
  ## NA Values
  aa = a[which(!is.na(a))]
  if(length(aa)<=3){
    return(NA)
  }
  if(var(aa)<0.001){
    return(NA)
  }
  
  ## T test
  temp = t.test(x = aa)
  pp = temp$p.value
  pp

}
