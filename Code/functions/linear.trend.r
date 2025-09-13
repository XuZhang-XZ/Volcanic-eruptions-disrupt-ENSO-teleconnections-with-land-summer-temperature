

# a= c(1:100)
linear.trend = function(a) {
  if(length(which(!is.na(a)))<10){
    return(NA)
  } else {
    a.data = data.frame("x" = c(1:length(a)),
                        "y" = a)
    a.data = a.data[complete.cases(a.data),]
    kk1 = lm(y~x,data = a.data)
    slope = kk1$coefficients[2]
  }
  return(slope)
}

detrend_linear = function(y){
  data1 = data.frame("x" = seq_len(length(y)),
                     "y" = y)
  if(length(which(complete.cases(data1)))<5) return(array(NA,length(y)))
  kk1 = lm(y~x, data = data1)
  data1$Pre.y = predict(kk1,data1)
  data1$Res.y = data1$y - data1$Pre.y
  return(as.numeric(data1$Res.y))
}

detrend_quadratic = function(y){
  data1 = data.frame("x" = seq_len(length(y)),
                     "y" = y)
  if(length(which(complete.cases(data1)))<5) return(array(NA,length(y)))
  kk1 = lm(y~x + x^2, data = data1)
  data1$Pre.y = predict(kk1,data1)
  data1$Res.y = data1$y - data1$Pre.y
  return(as.numeric(data1$Res.y))
}

detrend_cubic = function(y){
  data1 = data.frame("x" = seq_len(length(y)),
                     "y" = y)
  if(length(which(complete.cases(data1)))<5) return(array(NA,length(y)))
  kk1 = lm(y~x + x^2 + x^3, data = data1)
  data1$Pre.y = predict(kk1,data1)
  data1$Res.y = data1$y - data1$Pre.y
  return(as.numeric(data1$Res.y))
}


detrend_spline = function(y){
  data1 = data.frame("x" = seq_len(length(y)),
                     "y" = y)
  if(length(which(complete.cases(data1)))<5) return(array(NA,length(y)))
  kk1 = smooth.spline(x = data1$x, y = data1$y)
  temp = predict(kk1,data1$x)
  data1$Res.y = data1$y -  temp$y
  return(as.numeric(data1$Res.y))
}


Sen_sigle = function(x1,y1){
  data1 = data.frame("x" = x1,
                     "y" = y1)
  kk1 = lm(y~x, data = data1)
  return(c(kk1$coefficients[1],kk1$coefficients[2]))
}
