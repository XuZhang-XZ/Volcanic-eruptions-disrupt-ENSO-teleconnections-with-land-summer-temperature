
## pass filter with NAs
## remove means at first
# pass.filter.NA=function(a=raw.chro[,2],W=70,type="high",method = c("Butterworth")){
#   a1=a
#   # a1=a1-mean(a1,na.rm=TRUE)
#   V.ge=which(!is.na(a))
#   if(length(V.ge)<=10) {return(array(NA,length(a)))}
#   
#   a1[V.ge]=dplR::pass.filt(y=a[V.ge],W=W,type=type,method = method)
#   a1
# }

Filter.NA = function(x, W = 15){
  
  a1 = x[which(!is.na(x))]
  
  if (length(a1) < 62) {
    a2 = detrend_linear(a1)
  } else {
    a2 = pass.filter.NA.mean(
      a = a1,
      W = W,
      type = "high",
      method = c("Butterworth")
    )
  }
  
  a.r = x
  a.r[which(!is.na(a.r))] = a2
  return(a.r)
}
 
mean.80 = function(x){
  a1 = x[which(!is.na(x))]
  if(length(a1)<=(0.5*length(x))){
    a.r = NA
  } else{
    a.r = mean(x,na.rm = TRUE)
  }
  a.r
}



pass.filter.NA=function(a=raw.chro[,2],W=70,type="high",method = c("Butterworth"),n = 4, Rp = 1){
  a1=a
  V.ge=which(!is.na(a))
  if(length(V.ge)<=10) {return(array(NA,length(a)))}
  
  a1[V.ge]=dplR::pass.filt(y=a[V.ge],W=W,type=type,method = method,n = n, Rp = Rp)
  a1=a1-mean(a1,na.rm=TRUE)+mean(a,na.rm=TRUE)
  a1
} 

pass.filter.NA.mean=function(a=raw.chro[,2],W=50,type="high",method = c("Butterworth"),n = 4, Rp = 1){
  a1=a
  V.ge=which(!is.na(a))
  if(length(V.ge)<=10) {return(array(NA,length(a)))}
  
  a1[V.ge]=dplR::pass.filt(y=a[V.ge],W=W,type=type,method = method,n = n, Rp = Rp)
  a1=a1-mean(a1,na.rm=TRUE)
  a1
} 
