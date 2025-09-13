
anomalies=function(x,monthss = rep(c(1:12),500)[1:length(x)]){
  season.mean=0
  for(i in 1:12){
    season.mean[i]=mean(x[which(monthss==i)],na.rm=TRUE)
  }
  anoma=x-season.mean[monthss]
  return(anoma)
}
