

## monthly mean
month.mean=function(x){
  season=rep(c(1:12),3000)[1:length(x)]
  season.mean=0
  for(i in 1:12){
    season.mean[i]=mean(x[which(season==i)],na.rm=TRUE)
  }
  season.mean
}


## deseasonalize with automatic months
seal.mean1=function(x){
  season=rep(c(1:12),3000)[1:length(x)]
  season.mean=0
  for(i in 1:12){
    season.mean[i]=mean(x[which(season==i)],na.rm=TRUE)
  }
  new.x=x-rep(season.mean,3000)[1:length(x)]
  return(new.x)
}

## deseasonalize
seal.mean=function(x,monthss){
  
  season.mean=0
  for(i in 1:12){
    season.mean[i]=mean(x[which(monthss==i)],na.rm=TRUE)
  }
  season.series=season.mean[monthss]
  new.x=x-season.series
  
  return(new.x)
}
