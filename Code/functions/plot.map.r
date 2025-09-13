

Trans.map = function(shp1){
  shp2 = data.frame()
  group.id = levels(unique(shp1$group))
  j=2
  for(j in 1:length(group.id)){
    
    pol.shp = shp1[shp1$group == group.id[j],]
    lon.r = range(pol.shp$long)
    
    if(lon.r[1]>=(0)) {
      shp2 = rbind(shp2,pol.shp)
      next
    }
    if(lon.r[2]<=(0)) {
      pol.shp$long = pol.shp$long+360
      shp2 = rbind(shp2,pol.shp)
      next
    }
    if(lon.r[1]<(0)&lon.r[2]>(0)) {
      id1 = paste0(group.id[j],".1")
      shp.temp1 = pol.shp
      shp.temp1$long[shp.temp1$long<(-1)] = -1
      shp.temp1$group = id1

      id2 = paste0(group.id[j],".2")
      shp.temp2 = pol.shp
      shp.temp2$long = shp.temp2$long+360
      shp.temp2$long[shp.temp2$long>(361)] = 361
      shp.temp2$group = id2

      shp2 = rbind(shp2,shp.temp1,shp.temp2)
    }
  }
  shp2
}


## https://stackoverflow.com/questions/5353184/fixing-maps-library-data-for-pacific-centred-0-360-longitude-display
plot.map<- function(database,center,...){
  
  Obj <- map(database,plot=F)
  coord <- cbind(Obj[[1]],Obj[[2]])
  
  # split up the coordinates
  id <- rle(!is.na(coord[,1]))
  id <- matrix(c(1,cumsum(id$lengths)),ncol=2,byrow=T)
  polygons <- apply(id,1,function(i){coord[i[1]:i[2],]})
  
  # split up polygons that differ too much
  polygons <- lapply(polygons,function(x){
    x[,1] <- x[,1] + center
    x[,1] <- ifelse(x[,1]>180,x[,1]-360,x[,1])
    if(sum(diff(x[,1])>300,na.rm=T) >0){
      id <- x[,1] < 0
      x <- rbind(x[id,],c(NA,NA),x[!id,])
    }
    x
  })
  # reconstruct the object
  polygons <- do.call(rbind,polygons)
  Obj[[1]] <- polygons[,1]
  Obj[[2]] <- polygons[,2]
  
  map(Obj,...)
}