

areal.mean=function(z,A.Weight){
  dim.year = dim(z)[3]
  x1 = NA
  for(i in 1:dim.year){
    x1[i] = sum(z[,,i] * A.Weight, na.rm = TRUE)/sum(is.finite(z[,,i]) * A.Weight)
  }
  x1
}

area.drought=function(z,A.Weight){
  dim.year = dim(z)[3]
  x1 = NA
  for(i in 1:dim.year){
    x1[i] = sum(z[,,i] * A.Weight, na.rm = TRUE)
  }
  x1
}

Areal=function(z,A.Area,startv,endv){
  
  z1=z
  z1[which(!(startv<z1&z1<=endv))]=NA
  z1[which(!is.na(z1))]=1
  
  z.area <- sum(z1 * A.Area, na.rm = TRUE) / sum(is.finite(z) * A.Area)
  if(is.nan(z.area)) {z.area=0}
  z.area*100
}


Drought.FS=function(z,startv,endv){
  
  if(length(which(!is.na(z))) < 1) {
    return(c(NA,NA))
  }
  
  z1=z
  z1[which(!(startv<z1&z1<=endv))]=NA
  Num = length(which(!is.na(z1)))
  Severity = mean(z1,na.rm = T)
  return(c(Num,Severity))
}

# ## Test
# A.Weight = array(rep(cos(o.lat1*pi/180),each = length(o.lon1)),c(length(o.lon1),length(o.lat1)))
# image.plot(Mean.Rec.TWS[,,1],zlim = c(-50,50))
# image.plot(A.Weight*Mean.Rec.TWS[,,1],zlim = c(-50,50))
# 
# Mean.Rec.TWS[,,1] = 1
# image.plot(Mean.Rec.TWS[,,1])
# image.plot(A.Weight*Mean.Rec.TWS[,,1])
# 
# image.plot(is.finite(Mean.Rec.TWS[,,1]))


# 
# 
# TRUE*1
# FALSE*1
# 
# areal.mean=function(z,A.Weight){
#   
#   zo <- sum(z * A.Weight, na.rm = TRUE)/sum(is.finite(z * A.Weight))
#   if(is.nan(zo)) {zo=NA}
#   zo
# }



