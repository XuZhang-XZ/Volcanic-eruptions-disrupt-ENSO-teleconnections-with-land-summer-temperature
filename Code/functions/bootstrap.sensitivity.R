

bootstrap.sensitivity=function(x1,y){
  
  sen.sum = 0
  Total.data = data.frame("x1" = x1,"y"= y)
  
  for(i in 1:1000){
    Sel = sample(1:NROW(Total.data),NROW(Total.data),replace = TRUE)
    Total.data1 = Total.data[Sel,]
    kk1 = lm(y~x1,data = Total.data1)
    sen.sum[i] = kk1$coefficients[2]
  }
  sen.sum
}


boot.cor.33 = function(x1,x2,n){
  
  cor.sum = 0
  Total.data = data.frame("x1" = x1,"x2"= x2)
  
  for(i in 1:1000){
    Sel = sample(1:NROW(Total.data),n)
    Total.data1 = Total.data[Sel,]
    cor.sum[i] = cor(Total.data1$x1,Total.data1$x2)
  }
  
  cor.sum
}


boot.cor.resample = function(x1,x2,n = length(x1)){
  
  cor.sum = 0
  Total.data = data.frame("x1" = x1,"x2"= x2)
  
  for(i in 1:1000){
    Sel = sample(1:NROW(Total.data),n,replace = TRUE)
    Total.data1 = Total.data[Sel,]
    cor.sum[i] = cor(Total.data1$x1,Total.data1$x2)
  }
  
  cor.sum
}
