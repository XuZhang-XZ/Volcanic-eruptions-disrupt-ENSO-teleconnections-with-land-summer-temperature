

# MEAN ----------------------------------------------

## Library
library(randomForest)

## Global Temperature
Noah=nc_open("Z:/2023/Science10_SM/Output Data/Observation_Summary/Ens.Tmp.mean.nc")
Tmp.data=ncvar_get(Noah,"tmp")
i.lon=ncvar_get(Noah, "lon")
i.lat=ncvar_get(Noah, "lat")
i.time=ncvar_get(Noah, "time")
Tmp.time1=i.time+ymd(17010101)
Tmp.time1 = ymd(year(Tmp.time1)*10000+month(Tmp.time1)*100+1)
Tmp.year = unique(year(Tmp.time1))
nc_close(Noah)

## Summer Data
Tmp_Summer = Field.sea.pass(
  Tmp.data = Tmp.data,
  Series.Time = Tmp.time1,
  filter_high = FALSE,
  Sea = "Summer",
  scaless = TRUE
)

## Remove NA
Tmp.NA.Num = apply(Tmp_Summer,c(1,2),function(x) {length(which(is.na(x)))})
for(i in 1:length(i.lon)) {
  for (j in 1:length(i.lat)) {
    if (Tmp.NA.Num[i, j] > 0) {
      Tmp_Summer[i, j, ] = NA
    }
  }
}

## Plot
if(1 == 0) {
  image.plot(Tmp_Summer[,,100])
}

## Eruption years
Volcanics1 = read.csv("Output Data/Plot_Data/Volcanics1.csv",row.names = 1)
A.NINO = read.csv("Output Data/Plot_Data/A.NINO.csv",row.names = 1)

## Correlation summary
Cor_Summary = data.frame()

## Four Models
Models.names = c("Global","NH","SH","Tropicial","Pacific")
i = 1

## PCs
for(i in 1:5){
  
  ## Temperature Data
  Tmp_Summer1 = Tmp_Summer
  
  ## Scenarios
  if(i ==1){
    Tmp_Summer1 = Tmp_Summer
  }
  if(i ==2){
    Tmp_Summer1[,which(i.lat<(0)),] = NA
  }
  if(i ==3){
    Tmp_Summer1[,which(i.lat>(0)),] = NA
  }
  if(i ==4){
    Tmp_Summer1[,which(i.lat<(-23) | i.lat>(23)),] = NA
  }
  if(i ==5){
    Tmp_Summer1[,which(i.lat>(45)|i.lat<(-45)),] = NA
    Tmp_Summer1[which(i.lon>(320)|i.lon<(80)),,] = NA
  }
  
  ## PCs for Reconstruction
  PCs_data = data.frame("year" = Tmp.year)
  names(dim(Tmp_Summer1)) <- c("lon","lat","sdate")
  EOF_data = s2dv::EOF(ano=Tmp_Summer1, lon=i.lon, lat=i.lat, neofs = 10, corr = FALSE)
  PCs_1=as.data.frame(EOF_data$PCs)
  PCs_data[,paste0("PCs_",Models.names[i],"_",c(1:10))] = PCs_1
  PCs_data[2:NROW(PCs_1),paste0("De_PCs_",Models.names[i],"_",c(1:10))] = PCs_1[1:(NROW(PCs_1)-1),]
  
  ## Plot
  if(1 == 0) {
    ttEOF = aperm(EOF_data$EOFs,c(3,2,1))
    par(mfcol = c(3, 1))
    image.plot(ttEOF[,,1], zlim = c(-0.02,0.02))
    image.plot(ttEOF[,,2], zlim = c(-0.02,0.02))
    image.plot(ttEOF[,,3], zlim = c(-0.02,0.02))
    
    plot(PCs_data$year,scale(PCs_data$PCs_Global_1),type = "l")
    lines(A.NINO$year,scale(A.NINO$NINO34),col = "red")
    plot(PCs_data$year,scale(PCs_data$PCs_Global_2),type = "l")
    lines(A.NINO$year,scale(A.NINO$NINO34),col = "red")
  }
  
  ## Pass-filter
  PCs_data = PCs_data %>%
    dplyr::mutate(across(!year,Filter.NA,W = 15))

  ## Comparison
  Total = merge(A.NINO,PCs_data,by = "year")
  
  ## Prediction Selection
  Cor.PCs = data.frame("Names" = c(
    paste0("PCs_", Models.names[i], "_", c(1:10)),
    paste0("De_PCs_", Models.names[i], "_", c(1:10))
  ))
  for(ks in 1:NROW(Cor.PCs)) {
    temp = cor.test(Total[,Cor.PCs$Names[ks]],Total$NINO34)
    Cor.PCs[ks,"Cor"] = temp$estimate
  }
  Sel.cols = order(abs(Cor.PCs$Cor),decreasing = T)[1:5]
  Total_Pre = Total[,c(Cor.PCs$Names[Sel.cols],"NINO34")]
  
  # Linear Regression——Cross-validation
  Total$CV.type = rep(c(1:20),each = 10)[1:NROW(Total)]
  CV.ges = unique(Total$CV.type)
  CV.ks = 1
  for(CV.ks in CV.ges) {
    Cali.rows = which(Total$CV.type != CV.ks)
    Vali.rows = which(Total$CV.type == CV.ks)
    rf <- lm(NINO34 ~ ., data= Total_Pre[Cali.rows,])
    Total[Vali.rows,paste0("Pre_NINO34_",Models.names[i])] = predict(rf, Total_Pre)[Vali.rows]
  }
  A.NINO[,paste0("Pre_NINO34_LmCV_",Models.names[i])] = Total[,paste0("Pre_NINO34_",Models.names[i])]
  A.NINO[,paste0("Pre_NINO34_",Models.names[i])] = Total[,paste0("Pre_NINO34_",Models.names[i])]
  
  ## Correlations
  Total1 = Total[which(Total$Eruption.year == "NVol"),]
  Total2 = Total[which(Total$Eruption.year == "Vol"),]

  ## NVol years
  temp1 = cor.test(Total1$NINO34,Total1[,paste0("Pre_NINO34_",Models.names[i])])
  Cor_Summary1 = data.frame()
  Cor_Summary1[1,c("Cor","Cor_p")] = c(temp1$estimate,temp1$p.value)
  Cor_Summary1[1,c("Model","Erupton.year","Method")] = c(Models.names[i],"NVol","lmCV")

  ## Vol years
  temp1 = cor.test(Total2$NINO34,Total2[,paste0("Pre_NINO34_",Models.names[i])])
  Cor_Summary1[2,c("Cor","Cor_p")] = c(temp1$estimate,temp1$p.value)
  Cor_Summary1[2,c("Model","Erupton.year","Method")] = c(Models.names[i],"Vol","lmCV")

  ## Combine
  Cor_Summary = rbind(Cor_Summary,Cor_Summary1)

  ## Print
  print(i)
}

## Correlation
Cor_Summary

## Write data
write.csv(Cor_Summary,"Output Data/Observation_Summary/Rec_Cor_Summary_MEAN.csv")
write.csv(A.NINO,"Output Data/Observation_Summary/SEA.data_MEAN.csv")

# End -------------------------------------------------------------

