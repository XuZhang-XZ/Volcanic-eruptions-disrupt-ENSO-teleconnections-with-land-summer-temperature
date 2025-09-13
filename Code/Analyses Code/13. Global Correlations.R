

# Land Temperature ------------------------------------------------

## Eruption years
Volcanics1 = read.csv("Output Data/Plot_Data/Volcanics1.csv",row.names = 1)
A.NINO = read.csv("Output Data/Plot_Data/A.NINO.csv",row.names = 1)
GLST_Ens = read.csv("Output Data/Observation_Summary/GLST_Ens.csv",row.names = 1)

## Comparison
Total = merge(GLST_Ens,A.NINO)
Total.cor = data.frame()

## All Years
cor.test(Total$NINO34,Total$GLST_MEAN)
cor.test(Total$Relative_NINO34,Total$GLST_MEAN)

## Non-Volcanic years
Total1 = Total[which(Total$Eruption.year == "NVol"),]
cor.test(Total1$NINO34,Total1$GLST_MEAN)
cor.test(Total1$Relative_NINO34,Total1$GLST_MEAN)
plot(Total1$NINO34,Total1$GLST_CRU,type = "p")

## Volcanic years
Total2 = Total[which(Total$Eruption.year %in% c("Vol")),]
cor.test(Total2$NINO34,Total2$GLST_MEAN)
cor.test(Total2$Relative_NINO34,Total2$GLST_MEAN)
plot(Total2$NINO34,Total2$GLST_CRU,type = "p")

## Volcanic years removing 1991-1993
Total3 = Total2%>%dplyr::filter(Eruption.year%in%c("Vol"))%>%dplyr::filter(!year %in% c(1991:1993))
cor.test(Total3$NINO34,Total3$GLST_MEAN)
cor.test(Total3$Relative_NINO34,Total3$GLST_MEAN)
plot(Total3$NINO34,Total3$Mid_Tmp,type = "p")

## Summary
Total1 = Total[which(Total$Eruption.year == "NVol"),]
Total.cor[1,c("Dataset","Eruption.year","Cor","Cor.p")] = c("CRU","NVol",cor25.1(Total1$NINO34,Total1$GLST_CRU))
Total.cor[2,c("Dataset","Eruption.year","Cor","Cor.p")] = c("GISS","NVol",cor25.1(Total1$NINO34,Total1$GLST_GISS))
Total.cor[3,c("Dataset","Eruption.year","Cor","Cor.p")] = c("BEST","NVol",cor25.1(Total1$NINO34,Total1$GLST_BEST))
Total.cor[4,c("Dataset","Eruption.year","Cor","Cor.p")] = c("MEAN","NVol",cor25.1(Total1$NINO34,Total1$GLST_MEAN))

## Summary
Total2 = Total[which(Total$Eruption.year == "Vol"),]
Total.cor[5,c("Dataset","Eruption.year","Cor","Cor.p")] = c("CRU","Vol",cor25.1(Total2$NINO34,Total2$GLST_CRU))
Total.cor[6,c("Dataset","Eruption.year","Cor","Cor.p")] = c("GISS","Vol",cor25.1(Total2$NINO34,Total2$GLST_GISS))
Total.cor[7,c("Dataset","Eruption.year","Cor","Cor.p")] = c("BEST","Vol",cor25.1(Total2$NINO34,Total2$GLST_BEST))
Total.cor[8,c("Dataset","Eruption.year","Cor","Cor.p")] = c("MEAN","Vol",cor25.1(Total2$NINO34,Total2$GLST_MEAN))

## Relative NINO34
Total.cor[9,c("Dataset","Eruption.year","Cor","Cor.p")] = c("RMEAN","NVol",cor25.1(Total1$Relative_NINO34,Total1$GLST_MEAN))
Total.cor[10,c("Dataset","Eruption.year","Cor","Cor.p")] = c("RMEAN","Vol",cor25.1(Total2$Relative_NINO34,Total2$GLST_MEAN))

## Round
Total.cor = Total.cor %>% 
  dplyr::mutate(Cor = round(as.numeric(Cor),2), Cor.p = round(as.numeric(Cor.p),2))

## Write
write.csv(Total.cor,paste0("Output Data/Observation_Summary/Total.cor.csv"))
write.csv(Total,paste0("Output Data/Observation_Summary/Low_Mid_Tmp.csv"))

# End -------------------------------------------------------------
