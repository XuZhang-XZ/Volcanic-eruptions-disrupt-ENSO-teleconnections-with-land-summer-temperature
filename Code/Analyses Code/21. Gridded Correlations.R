

# Parameters ------------------------------------------------------

## Eruption years
Volcanics1 = read.csv("Output Data/Plot_Data/Volcanics1.csv",row.names = 1)
A.NINO = read.csv("Output Data/Plot_Data/A.NINO.csv",row.names = 1)

# Global Temperature MEAN -------------------------------------------------------------

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

## Available Years
Tmp.year = unique(year(Tmp.time1))

## Change Resolution
Tmp.data = Tmp.data

## Time
Tmp.data = Tmp.data[,,which(year(Tmp.time1)%in% c(1901:2022))]
Tmp.time1 = Tmp.time1[which(year(Tmp.time1)%in% c(1901:2022))]
Tmp.year = unique(year(Tmp.time1))

## Remove Antarctic
Tmp.data[,which(i.lat< (-60)),] = NA

## Change Annual Resolution
Tmp_Summer = Field.sea.pass(Tmp.data = Tmp.data,Series.Time = Tmp.time1,filter_high = TRUE,Sea = "Summer",scaless = FALSE)

## Composite Values
Output.composite = expand.grid(lon = i.lon, lat = i.lat)
Num.years = Com.Par
ge = 1; i = 1

## Composite Tmp
for(i in 1:NROW(Com.Par)){
  
  ## Mean Values
  Tem_Summer_1 = Tmp_Summer[,,which(Tmp.year%in%(A.NINO$year[which(A.NINO$Eruption.year == Com.Par$Following[i]&A.NINO$Type == Com.Par$ENSO[i])]))]
  Tem_Summer_2 = Tmp_Summer[,,which(Tmp.year%in%(A.NINO$year[which(A.NINO$Eruption.year != Com.Par$Following[i]|A.NINO$Type != Com.Par$ENSO[i])]))]
  
  ## Mean Values
  temp_1 = apply(Tem_Summer_1,c(1,2),mean.80)
  Output.composite[,paste0("Tmp_",ge,"_",Com.Par$Following[i],"_",Com.Par$ENSO[i])] = as.numeric(temp_1)
  
  ## Significance
  temp_2 = apply(Tem_Summer_1,c(1,2),FUN = t.test.0)
  Output.composite[,paste0("Tmp_",ge,"_",Com.Par$Following[i],"_",Com.Par$ENSO[i],"_p")] = as.numeric(temp_2)
}

## Correlations
temp3 = apply(X = Tmp_Summer,MARGIN = c(1,2),FUN = cor30.1.df,a2 = A.NINO$NINO34,sel = which(A.NINO$Eruption.year == "Vol"),Tn = 88)
temp4 = apply(X = Tmp_Summer,MARGIN = c(1,2),FUN = cor30.1.df,a2 = A.NINO$NINO34,sel = which(A.NINO$Eruption.year == "NVol"),Tn = 88)

## Data
Output.composite[,paste0("Tmp_",ge,"_Vol_Cor")] = as.numeric(temp3[1,,])
Output.composite[,paste0("Tmp_",ge,"_Vol_Cor_p")] = as.numeric(temp3[2,,])
Output.composite[,paste0("Tmp_",ge,"_Vol_Cor_op")] = as.numeric(temp3[3,,])
Output.composite[,paste0("Tmp_",ge,"_NVol_Cor")] = as.numeric(temp4[1,,])
Output.composite[,paste0("Tmp_",ge,"_NVol_Cor_p")] = as.numeric(temp4[2,,])
Output.composite[,paste0("Tmp_",ge,"_NVol_Cor_op")] = as.numeric(temp4[3,,])

## Sensitivity
temp3 = apply(X = Tmp_Summer,MARGIN = c(1,2),FUN = Sen_30.1,a2 = A.NINO$NINO34,sel = which(A.NINO$Eruption.year == "Vol"),simplify = TRUE)
temp4 = apply(X = Tmp_Summer,MARGIN = c(1,2),FUN = Sen_30.1,a2 = A.NINO$NINO34,sel = which(A.NINO$Eruption.year == "NVol"),simplify = TRUE)

## Data
Output.composite[,paste0("Tmp_",ge,"_Vol_Sen")] = as.numeric(temp3[1,,])
Output.composite[,paste0("Tmp_",ge,"_Vol_Sen_p")] = as.numeric(temp3[2,,])
Output.composite[,paste0("Tmp_",ge,"_NVol_Sen")] = as.numeric(temp4[1,,])
Output.composite[,paste0("Tmp_",ge,"_NVol_Sen_p")] = as.numeric(temp4[2,,])

if(1 == 0) {
  
  ## Plot Correlations
  layout(matrix(c(1:2),nrow=1,byrow = FALSE))
  image.plot(temp_1,zlim = c(-1,1))
  image.plot(temp_2,zlim = c(-1,1))
  
  ## Plot Correlations
  layout(matrix(c(1:2),nrow=1,byrow = FALSE))
  image.plot(temp3[1,,],zlim = c(-1,1))
  image.plot(temp4[1,,],zlim = c(-1,1))
}

## Correlation Type
Output.composite$Tmp_1_NVol_Re = Re.Cor(Output.composite$Tmp_1_NVol_Cor,Output.composite$Tmp_1_NVol_Cor_p)
Output.composite$Tmp_1_Vol_Re = Re.Cor(Output.composite$Tmp_1_Vol_Cor,Output.composite$Tmp_1_Vol_Cor_p)
Output.composite$Tmp_1_Re = paste0(Output.composite$Tmp_1_NVol_Re,"_",Output.composite$Tmp_1_Vol_Re)

## Examine
table(Output.composite$Tmp_1_Vol_Re)
table(Output.composite$Tmp_1_NVol_Re)
table(Output.composite$Tmp_1_Re)

## Examine
Output.composite$Tmp_1_NVol_Cor
Output.composite$Tmp_1_NVol_Cor_p

## Test
min(Output.composite$Tmp_1_NVol_Cor,na.rm = TRUE)

## Remote
Output.composite1 = Output.composite %>%
  dplyr::filter(!Tmp_1_Re %in% c("NA_NA","NA_P","NA_NS","NA_N","P_NA","NS_NA","N_NA")) %>%
  add_row(Tmp_1_Re = c(
    "N_P",
    "N_NS",
    "N_N",
    "NS_P",
    "NS_NS",
    "NS_N",
    "P_P",
    "P_NS",
    "P_N"
  ),lat = 90) %>%
  dplyr::mutate(Area = cos(lat*pi/180))

## Area
Output.composite.Area = Output.composite1 %>%
  dplyr::group_by(Tmp_1_Re) %>%
  dplyr::summarise(Area = sum(Area)) %>%
  arrange(desc(Tmp_1_Re)) %>%
  mutate(prop = Area / sum(Area) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop) %>%
  mutate(Text1 = paste0(round(prop,2), "%")) %>%
  mutate(Text2 = ifelse(prop > 10,Text1,NA)) %>%
  mutate(Dateset = "MEAN")

## Write
write.csv(Output.composite,"Output Data/Plot_Data/Output.composite_MEAN.csv")
write.csv(Output.composite.Area,"Output Data/Plot_Data/Output.composite.Area_MEAN.csv")

## Percentage area
sum(Output.composite.Area$prop[c(3,4,5,6,9)]) ## Negative Correlations
sum(Output.composite.Area$prop[1:6]) ## Significant Correlations
sum(Output.composite.Area$prop[c(2,3,4,5)]) ## Shifted Correlations
sum(Output.composite.Area$prop[c(2,3,4,5)]) / sum(Output.composite.Area$prop[1:6]) ## Sifted Regions
sum(Output.composite.Area$prop[c(2,3,4,5,7,9)]) / sum(Output.composite.Area$prop[1:9]) ## Sifted Regions to global regions

# End -------------------------------------------------------------

