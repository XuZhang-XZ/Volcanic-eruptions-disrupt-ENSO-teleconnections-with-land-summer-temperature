

# Volcanic Eruption Data ------------------------------------------------------------

## Read Events
Volcanics = read.csv("Input Data/Global Volcanic Eruptions/All Eruptions.csv",header = T)
Volcanics1 = Volcanics.raw = Volcanics[, c(
  "Volcano.Number",
  "Volcano.Name",
  "Eruption.Number",
  "Eruption.Category",
  "VEI",
  "Start.Year",
  "Start.Year.Uncertainty",
  "Start.Month",
  "Start.Day",
  "End.Year",
  "End.Month",
  "End.Day",
  "Latitude",
  "Longitude",
  "Evidence.Method..dating."
)]

## Selection Time Period
Volcanics1 = Volcanics1 %>% 
  dplyr::filter(Start.Year >= 1900 & Start.Year <= 2022) %>% 
  dplyr::filter(VEI >= 5)

## Selection Locations
Volcanics1 = Volcanics1 %>% 
  dplyr::filter(is.na(Start.Year.Uncertainty) | Start.Year.Uncertainty<1)

## Add Submarine Volcano NNE of Iriomotejima
Volcanics1 = rbind(Volcanics1,Volcanics.raw[which(Volcanics.raw$Volcano.Name == "Submarine Volcano NNE of Iriomotejima"),])

## Change Eruption Data
Volcanics1[which(Volcanics1$Volcano.Name == "Hunga Tonga-Hunga Ha'apai"), c("Start.Year","Start.Month")] = c(2022,1)
Volcanics1[which(Volcanics1$Volcano.Name == "Pinatubo"), c("Start.Month")] = 6
Volcanics1[which(Volcanics1$Volcano.Name == "Chichon, El"), c("Start.Month")] = 4
Volcanics1[which(Volcanics1$Volcano.Name == "Agung"), c("Start.Month")] = 4
Volcanics1[which(Volcanics1$Volcano.Name == "St. Helens"), c("Start.Month")] = 5
Volcanics1[which(Volcanics1$Volcano.Name == "Agung"), c("Start.Month")] = 3
Volcanics1[which(Volcanics1$Volcano.Name == "Bezymianny"), c("Start.Year","Start.Month")] = c(1956,3)
Volcanics1[which(Volcanics1$Volcano.Name == "Azul, Cerro"), c("Start.Year","Start.Month")] = c(1932,4)
Volcanics1[which(Volcanics1$Volcano.Name == "Submarine Volcano NNE of Iriomotejima"), c("VEI")] = 5

## Revise
Volcanics1 = Volcanics1 %>% 
  arrange(desc(Start.Year)) %>% 
  dplyr::mutate(Duration.year = End.Year - Start.Year)

## Output
Volcanics1.Write = Volcanics1 %>% dplyr::select(
  Volcano.Name,
  Longitude,
  Latitude,
  VEI,
  Start.Year,
  Start.Month
)

## Write
write.csv(Volcanics1,"Output Data/Plot_Data/Volcanics1.csv")
write_xlsx(Volcanics1.Write,"Output Data/Plot_Data/Volcanics1.xlsx")

## Defining Vol years
Volcanics.Early = Volcanics1[which(Volcanics1$Start.Month <= 8),]
Volcanics.Late = Volcanics1[which(Volcanics1$Start.Month > 8),]
Vol.years = c(
  Volcanics.Early$Start.Year,
  Volcanics.Early$Start.Year + 1,
  Volcanics.Early$Start.Year + 2,
  Volcanics.Late$Start.Year + 1,
  Volcanics.Late$Start.Year + 2,
  Volcanics.Late$Start.Year + 3
)
Vol.years = unique(Vol.years)

## First and Third Years
Vol_1 = unique(c(Volcanics.Early$Start.Year + 0, Volcanics.Late$Start.Year + 1))
Vol_2 = unique(c(Volcanics.Early$Start.Year + 1, Volcanics.Late$Start.Year + 2))
Vol_3 = unique(c(Volcanics.Early$Start.Year + 2, Volcanics.Late$Start.Year + 3))

# ENSO data -------------------------------------------------------

## ENSO
SST_ENSO = read.csv("Output Data/Plot_Data/SST_ENSO.csv",row.names = 1)

## Pass-filter
SST_ENSO = SST_ENSO %>%
  dplyr::group_by(Dataset) %>%
  dplyr::reframe(
    year = year,
    NINO34 = pass.filter.NA(
      a = NINO34,
      W = 15,
      type = "high",
      method = c("Butterworth")
    ),
    Relative_NINO34 = pass.filter.NA(
      a = Relative_NINO34,
      W = 15,
      type = "high",
      method = c("Butterworth")
    ),
    Tropic_T = pass.filter.NA(
      a = Tropic_T,
      W = 15,
      type = "high",
      method = c("Butterworth")
    )
  )

## Ensemble Median
SST_ENSO_1 = SST_ENSO %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(across(!Dataset,mean)) %>%
  dplyr::select(!Tropic_T)

## Definition of ENSO events
A.NINO = SST_ENSO_1
A.NINO$NINO34.z = as.numeric(scale(A.NINO$NINO34))
A.NINO$Type = NA
A.NINO$Type[which(A.NINO$NINO34.z<(-0.5))] = "La"
A.NINO$Type[which(A.NINO$NINO34.z>=(-0.5)&A.NINO$NINO34.z<=(0.5))] = "Ne"
A.NINO$Type[which(A.NINO$NINO34.z>(0.5))] = "Ei"

## Definition of Volcanic Eruption
A.NINO$Eruption.year = "NVol"
A.NINO$Eruption.year[which(A.NINO$year%in%Vol.years)] = "Vol"

## Write
write.csv(A.NINO,"Output Data/Plot_Data/All_A.NINO.csv")

## Clip
A.NINO = subset(A.NINO,year%in%c(1901:2022))

## Write
write.csv(A.NINO,"Output Data/Plot_Data/A.NINO.csv")

# End -------------------------------------------------------------
