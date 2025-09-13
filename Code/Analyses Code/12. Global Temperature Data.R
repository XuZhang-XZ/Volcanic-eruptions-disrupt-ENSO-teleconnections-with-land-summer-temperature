
# Land Temperature ------------------------------------------------

## Ensemble Mean
Ens.lon = seq(0.25, 359.75, by = 0.5)
Ens.lat = seq(-89.75, 89.75, by = 0.5)
Ens.year = 1901:2022
Ens.date = ymd(19010101) + months(0:(122*12-1))
Ens.Tmp.data = array(NA,c(4,720,360,122*12))

## Land Mask
Land.mask = NA

# CRU ------------------------------------------------

## Global Temperature
Noah=nc_open("Z:/2023/Input data/CRU TS v4.07/cru_ts4.07.1901.2022.tmp.dat.nc")
Tmp.data=ncvar_get(Noah,"tmp")
i.lon=ncvar_get(Noah, "lon")
i.lat=ncvar_get(Noah, "lat")
i.time=ncvar_get(Noah, "time")
Tmp.time1=i.time+ymd(19000101)
Tmp.time1 = ymd(year(Tmp.time1)*10000+month(Tmp.time1)*100+1)
Tmp.year = unique(year(Tmp.time1))
nc_close(Noah)

## Available Years
Tmp.year = unique(year(Tmp.time1))

## Change Resolution
i.lon = i.lon[c(361:720,1:360)]
i.lon[which(i.lon < 0)] = i.lon[which(i.lon < 0)] + 360
Tmp.data = Tmp.data[c(361:720,1:360),,]

## Time
Tmp.data = Tmp.data[,,which(year(Tmp.time1)%in% c(1901:2022))]
Tmp.time1 = Tmp.time1[which(year(Tmp.time1)%in% c(1901:2022))]
Tmp.year = unique(year(Tmp.time1))

## Remove Antarctic
Tmp.data[,which(i.lat< (-60)),] = NA

## Land Mask
Land.mask = Tmp.data[,,1464]

## Anomalies
Tmp.data.mean = array(NA,c(length(i.lon),length(i.lat),length(Tmp.time1)))
for(i in 1:12) {
  temp = apply(Tmp.data[,,which(month(Tmp.time1) == i)],c(1,2),mean,na.rm = TRUE)
  Tmp.data.mean[,,which(month(Tmp.time1) == i)] = temp
}
Tmp.data = Tmp.data - Tmp.data.mean

## Grids
lon.diff = diff(i.lon)[1]
lat.diff = diff(i.lat)[1]

## To Terra:Rast
target_data = aperm(Tmp.data,c(2,1,3))
rm <- rast(target_data)
ext(rm) <- c(-lon.diff, 360+lon.diff, -lat.diff-90, lat.diff+90)
terra::time(rm) <- Ens.date

## Re sample
s <- rast(nrows=360, ncols=720, xmin=0, xmax=360, ymin=-90, ymax=90)
t.total_resample = terra::resample(rm,s, method = "bilinear")

## Write data
out.name = paste0("Z:/2023/Science10_SM/Output Data/Observation_Summary/CRU_rigrid.nc")
rr <-
  writeCDF(
    t.total_resample,
    out.name,
    overwrite = TRUE,
    varname = "Tmp",
    longname = "surface temperature",
    unit = "K"
  )

## Ensemble Member
temp = as.array(t.total_resample)
temp = aperm(temp,c(2,1,3))
Ens.Tmp.data[1,,,] = temp

## Test
image.plot(temp[,,1])

# GISS ------------------------------------------------

## Global Temperature
Noah=nc_open("Z:/2023/Input data/GISTEMP v4/gistemp250_GHCNv4.nc")
Tmp.data=ncvar_get(Noah,"tempanomaly")
i.lon=ncvar_get(Noah, "lon")
i.lat=ncvar_get(Noah, "lat")
i.time=ncvar_get(Noah, "time")
Tmp.time1=i.time+ymd(18000101)
Tmp.time1 = ymd(year(Tmp.time1)*10000+month(Tmp.time1)*100+1)
Tmp.year = unique(year(Tmp.time1))
nc_close(Noah)

## Available Years
Tmp.year = unique(year(Tmp.time1))

## Test
image.plot(Tmp.data[,,1700])

## Change Resolution
i.lon = i.lon[c(91:180,1:90)]
i.lon[which(i.lon < 0)] = i.lon[which(i.lon < 0)] + 360
Tmp.data = Tmp.data[c(91:180,1:90),,]

## Time
Tmp.data = Tmp.data[,,which(year(Tmp.time1)%in% c(1901:2022))]
Tmp.time1 = Tmp.time1[which(year(Tmp.time1)%in% c(1901:2022))]
Tmp.year = unique(year(Tmp.time1))

## Remove Antarctic
Tmp.data[,which(i.lat< (-60)),] = NA

## Anomalies
Tmp.data.mean = array(NA,c(length(i.lon),length(i.lat),length(Tmp.time1)))
for(i in 1:12) {
  temp = apply(Tmp.data[,,which(month(Tmp.time1) == i)],c(1,2),mean,na.rm = TRUE)
  Tmp.data.mean[,,which(month(Tmp.time1) == i)] = temp
}
Tmp.data = Tmp.data - Tmp.data.mean

## Grids
lon.diff = diff(i.lon)[1]
lat.diff = diff(i.lat)[1]

## To Terra:Rast
target_data = aperm(Tmp.data,c(2,1,3))
rm <- rast(target_data)
ext(rm) <- c(-lon.diff, 360+lon.diff, -lat.diff-90, lat.diff+90)
terra::time(rm) <- Ens.date

## Re sample
s <- rast(nrows=360, ncols=720, xmin=0, xmax=360, ymin=-90, ymax=90)
t.total_resample = terra::resample(rm,s, method = "bilinear")

## Write data
out.name = paste0("Z:/2023/Science10_SM/Output Data/Observation_Summary/GISS_rigrid.nc")
rr <-
  writeCDF(
    t.total_resample,
    out.name,
    overwrite = TRUE,
    varname = "Tmp",
    longname = "surface temperature",
    unit = "K"
  )

## Ensemble Member
temp = as.array(t.total_resample)
temp = aperm(temp,c(2,1,3))
Ens.Tmp.data[2,,,] = temp


# BEST ------------------------------------------------

## Global Temperature
Noah=nc_open("Z:/2023/Input data/Berkeley Temperature/Complete_TAVG_LatLong1.nc")
Tmp.data=ncvar_get(Noah,"temperature")
i.lon=ncvar_get(Noah, "longitude")
i.lat=ncvar_get(Noah, "latitude")
i.time=ncvar_get(Noah, "time")
Tmp.time1 = date_decimal(i.time)
Tmp.time1 = ymd(year(Tmp.time1)*10000+month(Tmp.time1)*100+1)
Tmp.year = unique(year(Tmp.time1))
nc_close(Noah)

## Available Years
Tmp.year = unique(year(Tmp.time1))

## Change Resolution
i.lon = i.lon[c(181:360,1:180)]
i.lon[which(i.lon < 0)] = i.lon[which(i.lon < 0)] + 360
Tmp.data = Tmp.data[c(181:360,1:180),,]

## Time
Tmp.data = Tmp.data[,,which(year(Tmp.time1)%in% c(1901:2022))]
Tmp.time1 = Tmp.time1[which(year(Tmp.time1)%in% c(1901:2022))]
Tmp.year = unique(year(Tmp.time1))

## Remove Antarctic
Tmp.data[,which(i.lat< (-60)),] = NA

## Anomalies
Tmp.data.mean = array(NA,c(length(i.lon),length(i.lat),length(Tmp.time1)))
for(i in 1:12) {
  temp = apply(Tmp.data[,,which(month(Tmp.time1) == i)],c(1,2),mean,na.rm = TRUE)
  Tmp.data.mean[,,which(month(Tmp.time1) == i)] = temp
}
Tmp.data = Tmp.data - Tmp.data.mean

## Grids
lon.diff = diff(i.lon)[1]
lat.diff = diff(i.lat)[1]

## To Terra:Rast
target_data = aperm(Tmp.data,c(2,1,3))
rm <- rast(target_data)
ext(rm) <- c(-lon.diff, 360+lon.diff, -lat.diff-90, lat.diff+90)
terra::time(rm) <- Ens.date

## Re sample
s <- rast(nrows=360, ncols=720, xmin=0, xmax=360, ymin=-90, ymax=90)
t.total_resample = terra::resample(rm,s, method = "bilinear")

## Write data
out.name = paste0("Z:/2023/Science10_SM/Output Data/Observation_Summary/BEST_rigrid.nc")
rr <-
  writeCDF(
    t.total_resample,
    out.name,
    overwrite = TRUE,
    varname = "Tmp",
    longname = "surface temperature",
    unit = "K"
  )

## Ensemble Member
temp = as.array(t.total_resample)
temp = aperm(temp,c(2,1,3))
Ens.Tmp.data[3,,,] = temp

# Ensemble Mean -------------------------------------------------

## Ensemble Mean
i.lon = Ens.lon
i.lat = Ens.lat

## Ensemble Mean
for(j in 1:(122*12)) {
  kk = apply(Ens.Tmp.data[1:3,,,j],c(2,3),mean,na.rm = TRUE)
  Ens.Tmp.data[4,,,j] = kk
  print(j)
}

## Remove Land Masks
for(i in 1:length(i.lon)) {
  for (j in 1:length(i.lat)) {
    if (is.na(Land.mask[i, j])) {
      Ens.Tmp.data[, i, j, ] = NA
    }
  }
}

## Test
image.plot(Ens.Tmp.data[1,,,1],zlim = c(-10,10))
image.plot(Ens.Tmp.data[2,,,1],zlim = c(-10,10))
image.plot(Ens.Tmp.data[3,,,1],zlim = c(-10,10))
image.plot(Ens.Tmp.data[4,,,1],zlim = c(-10,10))

## Write data_Ensemble Member
out.name = paste0("Z:/2023/Science10_SM/Output Data/Observation_Summary/Ens.Tmp.member.nc")
ge <- ncdim_def(name = "ge", units = "ge", vals = 1:4)
x <- ncdim_def(name = "lon", units = "degrees_east", vals = i.lon)
y <- ncdim_def(name = "lat", units = "degrees_north", vals = i.lat)
t <- ncdim_def(name = "time", units = "Days since 17000101",vals = as.numeric(Ens.date -ymd(17010101)), unlim = TRUE)
var1 <- ncvar_def("tmp", "degree", list(ge, x, y, t), NA, prec = "float")
vars <- list(var1)
ncnew <- nc_create(out.name,  vars)
ncvar_put(ncnew, var1, Ens.Tmp.data)
ncatt_put(ncnew, "lon", "standard_name", "longitude", prec = "text")
ncatt_put(ncnew, "lat", "standard_name", "latitude", prec = "text")
nc_close(ncnew)

## Write data_Mean
out.name = paste0("Z:/2023/Science10_SM/Output Data/Observation_Summary/Ens.Tmp.mean.nc")
x <- ncdim_def(name = "lon", units = "degrees_east", vals = i.lon)
y <- ncdim_def(name = "lat", units = "degrees_north", vals = i.lat)
t <- ncdim_def(name = "time", units = "Days since 17000101",vals = as.numeric(Ens.date -ymd(17010101)), unlim = TRUE)
var1 <- ncvar_def("tmp", "degree", list(x, y, t), NA, prec = "float")
vars <- list(var1)
ncnew <- nc_create(out.name,  vars)
ncvar_put(ncnew, var1, Ens.Tmp.data[4,,,])
ncatt_put(ncnew, "lon", "standard_name", "longitude", prec = "text")
ncatt_put(ncnew, "lat", "standard_name", "latitude", prec = "text")
nc_close(ncnew)

# Combine GLST ----------------------------------------------------

## Global Temperature
Noah=nc_open("Z:/2023/Science10_SM/Output Data/Observation_Summary/Ens.Tmp.member.nc")
Ens.Tmp.data=ncvar_get(Noah,"tmp")
i.lon=ncvar_get(Noah, "lon")
i.lat=ncvar_get(Noah, "lat")
i.time=ncvar_get(Noah, "time")
Tmp.time1=i.time+ymd(17010101)
Tmp.time1 = ymd(year(Tmp.time1)*10000+month(Tmp.time1)*100+1)
Tmp.year = unique(year(Tmp.time1))
nc_close(Noah)

## Mid-latitude Temperature
A.Weight = array(rep(cos(i.lat*pi/180),each = length(i.lon)),c(length(i.lon),length(i.lat)))
CRU_mid_tmp_1 = data.frame(
  "Date" = Tmp.time1,
  "GLST_CRU" = areal.mean(z = Ens.Tmp.data[1,,,], A.Weight = A.Weight),
  "GLST_GISS" = areal.mean(z = Ens.Tmp.data[2,,,], A.Weight = A.Weight),
  "GLST_BEST" = areal.mean(z = Ens.Tmp.data[3,,,], A.Weight = A.Weight),
  "GLST_MEAN" = areal.mean(z = Ens.Tmp.data[4,,,], A.Weight = A.Weight)
)

## Total
GLST_Ens = CRU_mid_tmp_1 %>% 
  dplyr::mutate(year = year(Date), month = month(Date)) %>%
  dplyr::filter(month %in%  c(6:8) & year %in% c(1901:2022)) %>%
  dplyr::group_by(year) %>%
  dplyr::summarize(across(c(GLST_CRU, GLST_GISS, GLST_BEST,GLST_MEAN),  ~ mean(.x))) %>%
  dplyr::mutate(across(
    c(GLST_CRU, GLST_GISS, GLST_BEST,GLST_MEAN),
    ~ pass.filter.NA.mean(
      .x,
      W = 15,
      type = "high",
      method = c("Butterworth")
    )
  ))

## Write
write.csv(GLST_Ens,"Output Data/Observation_Summary/GLST_Ens.csv")

## Total Raw
GLST_Ens_Raw = CRU_mid_tmp_1 %>% 
  dplyr::mutate(year = year(Date), month = month(Date)) %>%
  dplyr::filter(month %in%  c(6:8) & year %in% c(1901:2022)) %>%
  dplyr::group_by(year) %>%
  dplyr::summarize(across(c(GLST_CRU, GLST_GISS, GLST_BEST,GLST_MEAN),  ~ mean(.x)))
  
## Write
write.csv(GLST_Ens_Raw,"Output Data/Observation_Summary/GLST_Ens_Raw.csv")

## Plot
plot(GLST_Ens$year,GLST_Ens$GLST_CRU,type = "l")
lines(GLST_Ens$year,GLST_Ens$GLST_GISS,col = "red")
lines(GLST_Ens$year,GLST_Ens$GLST_BEST,col = "red")
lines(GLST_Ens$year,GLST_Ens$GLST_MEAN,col = "blue")

## Plot
plot(GLST_Ens_Raw$year,GLST_Ens_Raw$GLST_CRU,type = "l")

# End -------------------------------------------------------------
