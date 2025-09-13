

## Functions for Summer and pass-filter
Field.sea.pass = function(Tmp.data,Series.Time,Sea = "Summer",filter_high = TRUE,scaless = FALSE){
  
  ## Winter
  if(Sea == "Winter") {
    Tmp_1 = Tmp.data[,,which(month(Series.Time) == 1)]
    Tmp_2 = Tmp.data[,,which(month(Series.Time) == 2)]
    length.years = dim(Tmp_1)[3]
    Tmp_12_temp = Tmp.data[,,which(month(Series.Time) == 12)]
    Tmp_12 = Tmp_1
    Tmp_12[,,1] = NA
    Tmp_12[,,2:length.years] = Tmp_12_temp[,,c(1:(length.years-1))]
    Tmp_Summer = (Tmp_12+Tmp_1+Tmp_2)/3
  }
  
  ## Spring
  if(Sea == "Spring") {
    Tmp_3 = Tmp.data[,,which(month(Series.Time) == 3)]
    Tmp_4 = Tmp.data[,,which(month(Series.Time) == 4)]
    Tmp_5 = Tmp.data[,,which(month(Series.Time) == 5)]
    Tmp_Summer = (Tmp_3+Tmp_4+Tmp_5)/3
  }
  
  ## Summer
  if(Sea == "Summer") {
    Tmp_6 = Tmp.data[,,which(month(Series.Time) == 6)]
    Tmp_7 = Tmp.data[,,which(month(Series.Time) == 7)]
    Tmp_8 = Tmp.data[,,which(month(Series.Time) == 8)]
    Tmp_Summer = (Tmp_6+Tmp_7+Tmp_8)/3
  }
  
  ## Summer
  if(Sea == "Pre") {
    Tmp_1 = Tmp.data[,,which(month(Series.Time) == 1)]
    Tmp_2 = Tmp.data[,,which(month(Series.Time) == 2)]
    Tmp_3 = Tmp.data[,,which(month(Series.Time) == 3)]
    Tmp_4 = Tmp.data[,,which(month(Series.Time) == 4)]
    Tmp_5 = Tmp.data[,,which(month(Series.Time) == 5)]
    Tmp_6 = Tmp.data[,,which(month(Series.Time) == 6)]
    Tmp_7 = Tmp.data[,,which(month(Series.Time) == 7)]
    Tmp_8 = Tmp.data[,,which(month(Series.Time) == 8)]
    Tmp_Summer = (Tmp_1+Tmp_2+Tmp_3+Tmp_4+Tmp_5+Tmp_6+Tmp_7+Tmp_8)/3
  }
  
  ## Annual
  if(Sea == "Annual") {
    
    Tmp_1 = Tmp.data[,,which(month(Series.Time) == 1)]
    for(i in 2:12) {
      Tmp_1 = Tmp_1 + Tmp.data[,,which(month(Series.Time) == i)]
    }
    Tmp_Summer = Tmp_1/12
    
  }
  
  ## Pass-filter
  if(filter_high == TRUE){
    Tmp_Summer = apply(Tmp_Summer,c(1,2),Filter.NA, W = 15)
    Tmp_Summer = aperm(Tmp_Summer,c(2,3,1))
  }
  if(filter_high == FALSE){
    Tmp_Summer = Tmp_Summer
  }
  if(filter_high == "Linear"){
    Tmp_Summer = apply(Tmp_Summer,c(1,2),detrend_linear)
    Tmp_Summer = aperm(Tmp_Summer,c(2,3,1))
  }

  ## Scale
  if(scaless == TRUE){
    Tmp_Summer = apply(Tmp_Summer,c(1,2),base::scale,center = TRUE, scale = TRUE)
  } else {
    Tmp_Summer = apply(Tmp_Summer,c(1,2),base::scale,center = TRUE, scale = FALSE)
  }
  Tmp_Summer = aperm(Tmp_Summer,c(2,3,1))
  
  ## Return
  Tmp_Summer
}




## Functions for Summer and pass-filter
Field.summer.std.pass = function(Tmp.data){
  
  ## Summer
  Tmp_6 = Tmp.data[,,which(month(Tmp.time1) == 6)]
  Tmp_7 = Tmp.data[,,which(month(Tmp.time1) == 7)]
  Tmp_8 = Tmp.data[,,which(month(Tmp.time1) == 8)]
  Tmp_Summer = (Tmp_6+Tmp_7+Tmp_8)/3
  
  ## Pass-filter
  Tmp_Summer = apply(Tmp_Summer,c(1,2),Filter.NA)
  Tmp_Summer = aperm(Tmp_Summer,c(2,3,1))
  Tmp_Summer = apply(Tmp_Summer,c(1,2),base::scale,center = TRUE, scale = FALSE)
  # Tmp_Summer = apply(Tmp_Summer,c(1,2),base::scale,center = TRUE, scale = TRUE)
  Tmp_Summer = aperm(Tmp_Summer,c(2,3,1))
  Tmp_Summer
}
