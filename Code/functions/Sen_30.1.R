

# ## Test
# a1 = rnorm(100)
# a2 = rnorm(100)
# Sen_30.1(a1,a2,sel = 1:10)

# a1 = Tmp_Summer_Sel[1,230,]
# a2 = A.NINO$NINO34


Sen_30.1=function(a1,a2,sel){
  
  total.data=data.frame("a1"=a1,"a2"=a2)
  total.data=total.data[sel,]
  total.data=total.data[complete.cases(total.data),]
  if (NROW(total.data) <= 12) {
    kk = c(NA,NA)
    return(kk)
  }
  kk.lm = lm(a1~a2, data = total.data)
  kk.lm.summary = summary(kk.lm)
  temp = kk.lm.summary[["coefficients"]]
  Sen = as.numeric(temp[2,1])
  Sen_p = as.numeric(temp[2,4])
  kk = c(Sen,Sen_p)
  return(kk)
  
}

Sen_30.2=function(a1,a2,sel){
  
  total.data=data.frame("a1"=a1,"a2"=a2)
  total.data=total.data[sel,]
  total.data=total.data[complete.cases(total.data),]
  if (NROW(total.data) <= 12) {
    kk = c(NA,NA,NA,NA)
    return(kk)
  }
  kk.lm = lm(a1~a2, data = total.data)
  kk.lm.summary = summary(kk.lm)
  temp = kk.lm.summary[["coefficients"]]
  Sen = as.numeric(temp[2,1])
  Sen_p = as.numeric(temp[2,4])
  
  ## T test
  Mean_a1 = mean(total.data$a1)
  t.results = t.test(total.data$a1)
  Mean_a1_p = t.results$p.value
  
  kk = c(Sen,Sen_p,Mean_a1,Mean_a1_p)
  return(kk)
  
}


# a1 = rnorm(100)
# a2 = rnorm(100)
# sel1 = 1:50
# sel2 = 51:1000


Sen_30.3=function(a1,a2,sel1,sel2){
  
  ## Data
  total.data=data.frame("a1"=a1,"a2"=a2)
  
  ## Scale
  total.data$a1 = as.numeric(scale(total.data$a1))
  
  ## Check
  if (NROW(total.data) <= 10) {
    kk = rep(NA,8)
    return(kk)
  }
  
  ## Sensitivity First
  total.data1=total.data[sel1,]
  total.data1=total.data1[complete.cases(total.data1),]
  if (NROW(total.data1) <= 10) {
    kk = rep(NA,8)
    return(kk)
  }
  kk.lm1 = lm(a1~a2, data = total.data1)
  kk.lm.summary1 = summary(kk.lm1)
  temp1 = kk.lm.summary1[["coefficients"]]
  Sen1 = as.numeric(temp1[2,1])
  Sen1_p = as.numeric(temp1[2,4])
  
  ## Sensitivity Second
  total.data2=total.data[sel2,]
  total.data2=total.data2[complete.cases(total.data2),]
  if (NROW(total.data2) <= 10) {
    kk = rep(NA,8)
    return(kk)
  }
  kk.lm2 = lm(a1~a2, data = total.data2)
  kk.lm.summary2 = summary(kk.lm2)
  temp2 = kk.lm.summary2[["coefficients"]]
  Sen2 = as.numeric(temp2[2,1])
  Sen2_p = as.numeric(temp2[2,4])
  
  ## Mean First
  Mean_a1 = mean(total.data1$a1)
  Mean_a2 = mean(total.data2$a1)
  
  ## T test
  t.results = t.test(total.data1$a1)
  Mean_a1_p = t.results$p.value
  t.results = t.test(total.data2$a1)
  Mean_a2_p = t.results$p.value
  
  # ## Select
  # total.data=total.data[complete.cases(total.data),]
  
  # ## Uncertainty
  # Mean_a1_R = NA
  # Mean_a2_R = NA
  # for(i in 1:100) {
  #   Mean_a1_R[i] = mean(sample(total.data$a1,NROW(total.data1),replace = T))
  #   Mean_a2_R[i] = mean(sample(total.data$a1,NROW(total.data2),replace = T))
  # }
  # Mean_a1_R_Range = quantile(Mean_a1_R,probs = c(0.05,0.95))
  # Mean_a2_R_Range = quantile(Mean_a2_R,probs = c(0.05,0.95))
 
  # ##T test
  # if(Mean_a1 > Mean_a1_R_Range[1] & Mean_a1 < Mean_a1_R_Range[2]) {
  #   Mean_a1_p = 0.9
  # } else {
  #   Mean_a1_p = 0.04
  # }
  # if(Mean_a2 > Mean_a2_R_Range[1] & Mean_a2 < Mean_a2_R_Range[2]) {
  #   Mean_a2_p = 0.9
  # } else {
  #   Mean_a2_p = 0.04
  # }
  
  # ## T test
  # t.results = t.test(total.data1$a1,total.data2$a1)
  # Mean_a1_p = Mean_a2_p = t.results$p.value

  ## Return
  kk = c(Sen1,Sen1_p,Sen2,Sen2_p,
         Mean_a1,Mean_a1_p,Mean_a2,Mean_a2_p)
  return(kk)
  
}

