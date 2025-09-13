

cor30.sel=function(a1,a2,sel){
  a1 = detrend_linear(a1)
  a2 = detrend_linear(a2)
  total.data=data.frame("a1"=a1,"a2"=a2)
  total.data=total.data[sel,]
  total.data=total.data[complete.cases(total.data),]
  if(NROW(total.data)<=12) {return(c(NA,NA))}
  cor=cor.test(total.data$a1,total.data$a2)
  return(c(cor$estimate,cor$p.value))
}


cor30.1=function(a1,a2,sel){
  total.data=data.frame("a1"=a1,"a2"=a2)
  total.data=total.data[sel,]
  total.data=total.data[complete.cases(total.data),]
  if(NROW(total.data)<=12) {return(c(NA,NA))}
  cor=cor.test(total.data$a1,total.data$a2)
  return(c(cor$estimate,cor$p.value))
}


cor30.1.df=function(a1,a2,sel,Tn = 88){
  
  ## Data
  total.data=data.frame("a1"=a1,"a2"=a2)
  total.data=total.data[sel,]
  total.data=total.data[complete.cases(total.data),]
  
  ## Check
  if(NROW(total.data)<=12) {return(c(NA,NA,NA))}
  
  ## Correlation
  cor=cor.test(total.data$a1,total.data$a2)
  
  ## P-value
  L1 = ((1-cor$estimate^2)/(Tn - 2))^(1/2)
  t_value = cor$estimate/L1
  cor_df_p_1 = 2*stats::pt(q = t_value,df = Tn - 2)
  cor_df_p_2 = 2*(1-stats::pt(q = t_value,df = Tn - 2))
  cor_df_p = min(cor_df_p_1,cor_df_p_2)
  temp = as.numeric(c(cor$estimate,cor$p.value,cor_df_p))
  return(temp)
}

# # ## Test
# cor30.1.df(rnorm(20),rnorm(20))
# cor30.1.df(rnorm(20)+1:20,rnorm(20)+1:20)
# temp = cor.test(rnorm(20)+1:10,rnorm(20)+1:10)
# temp = cor.test(rnorm(20),rnorm(20))
# temp
# temp$parameter
# temp$statistic
# min(2*stats::pt(q = temp$statistic,df = temp$parameter),2*(1-stats::pt(q = temp$statistic,df = temp$parameter)))
# min(2*stats::pt(q = temp$statistic,df = temp$parameter+100),2*(1-stats::pt(q = temp$statistic,df = temp$parameter+100)))

cor7=function(a1,a2){
  
  total.data=data.frame("a1"=a1,"a2"=a2)
  total.data=total.data[complete.cases(total.data),]
  if(NROW(total.data)<=7) {return(list("estimate"=NA,"p.value"=NA))}
  cor=cor.test(total.data$a1,total.data$a2)
  return(cor)
}


cor30=function(a1,a2){
  
  total.data=data.frame("a1"=a1,"a2"=a2)
  total.data=total.data[complete.cases(total.data),]
  if(NROW(total.data)<=30) {return(list("estimate"=NA,"p.value"=NA))}
  cor=cor.test(total.data$a1,total.data$a2,method = c("pearson"))
  return(cor)
}

cor25.1=function(a1,a2){
  total.data=data.frame("a1"=a1,"a2"=a2)
  total.data=total.data[complete.cases(total.data),]
  if(NROW(total.data)<=25) {return(c(NA,NA))}
  cor=cor.test(total.data$a1,total.data$a2)
  return(c(cor$estimate,cor$p.value))
}


cor7.1=function(a1,a2){
  total.data=data.frame("a1"=a1,"a2"=a2)
  total.data=total.data[complete.cases(total.data),]
  if(NROW(total.data)<=7) {return(c(NA,NA))}
  cor=cor.test(total.data$a1,total.data$a2)
  return(c(cor$estimate,cor$p.value))
}


cor50.1=function(a1,a2){
  total.data=data.frame("a1"=a1,"a2"=a2)
  total.data=total.data[complete.cases(total.data),]
  if(NROW(total.data)<=50) {return(list("estimate"=NA,"p.value"=NA))}
  cor=cor.test(total.data$a1,total.data$a2)
  return(c(cor$estimate,cor$p.value))
}


cor.non.na=function(a1,a2){
  total.data=data.frame("a1"=a1,"a2"=a2)
  if(length(which(is.na(total.data)))>0) {return(list("estimate"=NA,"p.value"=NA))}
  
  cor=cor.test(total.data$a1,total.data$a2)
  return(cor)
}
cor.non.na2=function(a1,a2){
  total.data=data.frame("a1"=a1,"a2"=a2)
  if(length(which(is.na(total.data)))>2) {return(list("estimate"=NA,"p.value"=NA))}
  
  cor=cor.test(total.data$a1,total.data$a2)
  return(cor)
}

