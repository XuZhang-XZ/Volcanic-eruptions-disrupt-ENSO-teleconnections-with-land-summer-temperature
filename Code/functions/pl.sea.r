


pl.sea.ENSO=function(pgm_PAGS2k,type,ylab = "Pressure (hPa)"){
  
  ## top-left
  if(type==1) {
    pp=theme(axis.title.x = element_blank(),
             axis.text.x = element_blank(),
             plot.margin = margin(t = 5.5, r = 5.5, b = 5.5, l = 5.5, unit = "pt"))
  }
  ## top-right
  if(type==2) {
    pp=theme(axis.title.x = element_blank(),
             axis.text.x = element_blank(),
             axis.title.y = element_blank(),
             plot.margin = margin(t = 5.5, r = 5.5, b = 5.5, l = 5.5, unit = "pt"))
  }
  ## bottom-left
  if(type==3) {
    pp=theme(plot.margin = margin(t = 5.5, r = 4.5, b = 5.5, l = 4.5, unit = "pt"))
  }
  ## bottom-right
  if(type==4) {
    pp=theme(axis.title.y = element_blank(),
             plot.margin = margin(t = 5.5, r = 5.5, b = 5.5, l = 5.5, unit = "pt"))
  }
  
  x=pgm_PAGS2k
  Each.series = as.data.frame(t(x$observed)) %>%
    dplyr::mutate(lag = x$departure$lag) %>%
    dplyr::relocate(lag) %>%
    tidyr::pivot_longer(cols = -c(1))

  p <- ggplot()+
    geom_path(data = Each.series,color = "gray10",aes(x = lag, y = value, group = name))+
    geom_path(data = x$departure, aes(y = mean, x = lag),col= "red",size = 0.5)+
    geom_point(data = x$departure, aes(y = mean, x = lag),col= "black",shape = 16,size = 1)+
    geom_path(data = x$departure,aes(y = upper_99_perc, x = lag),col = "gray10",linetype = "dashed",size = 0.2)+
    geom_path(data = x$departure,aes(y = lower_99_perc, x = lag),col = "gray10",linetype = "dashed",size = 0.2)+
    geom_path(data = x$departure,aes(y = upper_95_perc, x = lag),col = "gray10",linetype = "dashed",size = 0.2)+
    geom_path(data = x$departure,aes(y = lower_95_perc, x = lag),col = "gray10",linetype = "dashed",size = 0.2)+
    geom_hline(yintercept = 0,linetype = "longdash",size = 0.2)+
    geom_vline( xintercept = 0, col = "red", size = 0.1, linetype = "dashed")+
    xlab("Months relative to eruption")+ylab(ylab)+
    scale_x_continuous(breaks = seq(-8,8,2),limits = c(-9,9),expand = c(0,0))+
    scale_y_continuous(breaks = seq(-1.5,3,0.5),limits = c(-1.8,3),expand = c(0,0))+
    theme(panel.border=element_rect(colour = "black",fill=NA,size = 0.4),
          panel.background = element_blank(),
          axis.text = element_text(colour = "black"),
          axis.title = element_text(colour = "black"),
          axis.ticks = element_line(colour = "black",size = 0.3)
    )+
    pp
  return(p)
}


pl.sea.mean=function(pgm_PAGS2k,type,ylab = "Pressure (hPa)"){
  
  ## top-left
  if(type==1) {
    pp=theme(axis.title.x = element_blank(),
             axis.text.x = element_blank(),
             plot.margin = margin(t = 5.5, r = 5.5, b = 5.5, l = 5.5, unit = "pt"))
  }
  ## top-right
  if(type==2) {
    pp=theme(axis.title.x = element_blank(),
             axis.text.x = element_blank(),
             axis.title.y = element_blank(),
             plot.margin = margin(t = 5.5, r = 5.5, b = 5.5, l = 5.5, unit = "pt"))
  }
  ## bottom-left
  if(type==3) {
    pp=theme(plot.margin = margin(t = 5.5, r = 4.5, b = 5.5, l = 4.5, unit = "pt"))
  }
  ## bottom-right
  if(type==4) {
    pp=theme(axis.title.y = element_blank(),
             plot.margin = margin(t = 5.5, r = 5.5, b = 5.5, l = 5.5, unit = "pt"))
  }
  
  x=pgm_PAGS2k
  p <- ggplot(data = x$departure, aes_string(y = "mean", x = "lag"))+
    geom_path(col= "red",size = 0.5)+
    geom_point(col= "black",shape = 16,size = 1)+
    geom_path(aes_string(y = "upper_99_perc"),col = "gray10",linetype = "dashed",size = 0.2)+
    geom_path(aes_string(y = "lower_99_perc"),col = "gray10",linetype = "dashed",size = 0.2)+
    geom_path(aes_string(y = "upper_95_perc"),col = "gray10",linetype = "dashed",size = 0.2)+
    geom_path(aes_string(y = "lower_95_perc"),col = "gray10",linetype = "dashed",size = 0.2)+
    geom_hline(yintercept = 0,linetype = "longdash",size = 0.2)+
    geom_vline(
      xintercept = 0,
      col = "red",
      size = 0.1,
      linetype = "dashed"
    )+
    xlab("Months relative to eruption")+ylab(ylab)+
    scale_x_continuous(breaks = seq(-12,48,12),limits = c(-12,49),expand = c(0,0))+
    scale_y_continuous(breaks = seq(-1.5,1.5,0.5),limits = c(-1.8,1.8),expand = c(0,0))+
    # theme_bw() +
    theme(panel.border=element_rect(colour = "black",fill=NA,size = 0.4),
          panel.background = element_blank(),
          axis.text = element_text(colour = "black"),
          axis.title = element_text(colour = "black"),
          axis.ticks = element_line(colour = "black",size = 0.3)
          )+
    # theme(panel.border = element_rect(color= "black",fill=NA))+
    # panel.grid = element_blank(),
    pp
  return(p)
}




# pl.sea.mean=function(pgm_PAGS2k,type){
#   
#   ## top-left
#   if(type==1) {
#     pp=theme(axis.title.x = element_blank(),
#              axis.text.x = element_blank(),
#              plot.margin = margin(t = 5.5, r = 5, b = 5.5, l = 5.5, unit = "pt"))
#   }
#   ## top-right
#   if(type==2) {
#     pp=theme(axis.title.x = element_blank(),
#              axis.text.x = element_blank(),
#              axis.title.y = element_blank(),
#              plot.margin = margin(t = 5.5, r = 5, b = 5.5, l = 1, unit = "pt"))
#   }
#   ## bottom-left
#   if(type==3) {
#     pp=theme(plot.margin = margin(t = 5.5, r = 5, b = 5.5, l = 1, unit = "pt"))
#   }
#   ## bottom-right
#   if(type==4) {
#     pp=theme(axis.title.y = element_blank(),
#              plot.margin = margin(t = 5.5, r = 5, b = 5.5, l = 5.5, unit = "pt"))
#   }
#   
#   x=pgm_PAGS2k
#   p <- ggplot2::ggplot(x$actual, ggplot2::aes_string(y = "mean", x = "lag"))
#   p <- (
#     p + ggplot2::geom_col()
#     + ggplot2::geom_line(ggplot2::aes_string(y = "upper_99_perc"))
#     # + ggplot2::geom_point(ggplot2::aes_string(y = "upper_99_perc"))
#     + ggplot2::geom_line(ggplot2::aes_string(y = "lower_99_perc"))
#     # + ggplot2::geom_point(ggplot2::aes_string(y = "lower_99_perc"))
#     + ggplot2::geom_line(ggplot2::aes_string(y = "lower_95_perc"),
#                          linetype = "dashed")
#     # + ggplot2::geom_point(ggplot2::aes_string(y = "upper_95_perc"))
#     + ggplot2::geom_line(ggplot2::aes_string(y = "upper_95_perc"),
#                          linetype = "dashed")
#     # + ggplot2::geom_point(ggplot2::aes_string(y = "lower_95_perc"))+
#       geom_vline(xintercept = 0,col="red",linetype = "dashed")
#     + ggplot2::ylab("Mean departure")
#     + ggplot2::xlab("Lag")
#     + ggplot2::theme_bw() +
#       # scale_y_continuous(breaks = c(-))
#       xlab("Months relative to Eruption") +
#       ylab("Pressure (hPa)")+
#       pp
#   )
#   return(p)
# }




plot.SOD=function(V.year,SOD){
  
  time.ge=which(SOD.time>=(V.year-1)&SOD.time<=(V.year+3))
  SOD1=SOD[,time.ge]
  plot.data=data.frame("Year"=rep(SOD.time[time.ge],each=length(SOD.lat)),
                       "Year.min"=rep(c(SOD.time[time.ge]),each=length(SOD.lat)),
                       "Year.max"=rep(c(SOD.time[time.ge+1]),each=length(SOD.lat)),
                       "Value"=as.numeric(SOD1),
                       "Lat"=rep(SOD.lat,times=length(time.ge)),
                       "Lat.max"=rep(c(90,SOD.lat[-96]),times=length(time.ge)),
                       "Lat.min"=rep(c(SOD.lat[-1],-90),times=length(time.ge)))
  
  p1=ggplot()+
    geom_rect(data = plot.data,
              aes(xmin=Year.min,xmax=Year.max,ymin=Lat.min,ymax=Lat.max,fill=Value))+
    scale_fill_stepsn(colors=c("white",colorns8),limits=c(0,0.5),breaks=c(0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45),na.value = "white")+
    scale_x_continuous(expand = c(0,0))+
    scale_y_continuous(expand = c(0,0))+
    xlab("year (AD)")+ylab("Latitude (degree)")+ggtitle(paste0("Eruption in ",V.year))+
    theme_bw()+
    theme(
      legend.position = "bottom",
      legend.key.width = unit(70, "pt"),
      legend.key.height = unit(10, "pt"),
      legend.title = element_blank()
    )
}
