
## Parameters
o.lon = seq(1,359,by = 2)
o.lat = seq(-89,89,by = 2)

## Labels
ylab_GLSAT = bquote("Global "*italic({T}[a])*" anomaly")
ylab_GLSAT_Unit = bquote("Global "*italic({T}[a])*" anomaly (°C)")

## Packages
.libPaths()
.libPaths(new = "Z:/Software/R/Library",include.site = FALSE)
library(interp)
library(ggplot2)
library(terra)
library(reshape)
library(akima)
library(raster)
library(fields)
library(ggthemes)
library(readxl)
library(pracma)
library(dplyr)
library(leaps)
library(tidyr)
library(cowplot)
library(ncdf4)
library(fields)
library(lubridate)
library (ridge)
library(forecast)
library(burnr)
library(Cairo)
library(dplR)
library(sf)
library(terra)
library(ppcor)
library(RColorBrewer)
library(abind)
library(s2dv)
library(readxl)
library(writexl)

## Functions
functions.names=list.files(path = "Code/functions/")
for(i in 1:length(functions.names)){
  source(paste0("Code/functions/",functions.names[i]))
}

## Read Global Map
## Acknowledgement to Natural Earth https://www.naturalearthdata.com/about/terms-of-use/.
con.boundary <- st_read(dsn = 'Output Data/ne_110m_land/ne_110m_land.shp')
con.boundary1 <- as_Spatial(con.boundary, IDs=con.boundary$scalerank)
countymap <- fortify(con.boundary1)
con.boundary1 <- Trans.map(countymap)

# theme
colors_temp1 = brewer.pal(n=7,"Set1")
colors_temp2 = tim.colors(64)[64:1]
P_Xu_F1 = theme(
  plot.margin = margin(5, 10, 5, 10, "pt"),
  panel.background = element_blank(),
  panel.grid = element_blank(),
  panel.border = element_rect(linewidth = unit(0.5, "pt"),fill = NA, color = "black"),
  axis.line = element_blank(),
  axis.ticks = element_line(color= "black",linewidth = unit(0.3, "pt")),
  axis.text = element_text(color= "black"),
  axis.title = element_text(color= "black"),
  legend.title = element_blank(),
  legend.background = element_blank(),
  legend.key = element_blank()
)

## All themes
theme_figure1 =  theme(
  plot.margin = margin(3, 15, 3, 3, "pt"),
  legend.position = "none",
  legend.title = element_blank(),
  legend.background = element_rect(fill = NA),
  legend.key = element_blank(),
  panel.border = element_blank(),
  panel.background = element_blank(),
  panel.grid = element_blank(),
  axis.line.y.right = element_line(linewidth = 0.4, color = "black"),
  axis.line.x.bottom = element_line(linewidth = 0.4, color = "black"),
  axis.line.y.left = element_line(linewidth = 0.4, color = "black"),
  axis.line.x.top = element_line(linewidth = 0.4, color = "black"),
  axis.title.y.right = element_blank(),
  axis.title.x.bottom = element_text(color = "black"),
  axis.title.y.left = element_text(color = "black"),
  axis.title.x.top = element_blank(),
  axis.ticks.y.right = element_blank(),
  axis.ticks.x.bottom = element_line(linewidth = 0.4, color = "black"),
  axis.ticks.y.left = element_line(linewidth = 0.4, color = "black"),
  axis.ticks.x.top = element_blank(),
  axis.ticks.length.y.right = unit(0, "pt"),
  axis.ticks.length.x.bottom = unit(4, "pt"),
  axis.ticks.length.y.left = unit(4, "pt"),
  axis.ticks.length.x.top = unit(0, "pt"),
  axis.text.y.right = element_blank(),
  axis.text.x.bottom = element_text(color = "black"),
  axis.text.y.left = element_text(color = "black"),
  axis.text.x.top = element_blank()
)

## All themes 2
theme_figure2 =  theme(
  plot.margin = margin(3, 15, 3, 3, "pt"),
  legend.position = "none",
  legend.title = element_blank(),
  legend.background = element_rect(fill = NA),
  legend.key = element_blank(),
  panel.border = element_blank(),
  panel.background = element_blank(),
  panel.grid = element_blank(),
  axis.line.y.right = element_line(linewidth = 0.3, color = "black"),
  axis.line.x.bottom = element_line(linewidth = 0.3, color = "black"),
  axis.line.y.left = element_line(linewidth = 0.3, color = "black"),
  axis.line.x.top = element_line(linewidth = 0.3, color = "black"),
  axis.title.y.right = element_blank(),
  axis.title.x.bottom = element_text(color = "black"),
  axis.title.y.left = element_text(color = "black"),
  axis.title.x.top = element_blank(),
  axis.ticks.y.right = element_blank(),
  axis.ticks.x.bottom = element_line(linewidth = 0.3, color = "black"),
  axis.ticks.y.left = element_line(linewidth = 0.3, color = "black"),
  axis.ticks.x.top = element_blank(),
  axis.ticks.length.y.right = unit(0, "pt"),
  axis.ticks.length.x.bottom = unit(4, "pt"),
  axis.ticks.length.y.left = unit(4, "pt"),
  axis.ticks.length.x.top = unit(0, "pt"),
  axis.text.y.right = element_blank(),
  axis.text.x.bottom = element_text(color = "black"),
  axis.text.y.left = element_text(color = "black"),
  axis.text.x.top = element_blank()
)

# End -------------------------------------------------------------

