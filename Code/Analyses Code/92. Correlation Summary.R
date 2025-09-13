

# Plot Spatial Correlations ------------------------------------------------------------

## Correlations in Observations
Out.cor = read.csv("Output Data/Plot_Data/Output.composite_MEAN.csv",row.names = 1)

## Sig regions
sig.lon = seq(0.25,359.75,by = 3)
sig.lat = seq(-89.75,89.75,by = 3)
Out.cor.sig = Out.cor[which(Out.cor$lon%in%sig.lon&Out.cor$lat%in%sig.lat),]

## Parameters
p.names=paste0("Cor_Tmp_",c(1:16))
cols.plot = c("Tmp_1_NVol_Cor","Tmp_1_Vol_Cor")
cols.plot.p = paste0(cols.plot,"_p")
start.limit = rep(-0.65,20)
end.limit = -start.limit
plot.palette=rep(c("RdYlBu"),each=11)
i= 1

## Plot
for(i in 1:length(cols.plot)){
  assign(p.names[i],
         p1 <- ggplot() +
           geom_tile(
             data = Out.cor,
             aes(x = lon, y =lat,color=.data[[cols.plot[i]]],fill=.data[[cols.plot[i]]])
           ) +
           geom_point(
             data = Out.cor.sig%>%dplyr::filter(.data[[cols.plot.p[i]]]<0.05),color="black",shape = 16,size = 0.5,alpha = 0.5,
             aes(x = lon, y =lat)
           ) +
           geom_polygon(data=con.boundary1,
                        color = "gray30", fill = NA,linewidth=0.2,
                        aes(x = long, y = lat, group = group)) +
           annotate(geom="text",x = -Inf, y = -Inf, hjust = - 0.3, vjust = -0.5,
                    label = c("n = 88","n = 34")[i]) +
           scale_color_gradientn(
             colours = brewer.pal(n=11,plot.palette[i])[c(11:9,6,3:1)],
             limits = c(start.limit[i], end.limit[i]),
             breaks = seq(-0.6,0.6,0.1), 
             labels = round(seq(-0.6,0.6,0.1),1),
             oob = scales::squish,na.value = NA, name = "Correlation with Niño 3.4",
             guide = guide_colorbar(title.position = "top",title.hjust = 0.5)
           )+
           scale_fill_gradientn(
             colours = brewer.pal(n=11,plot.palette[i])[c(11:9,6,3:1)],
             limits = c(start.limit[i], end.limit[i]),
             breaks = seq(-0.6,0.6,0.1), 
             labels = round(seq(-0.6,0.6,0.1),1),
             oob = scales::squish,na.value = NA, name = "Correlation with Niño 3.4",
             guide = guide_colorbar(title.position = "top",title.hjust = 0.5)
           )+
           scale_x_continuous(breaks = seq(0,360,90),label = c("0°","90°E","180°","90°W","0°"),
                              sec.axis = sec_axis(~ .))+
           scale_y_continuous(breaks = seq(-60,90,30),label = c("60°S","30°S","0°S","30°N","60°N","90°N"),
                              sec.axis = sec_axis(~ .))+
           coord_quickmap(expand = FALSE,xlim = c(0,360),ylim = c(-60,90))+
           theme_figure2+
           theme(
             legend.position = "none",
             legend.key.width = unit(5,"pt"),
             legend.key.height = unit(35,"pt"),
             legend.direction = "vertical",
             plot.margin = margin(t = 10, r = 10, b = 10, l = 12, unit = "pt"),
             axis.title.x.bottom = element_blank(),
             axis.title.y.left = element_blank(),
             axis.text.x.bottom = element_text(color = "black",size = 8),
             axis.text.y.left = element_text(color = "black",size = 8)
           )
  )
}

## Legend
i = 1
p1 <- ggplot() +
  geom_tile(
    data = Out.cor,color=NA,
    aes(x = lon, y =lat,fill=.data[[cols.plot[i]]])
  ) +
  scale_fill_gradientn(
    colours = brewer.pal(n=11,plot.palette[i])[c(11:9,6,3:1)],
    limits = c(start.limit[i], end.limit[i]),
    breaks = seq(-0.6,0.6,0.1), 
    labels = round(seq(-0.6,0.6,0.1),1),
    oob = scales::squish,na.value = NA, name = "Correlation with Niño 3.4",
    guide = guide_colorbar(title.position = "top",title.hjust = 0.5)
  )+
  theme_figure2+
  theme(
    legend.position = "top",
    legend.key.width = unit(100,"pt"),
    legend.key.height = unit(6,"pt"),
    legend.direction = "horizontal",
    legend.title = element_text(),
    plot.margin = margin(t = 5, r = 5, b = 5, l = 5, unit = "pt"),
  )
legend1 = cowplot::get_plot_component(p1, 'guide-box-top', return_all = TRUE)
legend_Cor = plot_grid(legend1)

## Saving
p_Tmp_Cor = plot_grid(Cor_Tmp_1,Cor_Tmp_2,
                      nrow = 1,labels = c("a","b"),hjust = -0.1)
p_Tmp_Cor_1 = plot_grid(p_Tmp_Cor,legend_Cor,
                        ncol = 1,rel_heights = c(2.7,0.6))


# Plot Shifted Correlations --------------------------------------------

## Data
Output.cor = read.csv("Output Data/Plot_Data/Output.composite_MEAN.csv",row.names = 1)

## Select
Output.cor1 = Output.cor %>%
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

## Global Area
Output.composite.Area = Output.cor1 %>%
  dplyr::group_by(Tmp_1_Re) %>%
  dplyr::summarise(Area = sum(Area)) %>%
  arrange(desc(Tmp_1_Re)) %>%
  mutate(prop = Area / sum(Area) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop) %>%
  mutate(Text1 = paste0(round(prop,2), "%")) %>%
  mutate(Text2 = ifelse(prop > 10,Text1,NA)) %>%
  mutate(Dateset = "MEAN")

## Area
Output.composite.Area = Output.cor1 %>%
  dplyr::filter(lat > 23 | lat < (-23)) %>%
  dplyr::group_by(Tmp_1_Re) %>%
  dplyr::summarise(Area = sum(Area)) %>%
  arrange(desc(Tmp_1_Re)) %>%
  mutate(prop = Area / sum(Area) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop) %>%
  mutate(Text1 = paste0(round(prop,2), "%")) %>%
  mutate(Text2 = ifelse(prop > 10,Text1,NA)) %>%
  mutate(Dateset = "MEAN")

## Regions with shifted correlations
sum(Output.composite.Area$prop[c(2,3,4,5)]) / sum(Output.composite.Area$prop[1:6])

## Plot
p_Cor_Shift <- ggplot() +
  geom_tile(
    data = Output.cor1,
    aes(x = lon, y =lat,fill= Tmp_1_Re,color= Tmp_1_Re)
  ) +
  geom_polygon(data=con.boundary1,
               color = "gray30", fill = NA,linewidth=0.2,show.legend = FALSE,
               aes(x = long, y = lat, group = group)) +
  scale_fill_manual(
    breaks = c("N_P","N_NS","N_N",
               "NS_P","NS_NS","NS_N",
               "P_P","P_NS","P_N"),
    values = c(brewer.pal(9,"BuPu")[c(3,6,9)],
               brewer.pal(9,"PuBuGn")[c(3,6,9)],
               brewer.pal(9,"Oranges")[c(3,6,9)]),
    na.value = NA,
    guide = guide_legend(nrow = 3, byrow = TRUE)
  )+
  scale_color_manual(
    breaks = c("N_P","N_NS","N_N",
               "NS_P","NS_NS","NS_N",
               "P_P","P_NS","P_N"),
    values = c(brewer.pal(9,"BuPu")[c(3,6,9)],
               brewer.pal(9,"PuBuGn")[c(3,6,9)],
               brewer.pal(9,"Oranges")[c(3,6,9)]),
    na.value = NA,
    guide = guide_legend(nrow = 3, byrow = TRUE)
  )+
  scale_x_continuous(breaks = seq(0,360,90),label = c("0°","90°E","180°","90°W","0°"),
                     sec.axis = sec_axis(~ .))+
  scale_y_continuous(breaks = seq(-60,90,30),label = c("60°S","30°S","0°S","30°N","60°N","90°N"),
                     sec.axis = sec_axis(~ .))+
  coord_quickmap(expand = FALSE,xlim = c(0,360),ylim = c(-60,90))+
  theme_figure2+
  theme(
    legend.position ="none",
    legend.title = element_blank(),
    plot.margin = margin(t = 10, r = 10, b = 10, l = 12, unit = "pt"),
    axis.title.x.bottom = element_blank(),
    axis.title.y.left = element_blank(),
    axis.text.x.bottom = element_text(color = "black",size = 8),
    axis.text.y.left = element_text(color = "black",size = 8)
  )

## Legend
p_Cor_Shift1 <- ggplot() +
  geom_tile(
    data = Output.cor1,
    aes(x = lon, y =lat,fill= Tmp_1_Re)
  ) +
  scale_fill_manual(
    breaks = c("N_P","N_NS","N_N",
               "NS_P","NS_NS","NS_N",
               "P_P","P_NS","P_N"),
    values = c(brewer.pal(9,"BuPu")[c(3,6,9)],
               brewer.pal(9,"PuBuGn")[c(3,6,9)],
               brewer.pal(9,"Oranges")[c(3,6,9)]),
    na.value = NA,
    guide = guide_legend(nrow = 3, byrow = TRUE)
  )+
  theme(
    legend.position = "top",
    legend.justification = c(0.5,0.8),
    legend.title = element_blank()
  )
legend1 = cowplot::get_plot_component(p_Cor_Shift1, 'guide-box-top', return_all = TRUE)
legend_Shift_Cor = plot_grid(legend1)


# Plot Area -------------------------------------------------------

## Data
Output.Area = read.csv("Output Data/Plot_Data/Output.composite.Area_MEAN.csv",row.names = 1)

## Shifted Percentage
Output.Per = Output.Area %>%
  dplyr::group_by(Dateset) %>%
  dplyr::summarise(
    P_NS = sum(prop[which(Tmp_1_Re%in%c("P_NS"))]),
    P_P = sum(prop[which(Tmp_1_Re%in%c("P_P"))]),
    Negative.Area = sum(prop[which(Tmp_1_Re%in%c("N_N","P_N","NS_N"))]),
    Shift.Area = sum(prop[which(Tmp_1_Re%in%c("P_NS","P_N","N_P","N_NS"))]),
    Sig.Area = sum(prop[which(Tmp_1_Re%in%c("P_P","P_NS","P_N","N_P","N_NS","N_N"))])
  ) %>% 
  dplyr::mutate(Shift.per = Shift.Area/Sig.Area) %>% 
  dplyr::mutate(Shift.per =  sprintf(Shift.per*100, fmt = '%#.2f')) %>% 
  dplyr::mutate(Shift.per =  paste0(Shift.per,"%")) %>% 
  dplyr::mutate(across(!c(Dateset,Shift.per), ~sprintf(.x, fmt = '%#.2f')))

## Plot
p_per_Cor = ggplot() +
  geom_bar(stat="identity", width=1, color="white",
           data = Output.Area, 
           aes(x = "", y = prop,fill = Tmp_1_Re)) +
  geom_text(data = Output.Area, 
            aes(x="", y = ypos, label = Text2), color = "black", size=4)+
  scale_fill_manual(
    breaks = c("N_P","N_NS","N_N",
               "NS_P","NS_NS","NS_N",
               "P_P","P_NS","P_N"),
    values = c(brewer.pal(9,"BuPu")[c(3,6,9)],
               brewer.pal(9,"PuBuGn")[c(3,6,9)],
               brewer.pal(9,"Oranges")[c(3,6,9)]),
    na.value = NA,
    guide = guide_legend(nrow = 3, byrow = TRUE)
  )+
  coord_polar("y", start=0) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.margin = margin(t = 0, r = 2, b = 0, l = 0, unit = "pt")
  )

## Summary
p.bottom = plot_grid(
  p_Cor_Shift,
  legend_Shift_Cor,
  p_per_Cor,
  labels = c("c", "", "d"),
  hjust = -0.1,
  ncol = 3,
  rel_widths = c(5.7, 2.7, 3)
)
           
# Saving Figure 2 ---------------------------------------------------------

## Total
p.total = plot_grid(p_Tmp_Cor_1,p.bottom,
                    ncol = 1,rel_heights = c(2.7+0.6,2.7))

## Total
ggsave(filename = paste0("Figures/Final Figures/Fig_Temp_2_Shifted_Cor.tiff"),
       plot = p.total,
       width = 5.7*2, 
       height = 2.7+0.6+2.7)
ggsave(filename = paste0("Figures/Final Figures/Fig_Temp_2_Shifted_Cor.pdf"),
       plot = p.total,
       device = cairo_pdf,
       width = 5.7*2, 
       height = 2.7+0.6+2.7)

# End -------------------------------------------------------------
