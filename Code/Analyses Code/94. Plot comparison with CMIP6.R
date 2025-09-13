
# Plot MME --------------------------------------------------------------------

## Observation
Glo.Tmp.sum1 = read.csv("Output Data/Plot_Data/Total.cor.boot_CMIP6.csv",row.names = 1)
Glo.Tmp.sum1 = Glo.Tmp.sum1 %>%
  dplyr::filter(Dataset == "MEAN") %>%
  dplyr::mutate(Dataset = "Observation") %>%
  dplyr::rename(Eruption.year = Type,Model = Dataset,Cor = Cor) %>%
  dplyr::mutate(Dataset = "Observation")

## CMIP6 historical
Glo.Tmp.sum2 = read.csv("Output Data/CMIP6_histroical_Correlation/Cor_Global_Tmp.csv",row.names = 1)
Glo.Tmp.sum2 = Glo.Tmp.sum2 %>% 
  dplyr::filter(Type == "SAOD_NINO34") %>%
  dplyr::group_by(Model,Eruption.year) %>% 
  dplyr::summarise(Cor = quantile(Cor, probs = 0.5,na.rm = TRUE)) %>%
  dplyr::mutate(Dataset = "Historical MME")

## Uncertainty
Glo.Tmp.sum_bar = dplyr::add_row(Glo.Tmp.sum1,Glo.Tmp.sum2)%>%
  dplyr::group_by(Dataset,Eruption.year)%>%
  dplyr::summarise(
    Cor_975 = quantile(Cor, probs = 0.975),
    Cor_025 = quantile(Cor, probs = 0.025),
    Cor_5 = quantile(Cor, probs = 0.5),
    Cor_sd = sd(Cor)
  ) %>%
  dplyr::ungroup()

## Individual CMIP6 models
Glo.Tmp.sum2 = read.csv("Output Data/CMIP6_histroical_Correlation/Cor_Global_Tmp.csv",row.names = 1)
Glo.Tmp.sum2 = Glo.Tmp.sum2 %>% dplyr::filter(Type == "SAOD_NINO34")
Glo.Tmp.sum = Glo.Tmp.sum2

## Uncertainty
Glo.Tmp.sum_bar_I = Glo.Tmp.sum %>%
  dplyr::group_by(Model, Eruption.year) %>%
  dplyr::summarise(
    Cor_975 = quantile(Cor, probs = 0.975),
    Cor_025 = quantile(Cor, probs = 0.025),
    Cor_5 = quantile(Cor, probs = 0.5),
    Cor_sd = sd(Cor)
  )

## Models
All.models = unique(Glo.Tmp.sum_bar_I$Model)
  
## Combine
Glo.Tmp.sum_bar = Glo.Tmp.sum_bar %>%
  dplyr::add_row(Dataset = Glo.Tmp.sum_bar_I$Model,
                 Eruption.year = Glo.Tmp.sum_bar_I$Eruption.year,
                 Cor_5 = Glo.Tmp.sum_bar_I$Cor_5) %>%
  dplyr::mutate(Dataset = factor(Dataset,levels = c("Observation","Historical MME",All.models)))

## Plot
p_MME_Cor = ggplot()+
  geom_hline(yintercept = -2, color = "gray50",linetype = "solid", linewidth = 0.4) +
  geom_col(data = Glo.Tmp.sum_bar,
           position = position_dodge(0.5), width = 0.4,
           aes(x = Dataset, y = Cor_5,color = Eruption.year,fill = Eruption.year))+
  geom_errorbar(data = Glo.Tmp.sum_bar,
                position = position_dodge(0.5), width = 0.2,
                color = "gray30",
                aes(x = Dataset, ymin = Cor_5-Cor_sd,ymax = Cor_5+Cor_sd,group = Eruption.year))+
  scale_y_continuous(expand = c(0,0),breaks = seq(-1,1,0.2),name = "Correlation")+
  scale_x_discrete(expand = c(0.02,0.02),name = "Correlation")+
  coord_cartesian(ylim = c(0,1))+
  theme_figure1+
  theme(
    legend.position = "inside",
    legend.justification = c(0.1,1),
    legend.direction = "vertical",
    plot.margin = margin(t = 10, r = 5, b = 5, l = 5, unit = "pt"),
    axis.title.x.bottom = element_blank(),
    axis.text.x.bottom = element_text(color = "black",size = 9,angle = 45,hjust = 1),
    axis.text.y.left = element_text(color = "black",size = 9)
  )


# Plot CMIP6 Correlations -----------------------------------------------

## Correlations in CMIP6
Out.cor = read.csv("Output Data/CMIP6_histroical_Correlation/Out.cor_CRU.csv",row.names = 1)

## Sig regions
sig.lon = seq(1,359,by = 4)
sig.lat = seq(-89,89,by = 4)
Out.cor.sig = Out.cor[which(Out.cor$lon%in%sig.lon&Out.cor$lat%in%sig.lat),]

## Parameters
p.names=paste0("p_ENSO_Cor_",c(1:16))
cols.plot = c("Developing_NVol_Cor","Developing_Vol_Cor","Decaying_NVol_Cor","Decaying_Vol_Cor")
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
             aes(x = lon, y =lat,fill=.data[[cols.plot[i]]],color=.data[[cols.plot[i]]])
           ) +
           geom_point(
             data = Out.cor.sig%>%dplyr::filter(.data[[cols.plot.p[i]]]<0.05),color="black",shape = 16,size = 0.5,alpha = 0.5,
             aes(x = lon, y =lat)
           ) +
           geom_polygon(data=con.boundary1,
                        color = "gray30", fill = NA,linewidth=0.2,
                        aes(x = long, y = lat, group = group)) +
           scale_fill_gradientn(
             colours = brewer.pal(n=11,plot.palette[i])[c(11:9,6,3:1)],
             limits = c(start.limit[i], end.limit[i]),
             oob = scales::squish,na.value = NA
           )+
           scale_color_gradientn(
             colours = brewer.pal(n=11,plot.palette[i])[c(11:9,6,3:1)],
             limits = c(start.limit[i], end.limit[i]),
             oob = scales::squish,na.value = NA
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
             plot.margin = margin(t = 7, r = 7, b = 7, l = 7, unit = "pt"),
             axis.title.x.bottom = element_blank(),
             axis.title.y.left = element_blank(),
             axis.text.x.bottom = element_text(color = "black",size = 8),
             axis.text.y.left = element_text(color = "black",size = 8)
           )
  )
}

# Test
if(1 == 0) {
  plot_grid(p_ENSO_Cor_1,p_ENSO_Cor_2,ncol = 1)
  plot_grid(p_ENSO_Cor_3,p_ENSO_Cor_4,ncol = 1)
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
    labels = c(-0.6,-0.5,-0.4,-0.3,-0.2,-0.1,0,0.1,0.2,0.3,0.4,0.5,0.6),
    oob = scales::squish,na.value = NA, 
    name = "Correlation with Niño 3.4",
    guide = guide_colorbar(title.position = "top",title.hjust = 0.5)
  )+
  theme_figure2+
  theme(
    legend.position = "top",
    legend.justification = c(0.5,0.7),
    legend.key.width = unit(90,"pt"),
    legend.key.height = unit(6,"pt"),
    legend.direction = "horizontal",
    legend.title = element_text(),
    plot.margin = margin(t = 5, r = 5, b = 5, l = 5, unit = "pt"),
  )
legend1 = cowplot::get_plot_component(p1, 'guide-box-top', return_all = TRUE)
legend_ENSO_Cor  = plot_grid(legend1)

## Summary
p.middle = plot_grid(
  p_ENSO_Cor_3,
  p_ENSO_Cor_4,
  align = "h",
  labels = c("b", "c"),
  label_y = 1.05,
  nrow = 1
)

# Saving Figure ----------------------------------------------------------

## Saving
p.top = plot_grid(
  p_MME_Cor,
  nrow = 1,
  labels = c("a")
)

p.total = plot_grid(
  p.top,
  p.middle,
  legend_ENSO_Cor,
  rel_heights = c(4.5, 3, 0.7),
  ncol = 1
)

## Saving
ggsave(filename = "Figures/Final Figures/Fig_Temp_4.tiff",
       plot = p.total,
       width = 5.7*2, 
       height = 4.5+3+0.7)
ggsave(filename = "Figures/Final Figures/Fig_Temp_4.pdf",
       plot = p.total,
       device = cairo_pdf,
       width = 5.7*2, 
       height = 4.5+3+0.7)

# End -------------------------------------------------------------

