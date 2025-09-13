

# Plot Correlations -----------------------------------------------

## Data
Cor_Summary = read.csv("Output Data/Observation_Summary/Rec_Cor_Summary_MEAN.csv",row.names = 1)
Cor_Summary = Cor_Summary  %>% 
  dplyr::filter(Method == c("lmCV"))%>% 
  dplyr::filter(!Model %in% c("Pacific"))

## P-value
Cor_Summary$Cor1 = Cor_Summary$Cor
Cor_Summary$Cor1[which(Cor_Summary$Cor_p > 0.05)] = Cor_Summary$Cor1[which(Cor_Summary$Cor_p > 0.05)] + 1

## Factor
Cor_Summary$Model = factor(Cor_Summary$Model, levels = c("Global","NH","SH","Tropicial"))

## Plot 
p_Rec_Cor = ggplot()+
  geom_col(data = Cor_Summary,
           position = position_dodge(0.45), width = 0.4,
           aes(x = Model, y = Cor,color = Erupton.year,fill = Erupton.year))+
  geom_point(data = Cor_Summary,shape = 8,size = 2,
             position = position_dodge(0.45),
             aes(x = Model, y = Cor1+0.05,color = Erupton.year,fill = Erupton.year))+
  scale_y_continuous(limits = c(0,1),expand = c(0,0),name = "Correlation")+
  scale_x_discrete(expand = c(0.15,0.15),name = " ",
                   breaks = c("Global","NH","SH","Tropicial"),
                   labels = c("Global","NH", "SH","Tropical"))+
  theme_figure1+
  theme(
    legend.position = "inside",
    legend.justification.inside = c(0.7,0.99),
    legend.direction = "horizontal",
    legend.key.size = unit(13,"pt"),
    axis.title.x.bottom = element_blank(),
    axis.text.x.bottom = element_text(angle = 0,hjust = 0.5,size = 8),
    plot.margin = ggplot2::margin(12, 15, 15, 5, "pt")
  )

# Plot Reconstructed Time Series ------------------------------------------

## Eruption years
Volcanics1 = read.csv("Output Data/Plot_Data/Volcanics1.csv",row.names = 1)

## Data
Total = read.csv("Output Data/Observation_Summary/SEA.data_MEAN.csv",row.names = 1)

## Rename
Total = Total[,c("year","Type","NINO34","Pre_NINO34_Global","Pre_NINO34_NH","Pre_NINO34_SH", "Pre_NINO34_Tropicial")]
names(Total) = c("year","Type","Observation", "Global","NH", "SH","Tropical")

## Following ENSO
for(i in 1:NROW(Volcanics1)){
  Sel = which(Total$year == c(Volcanics1$Start.Year[i] + 1))
  if(length(Sel) == 0) {
    Volcanics1[i,"Following.type"] = NA
    Volcanics1[i,"Following.NINO34"] = NA
  } else {
    Volcanics1[i,"Following.type"] = Total$Type[Sel]
    Volcanics1[i,"Following.NINO34"] = Total$Observation[Sel]
  }
}
Volcanics1 = Volcanics1 %>%
  dplyr::select(Volcano.Name,
                VEI,
                Start.Year,
                Start.Month,
                Following.type,
                Following.NINO34) %>%
  dplyr::mutate(Start.Month_name = month.abb[Start.Month])

## Combine with Obs NINO 34 from 1890 to 1900
All_A.NINO = read.csv("Output Data/Plot_Data/All_A.NINO.csv",row.names = 1)

## Total Observation
Total[c((NROW(Total)+1):(NROW(Total)+11)),"year"] <- c(1890:1900)
Total = Total%>%dplyr::arrange(year)
Total[which(Total$year%in%c(1890:1901)),"Observation"] <- All_A.NINO$NINO34[which(All_A.NINO$year%in%c(1890:1901))]

## Plot Data
Plot.data = pivot_longer(data = Total, cols = c("Observation", "Global","NH","SH","Tropical"))

## Plot
p_ENSO = ggplot()+
  geom_path(data = Plot.data,
            aes(x = year, y = value, color = name))+
  scale_color_manual(
    breaks = c("Observation", "Global","NH","SH","Tropical"),
    labels = c("Observation", "Global","NH","SH","Tropical"),
    values = brewer.pal(5,"Set1"),
    guide = guide_legend(nrow = 1)
  ) +
  scale_x_continuous(breaks = seq(1900,2022,10),name = "year")+
  scale_y_continuous(name = "Niño 3.4 (°C)",breaks = seq(-4,4,1))+
  coord_cartesian(ylim = c(-3,3),xlim = c(1900,2022), expand = F)+
  theme_figure1+
  theme(
    plot.margin = ggplot2::margin(10, 15, 15, 10, "pt"),
    legend.position = c(0.35,0.95),
    legend.direction = "vertical",
    legend.title = element_blank(),
    axis.title.x.bottom = element_blank(),
    axis.ticks.y.right = element_line(linewidth = 0.3, color = "gray10"),
    axis.ticks.length.y.right = unit(4, "pt")
  )

## Saving
ggsave(
  filename = paste0("Figures/Supplementary Figures/Reconstruct ENSO.tiff"),
  plot = p_ENSO,
  width = 9,
  height = 5
)


# Selected Eruptions == 6 --------------------------------------------------

## Ei years
Volcanics_Sel = subset(Volcanics1,VEI >=6)
colors1 = brewer.pal(n=7,"Set1")

## Plot
plot.p = paste0("p_Vol_",1:20)
i = 1

## Highlighted Arrows
high.arrows = data.frame(year = c(1992,1992,1912,1912,1903,1903),
                         value = c(3,2,3,1.8,-2.8,-0.8),
                         sel = c(1,1,2,2,3,3))

## Limits
SEA_Limits = c(3,3,3)

## Plot
for(i in 1:NROW(Volcanics_Sel)) {
  
  ## Years
  Year_Sel = Volcanics_Sel$Start.Year[i]
  Volcanics_Date_Sel = paste0(Volcanics_Sel$Start.Month_name[i],", ",Volcanics_Sel$Start.Year[i])
  
  ## Plot
  assign(plot.p[i],
         p_ENSO_1 <- ggplot()+
           geom_vline(xintercept = Year_Sel,color = "gray50", linetype = "dashed",linewidth = 0.3) +
           geom_hline(yintercept = 0,color = "gray50", linetype = "dashed",linewidth = 0.3) +
           geom_path(data = Plot.data,
                     aes(x = year, y = value, color = name))+
           geom_line(
             data = high.arrows,
             linewidth = 0.3,color = "blue",
             aes(x = year, y = value, group = sel),
             arrow = arrow(length=unit(5,"pt"), ends="last", type = "closed"))+
           annotate("text",x = -Inf,y = Inf, hjust = -0.3, vjust = 1.2,label = Volcanics_Date_Sel) +
           scale_color_manual(
             breaks = c("Observation", "Global","NH","SH","Tropical"),
             labels = c("Observation", "Global","NH","SH","Tropical"),
             values = brewer.pal(5,"Set1"),
             guide = guide_legend(nrow = 1)
           ) +
           scale_x_continuous(breaks = seq(Year_Sel-4,Year_Sel+4,1),
                              labels = c(Year_Sel-4,"",Year_Sel-2,"",Year_Sel,"",Year_Sel+2,"",Year_Sel+4),
                              name = "year")+
           scale_y_continuous(name = "Niño 3.4 (°C)",breaks = seq(-4,4,1))+
           coord_cartesian(ylim = c(-SEA_Limits[i],SEA_Limits[i]),xlim = c(Year_Sel-4,Year_Sel+4), expand = F)+
           theme_figure1+
           theme(
             plot.margin = ggplot2::margin(12, 15, 15, 12, "pt"),
             legend.position = ,
             legend.direction = "vertical",
             legend.title = element_blank(),
             axis.title.x.bottom = element_blank(),
             axis.ticks.y.right = element_line(linewidth = 0.3, color = "gray10"),
             axis.ticks.length.y.right = unit(4, "pt")
           )
  )
}

## Plot
p_Vol_1
p_Vol_2
p_Vol_3

## Legend
p.legend.1 = ggplot() +
  geom_path(data = Plot.data,
            aes(x = year, y = value, color = name))+
  scale_color_manual(
    breaks = c("Observation", "Global","NH","SH","Tropical"),
    labels = c("Observation", "Global","NH","SH","Tropical"),
    values = brewer.pal(5,"Set1"),
    guide = guide_legend(nrow = 1)
  ) +
  theme_figure1+
  theme(
    legend.position = "top",
    legend.key.size = unit(1, 'cm'),
    legend.text = element_text(size=10)
  )
legend1 = cowplot::get_plot_component(p.legend.1, 'guide-box-top', return_all = TRUE)
legend1 = plot_grid(legend1)

# Saving ----------------------------------------------------------

## Total
p.total_1 = plot_grid(p_Rec_Cor ,
                      p_Vol_1,
                      p_Vol_2,
                      p_Vol_3,
                      labels = "auto",
                      nrow = 2)
p.total = plot_grid(p.total_1,legend1,rel_heights = c(5.5,0.5),ncol = 1)

## Saving
ggsave(
  filename = paste0("Figures/Final Figures/Fig_Temp_5.tiff"),
  plot = p.total,
  width = 8,
  height = 5.5+0.5
)
ggsave(
  filename = paste0("Figures/Final Figures/Fig_Temp_5.pdf"),
  plot = p.total,
  width = 8,
  height = 5.5+0.5
)

# End ---------------------------------------------------------------------


