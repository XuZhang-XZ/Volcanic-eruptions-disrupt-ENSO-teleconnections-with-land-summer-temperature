

# Plot Mid_Low Temperature ------------------------------------------------------------

## Colors
colors1 = brewer.pal(n=7,"Set1")
ylab_GLSAT = bquote("Global "*italic({T}[a])*" anomaly")
ylab_GLSAT_Unit = bquote("Global "*italic({T}[a])*" anomaly (°C)")

## Data
Volcanics1 = read.csv("Output Data/Plot_Data/Volcanics1.csv",row.names = 1)
Total = read.csv("Output Data/Observation_Summary/Low_Mid_Tmp.csv",row.names = 1)

## Each Number of events
Total%>%group_by(Type,Eruption.year)%>%summarise(count = length(NINO34))
Total%>%group_by(Type)%>%summarise(count = length(NINO34))
Total%>%group_by(Eruption.year)%>%summarise(count = length(NINO34))

## Plot Data
Total.NVol = Total %>% dplyr::filter(Eruption.year %in% c("NVol"))
Total.Vol = Total %>% dplyr::filter(Eruption.year %in% c("Vol"))

## Correlation
temp1 = cor.test(Total.NVol$NINO34,Total.NVol$GLST_MEAN)
temp2 = cor.test(Total.Vol$NINO34,Total.Vol$GLST_MEAN)
Cor_p1 = sprintf(temp1$estimate, fmt = '%#.2f') 
Cor_p2 = sprintf(temp2$estimate, fmt = '%#.2f') 

## Plot Data
Total.plot = Total %>%
  dplyr::mutate(across(c(GLST_CRU, GLST_GISS, GLST_BEST, GLST_MEAN),  ~
                         (.x) * 6)) %>%
  pivot_longer(cols = c("NINO34", "GLST_MEAN"))

## Plot Time series
p_ENSO = ggplot()+
  geom_hline(yintercept = 0, color = "gray50",linetype = "dashed", linewidth = 0.4) +
  geom_path(data = Total.plot,
            aes(x = year, y = value, color = name))+
  geom_point(data = Total.plot %>% dplyr::filter(Eruption.year %in% c("Vol")),show.legend = FALSE,
            aes(x = year, y = value, color = name,shape = Eruption.year))+
  geom_point(data = Volcanics1,shape = 8,color = colors1[5],
             aes(x = Start.Year, y = -2.8)) +
  scale_color_manual(breaks = c("NINO34", "GLST_MEAN"),
                     values = colors1[c(1,2)],
                     labels = c("Niño 3.4",ylab_GLSAT))+
  scale_x_continuous(breaks = seq(1900, 2022, 10), name = "year") +
  scale_y_continuous(
    name = "Niño 3.4 (°C)",
    breaks = seq(-4, 4, 1),
    sec.axis = sec_axis(
      ~ . / 6,
      breaks = c(-0.4, -0.2, 0, 0.2, 0.4),
      name = ylab_GLSAT_Unit
    )
  ) +
  coord_cartesian(ylim = c(-3,3),xlim = c(1900,2023), expand = F)+
  theme_figure1+
  theme(
    plot.margin = ggplot2::margin(7, 7, 10, 15, "pt"),
    legend.position = "inside",
    legend.position.inside = c(0.2,0.9),
    legend.direction = "vertical",
    legend.title = element_blank(),
    axis.title.x.bottom = element_blank(),
    axis.title.y.left = element_text(color = colors1[1]),
    axis.text.y.left = element_text(color = colors1[1]),
    axis.title.y.right = element_text(color = colors1[2]),
    axis.text.y.right = element_text(color = colors1[2]),
    axis.ticks.y.right = element_line(linewidth = 0.3, color = "gray10"),
    axis.ticks.length.y.right = unit(4, "pt")
  )

## Top
p_Tmp_T = p_ENSO


# Bottom Plot -----------------------------------------------------

## NVol years
p_Tmp_NVol = ggplot()+
  geom_point(data = Total.NVol,size = 1,aes(x = NINO34, y = GLST_MEAN))+
  annotate("text",x = -Inf, y = -Inf,vjust = -0.2, hjust = -0.05, color = "red",size = 4,
           label =  "paste(italic(r), \" = 0.62 (\",italic(p),  \" < 0.01; n = 88) \" )", parse = TRUE)+
  scale_x_continuous(limits = c(-3,3),name = "Niño 3.4 (°C)")+
  scale_y_continuous(name = ylab_GLSAT_Unit, breaks = c(-0.6,-0.4,-0.2,0,0.2,0.4,0.6))+
  coord_cartesian(ylim = c(-0.6,0.6),xlim = c(-3,3), expand = F)+
  theme_figure1+
  theme(
    plot.margin = ggplot2::margin(7, 8, 10, 7, "pt")
  )

## Vol years
p_Tmp_Vol = ggplot()+
  geom_point(data = Total.Vol,size = 1,aes(x = NINO34, y = GLST_MEAN))+
  geom_point(data = Total.Vol[which(Total.Vol$year == 1992),], color = colors1[3],size = 2,aes(x = NINO34, y = GLST_MEAN))+
  geom_text(data = Total.Vol[which(Total.Vol$year == 1992),], color = colors1[3],size = 3,hjust = 0.1,vjust = -1.2,
            aes(x = NINO34, y = GLST_MEAN,label = "1992"))+
  annotate("text",x = -Inf, y = -Inf,vjust = -0.2, hjust = -0.05, color = "red",size = 4,
           label =  "paste(italic(r), \" = 0.07 (\",italic(p),  \" = 0.68; n = 34) \" )", parse = TRUE)+
  scale_x_continuous(limits = c(-3,3),name = "Niño 3.4 (°C)")+
  scale_y_continuous(name = ylab_GLSAT_Unit, breaks = c(-0.6,-0.4,-0.2,0,0.2,0.4,0.6))+
  coord_cartesian(ylim = c(-0.6,0.6),xlim = c(-3,3), expand = F)+
  theme_figure1+
  theme(
    plot.margin = ggplot2::margin(7, 8, 10, 7, "pt")
  )

## Correlation Density
Total.cor.boot = read.csv("Output Data/Plot_Data/Total.cor.boot.csv",row.names = 1)

## MEAN
temp = Total.cor.boot %>% dplyr::filter(Type == "NVol" & Dataset == "MEAN")
temp1 = quantile(temp$Cor,probs = c(0.01,0.05))
Cor_0.01 = temp1[1]
Cor_0.05 = temp1[2]
Cor_Vol = 0.07

## Plot
p_Cor = ggplot()+
  geom_histogram(data = temp,
                 color = NA,binwidth = 0.04,fill = "gray70",
                 aes(x = Cor, y = after_stat(density))) +
  geom_segment(aes(x = Cor_0.01 , y = 0, xend = Cor_0.01, yend = 1), color = "blue", linewidth = 0.5) +
  annotate("text",x = Cor_0.01, y = 1,vjust = -0.2, hjust = 0.2, color = "blue",size = 3,label = "1%")+
  geom_segment(aes(x = Cor_0.05 , y = 0, xend = Cor_0.05, yend = 1), color = "blue", linewidth = 0.5) +
  annotate("text",x = Cor_0.05, y = 1,vjust = -0.2, hjust = -0.2, color = "blue",size = 3,label = "5%")+
  geom_segment(aes(x = Cor_Vol , y = 0, xend = Cor_Vol, yend = 3), color = "red", linewidth = 1) +
  annotate("text",x = Cor_Vol, y = 3,vjust = 0.5, hjust = -0.2, color = "red",size = 4,label = "Vol years")+
  guides(color = F) +
  scale_color_manual(breaks = c("NVol","Vol"), values = brewer.pal(9,"Set1")[c(7,4)])+
  scale_x_continuous(limits = c(-0.5,1),breaks = seq(-0.2,1,0.2),name = "Correlation")+
  scale_y_continuous(name = "Probability density")+
  coord_cartesian(xlim = c(-0.2,1),ylim = c(0,4), expand = F)+
  theme_figure1+
  theme(
    plot.margin = ggplot2::margin(7, 15, 10, 7, "pt"),
    legend.position = c(0.8,0.87)
  )

## Void
p_void = ggplot() +
  theme_void()

## Bottom
p_Tmp_B = plot_grid(p_Tmp_NVol,p_Tmp_Vol,p_Cor, p_void,
                    align = "h",
                    rel_widths = c(3,3,3,0.2),
                    labels = c(letters[2:4]," "),
                    nrow  = 1)

# Saving ----------------------------------------------------------

## Total
p.total = plot_grid(p_Tmp_T, p_Tmp_B, labels = c("a"," "), ncol = 1)

## Saving
ggsave(filename = paste0("Figures/Final Figures/Fig_Temp_1.tiff"),
       plot = p.total,
       width = 9+0.2, 
       height = 6.1)
ggsave(filename = paste0("Figures/Final Figures/Fig_Temp_1.pdf"),
       plot = p.total,
       device = cairo_pdf,
       width = 9+0.2, 
       height = 6.1)

# End -------------------------------------------------------------
