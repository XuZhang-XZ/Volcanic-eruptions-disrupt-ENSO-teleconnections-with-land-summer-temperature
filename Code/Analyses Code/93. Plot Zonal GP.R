

# Plot Air Pressure Sensitivity -------------------------------------------

## Label
y_label = c(1000,700,500,200,100,50,20,10)
y_breaks = -log10(y_label)

## Data
Out.SGP = read.csv("Output Data/NCEP/Out.SCor.csv",row.names = 1) %>%
  dplyr::mutate("y_axis" = -log10(Level))

## Functions
Inte_SGP = function(a) {
  Temp =  interp::interp(
    x = Out.SGP$Lat,
    y = Out.SGP$y_axis,
    z = a,
    xo = seq(-89.9, 89.9, length.out = 180),
    yo = seq(-2.9999, -1.01, length.out = 180),
    method = "linear"
  )
  re = as.numeric(Temp$z)
  re
}

## Output
Out.SGP_I = expand.grid(
  "X_axis" = seq(-89.9, 89.9, length.out = 180),
  "Y_axis" = seq(-2.9999, -1.01, length.out = 180)
) %>%
  dplyr::mutate(
    "SCor_NVol" = Inte_SGP(Out.SGP$SCor_NVol),
    "SCor_NVol_p" = Inte_SGP(Out.SGP$SCor_NVol_p),
    "SCor_Vol" = Inte_SGP(Out.SGP$SCor_Vol),
    "SCor_Vol_p" = Inte_SGP(Out.SGP$SCor_Vol_p)
  )

## Significance
sig.X = sort(unique(Out.SGP_I$X_axis))[2*(1:100)]
sig.Y = sort(unique(Out.SGP_I$Y_axis))[3*(1:100)]
Out.SGP_I_S = Out.SGP_I %>% dplyr::filter(X_axis %in% sig.X & Y_axis %in% sig.Y)

## Plot
pS_1 = ggplot() +
  geom_tile(data = Out.SGP_I, 
            aes(x = X_axis, y = Y_axis, fill = SCor_NVol, color = SCor_NVol)) +
  geom_point(
    data = Out.SGP_I_S %>% dplyr::filter(SCor_NVol_p < 0.1), 
    color = "gray10",
    shape = 16,
    size = 0.2,
    alpha = 0.5,
    aes(x = X_axis, y = Y_axis)
  ) +
  scale_color_gradientn(
    colours = brewer.pal(n = 11, "RdBu")[11:1],
    limits = c(-0.8, 0.8),
    oob = scales::squish,
    na.value = NA,
    name = "Correlation with Niño 3.4",
    guide = guide_colorbar(title.position = "top", title.hjust = 0.5)
  )+
  scale_fill_gradientn(
    colours = brewer.pal(n = 11, "RdBu")[11:1],
    limits = c(-0.8, 0.8),
    oob = scales::squish,
    na.value = NA,
    name = "Correlation with Niño 3.4",
    guide = guide_colorbar(title.position = "top", title.hjust = 0.5)
  )+
  scale_x_continuous(
    transform = "reverse",
    name = "Latitude",
    breaks = seq(-90, 90, 30),
    label = c("90°S", "60°S", "30°S", "0", "30°N", "60°N", "90°N"),
    sec.axis = sec_axis( ~ .)
  ) +
  scale_y_continuous(
    name = "Pressure level (hPa)",
    breaks =  y_breaks,
    labels = y_label,
    sec.axis = sec_axis( ~ .)) +
  coord_cartesian(xlim = c(90,-90),ylim = c(-3,-1),expand = FALSE) +
  theme_figure2+
  theme(
    legend.position = "none",
    legend.key.width = unit(5,"pt"),
    legend.key.height = unit(35,"pt"),
    legend.direction = "vertical",
    plot.margin = margin(t = 10, r = 12, b = 8, l = 8, unit = "pt"),
    axis.text.x.bottom = element_text(color = "black",size = 8),
    axis.text.y.left = element_text(color = "black",size = 8)
  )

## Plot
pS_2 = ggplot() +
  geom_tile(data = Out.SGP_I, 
            aes(x = X_axis, y = Y_axis, fill = SCor_Vol, color = SCor_Vol)) +
  geom_point(
    data = Out.SGP_I_S %>% dplyr::filter(SCor_Vol_p < 0.1), 
    color = "gray10",
    shape = 16,
    size = 0.2,
    alpha = 0.5,
    aes(x = X_axis, y = Y_axis)
  ) +
  scale_color_gradientn(
    colours = brewer.pal(n = 11, "RdBu")[11:1],
    limits = c(-0.8, 0.8),
    oob = scales::squish,
    na.value = NA,
    name = "Correlation with Niño 3.4",
    guide = guide_colorbar(title.position = "top", title.hjust = 0.5)
  )+
  scale_fill_gradientn(
    colours = brewer.pal(n = 11, "RdBu")[11:1],
    limits = c(-1, 1),
    breaks = seq(-0.6, 0.6, 0.1),
    labels = round(seq(-0.6, 0.6, 0.1), 1),
    oob = scales::squish,
    na.value = NA,
    name = "Correlation with Niño 3.4",
    guide = guide_colorbar(title.position = "top", title.hjust = 0.5)
  )+
  scale_x_continuous(
    transform = "reverse",
    name = "Latitude",
    breaks = seq(-90, 90, 30),
    label = c("90°S", "60°S", "30°S", "0", "30°N", "60°N", "90°N"),
    sec.axis = sec_axis( ~ .)
  ) +
  scale_y_continuous(
    name = "Pressure level (hPa)",
    breaks =  y_breaks,
    labels = y_label,
    sec.axis = sec_axis( ~ .)) +
  coord_cartesian(xlim = c(90,-90),ylim = c(-3,-1),expand = FALSE) +
  theme_figure2+
  theme(
    legend.position = "none",
    legend.key.width = unit(5,"pt"),
    legend.key.height = unit(35,"pt"),
    legend.direction = "vertical",
    plot.margin = margin(t = 10, r = 12, b = 8, l = 8, unit = "pt"),
    axis.text.x.bottom = element_text(color = "black",size = 8),
    axis.text.y.left = element_text(color = "black",size = 8)
  )


## Legend
i = 1
p1 <- ggplot() +
  geom_tile(data = Out.SGP_I, aes(x = X_axis, y = Y_axis, fill = SCor_Vol)) +
  scale_fill_gradientn(
    colours = brewer.pal(n = 11, "RdBu")[11:1],
    limits = c(-0.8, 0.8),
    breaks = seq(-0.8, 0.8, 0.2),
    oob = scales::squish,
    na.value = NA,
    name = "Geopotential height sensitivity (standard deviation per °C)",
    guide = guide_colorbar(title.position = "top", title.hjust = 0.5)
  ) +
  theme_figure2 +
  theme(
    legend.position = "top",
    legend.key.width = unit(90, "pt"),
    legend.key.height = unit(6, "pt"),
    legend.direction = "horizontal",
    legend.title = element_text(),
    plot.margin = margin(
      t = 5,
      r = 5,
      b = 5,
      l = 5,
      unit = "pt"
    ),
  )
legend1 = cowplot::get_plot_component(p1, 'guide-box-top', return_all = TRUE)
legend_Cor_S = plot_grid(legend1)



# Plot Air Pressure Composite -------------------------------------------

## Label
y_label = c(1000,700,500,200,100,50,20,10)
y_breaks = -log10(y_label)

## Data
Out.SGP = read.csv("Output Data/NCEP/Out.GP.csv",row.names = 1) %>%
  dplyr::mutate("y_axis" = -log10(Level))

## Functions
Inte_SGP = function(a) {
  Temp =  interp::interp(
    x = Out.SGP$Lat,
    y = Out.SGP$y_axis,
    z = a,
    xo = seq(-89.9, 89.9, length.out = 180),
    yo = seq(-2.9999, -1.01, length.out = 180),
    method = "linear"
  )
  re = as.numeric(Temp$z)
  re
}

## Output
Out.SGP_I = expand.grid(
  "X_axis" = seq(-89.9, 89.9, length.out = 180),
  "Y_axis" = seq(-2.9999, -1.01, length.out = 180)
) %>%
  dplyr::mutate(
    "GP_NVol" = Inte_SGP(Out.SGP$GP_NVol),
    "GP_NVol_p" = Inte_SGP(Out.SGP$GP_NVol_p),
    "GP_Vol" = Inte_SGP(Out.SGP$GP_Vol),
    "GP_Vol_p" = Inte_SGP(Out.SGP$GP_Vol_p)
  )

## Significance
sig.X = sort(unique(Out.SGP_I$X_axis))[2*(1:100)]
sig.Y = sort(unique(Out.SGP_I$Y_axis))[3*(1:100)]
Out.SGP_I_S = Out.SGP_I %>% dplyr::filter(X_axis %in% sig.X & Y_axis %in% sig.Y)

## Plot
pC_1 = ggplot() +
  geom_tile(data = Out.SGP_I, 
            aes(x = X_axis, y = Y_axis, fill = GP_NVol, color = GP_NVol)) +
  geom_point(
    data = Out.SGP_I_S %>% dplyr::filter(GP_NVol_p < 0.1), 
    color = "gray10",
    shape = 16,
    size = 0.2,
    alpha = 0.5,
    aes(x = X_axis, y = Y_axis)
  ) +
  scale_color_gradientn(
    colours = brewer.pal(n = 11, "RdBu")[11:1],
    limits = c(-0.6, 0.6),
    oob = scales::squish,
    na.value = NA,
    name = "Correlation with Niño 3.4",
    guide = guide_colorbar(title.position = "top", title.hjust = 0.5)
  )+
  scale_fill_gradientn(
    colours = brewer.pal(n = 11, "RdBu")[11:1],
    limits = c(-0.6, 0.6),
    oob = scales::squish,
    na.value = NA,
    name = "Correlation with Niño 3.4",
    guide = guide_colorbar(title.position = "top", title.hjust = 0.5)
  )+
  scale_x_continuous(
    transform = "reverse",
    name = "Latitude",
    breaks = seq(-90, 90, 30),
    label = c("90°S", "60°S", "30°S", "0", "30°N", "60°N", "90°N"),
    sec.axis = sec_axis( ~ .)
  ) +
  scale_y_continuous(
    name = "Pressure level (hPa)",
    breaks =  y_breaks,
    labels = y_label,
    sec.axis = sec_axis( ~ .)) +
  coord_cartesian(xlim = c(90,-90),ylim = c(-3,-1),expand = FALSE) +
  theme_figure2+
  theme(
    legend.position = "none",
    legend.key.width = unit(5,"pt"),
    legend.key.height = unit(35,"pt"),
    legend.direction = "vertical",
    plot.margin = margin(t = 10, r = 12, b = 8, l = 8, unit = "pt"),
    axis.text.x.bottom = element_text(color = "black",size = 8),
    axis.text.y.left = element_text(color = "black",size = 8)
  )



## Plot
pC_2 = ggplot() +
  geom_tile(data = Out.SGP_I, 
            aes(x = X_axis, y = Y_axis, fill = GP_Vol, color = GP_Vol)) +
  geom_point(
    data = Out.SGP_I_S %>% dplyr::filter(GP_Vol_p < 0.1), 
    color = "gray10",
    shape = 16,
    size = 0.2,
    alpha = 0.5,
    aes(x = X_axis, y = Y_axis)
  ) +
  scale_color_gradientn(
    colours = brewer.pal(n = 11, "RdBu")[11:1],
    limits = c(-0.6, 0.6),
    oob = scales::squish,
    na.value = NA,
    name = "Correlation with Niño 3.4",
    guide = guide_colorbar(title.position = "top", title.hjust = 0.5)
  )+
  scale_fill_gradientn(
    colours = brewer.pal(n = 11, "RdBu")[11:1],
    limits = c(-0.6, 0.6),
    oob = scales::squish,
    na.value = NA,
    name = "Correlation with Niño 3.4",
    guide = guide_colorbar(title.position = "top", title.hjust = 0.5)
  )+
  scale_x_continuous(
    transform = "reverse",
    name = "Latitude",
    breaks = seq(-90, 90, 30),
    label = c("90°S", "60°S", "30°S", "0", "30°N", "60°N", "90°N"),
    sec.axis = sec_axis( ~ .)
  ) +
  scale_y_continuous(
    name = "Pressure level (hPa)",
    breaks =  y_breaks,
    labels = y_label,
    sec.axis = sec_axis( ~ .)) +
  coord_cartesian(xlim = c(90,-90),ylim = c(-3,-1),expand = FALSE) +
  theme_figure2+
  theme(
    legend.position = "none",
    legend.key.width = unit(5,"pt"),
    legend.key.height = unit(35,"pt"),
    legend.direction = "vertical",
    plot.margin = margin(t = 10, r = 12, b = 8, l = 8, unit = "pt"),
    axis.text.x.bottom = element_text(color = "black",size = 8),
    axis.text.y.left = element_text(color = "black",size = 8)
  )


## Legend
i = 1
p1 <- ggplot() +
  geom_tile(data = Out.SGP_I, 
            aes(x = X_axis, y = Y_axis, fill = GP_Vol)) +
  scale_fill_gradientn(
    colours = brewer.pal(n = 11, "RdBu")[11:1],
    limits = c(-0.6, 0.6),
    breaks = seq(-0.6, 0.6, 0.1),
    labels = round(seq(-0.6, 0.6, 0.1),1),
    oob = scales::squish,
    na.value = NA,
    name = "Composite geopotential height anomaly (standard deviation)",
    guide = guide_colorbar(title.position = "top", title.hjust = 0.5)
  ) +
  theme_figure2 +
  theme(
    legend.position = "top",
    legend.key.width = unit(90, "pt"),
    legend.key.height = unit(6, "pt"),
    legend.direction = "horizontal",
    legend.title = element_text(),
    plot.margin = margin(
      t = 5,
      r = 5,
      b = 5,
      l = 5,
      unit = "pt"
    ),
  )
legend1 = cowplot::get_plot_component(p1, 'guide-box-top', return_all = TRUE)
legend_Cor_C = plot_grid(legend1)


# Saving ------------------------------------------------------------------

## Top
p_Top = plot_grid(pS_1, pS_2,
                  nrow = 1,
                  labels = c("a", "b"),
                  hjust = -1.0)

## Bottom
p_Bot = plot_grid(pC_1, pC_2,
                  nrow = 1,
                  labels = c("c", "d"),
                  hjust = -1.0)

## Total
p_total = plot_grid(
  p_Top,
  legend_Cor_S,
  p_Bot,
  legend_Cor_C,
  ncol = 1,
  rel_heights = c(3, 0.6, 3, 0.6)
)

## Saving
ggsave(
  filename = paste0("Figures/Final Figures/Final_3.tiff"),
  plot = p_total,
  width = 5 + 5,
  height = 3 + 0.6 + 3 + 0.6
)
ggsave(
  filename = paste0("Figures/Final Figures/Final_3.pdf"),
  plot = p_total,
  device = cairo_pdf,
  width = 5 + 5,
  height = 3 + 0.6 + 3 + 0.6
)

# End ---------------------------------------------------------------------





