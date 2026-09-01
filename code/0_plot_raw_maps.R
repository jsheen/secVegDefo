# Libraries
set.seed(324)
library(shiny)
library(cowplot)
library(shinyWidgets)
library(geobr)
library(randomForest)
library(sf)
library(rnaturalearth)
library(glmmTMB)
library(ggplot2)
library(gridExtra)
library(ggplotify)
library(ggtext)
library(gridGraphics)
library(pomp)
library(sp)
library(spdep)
library(sf)
library(raster)
library(spacetime)
library(data.table)
library(INLA)
library(dplyr)
library(splines)
library(tidyverse)
library(patchwork)
library(ggspatial)

# Code to show the amount of primary and secondary deforestation by municipality
# as well as the average age of secondary deforestation (showing not a lot of difference)

# Load municipalities
all_muni <- st_read("~/Desktop/secDef/muni_mun_exp/muni_mun_exp.shp")

# Load primary deforestation data
prim_allocated <- fread('~/Desktop/secDef/InteriorEdge/data/Res_Defo/prim_allocated.csv')
prim_allocated <- prim_allocated %>% filter(year > 2002)
sum(prim_allocated$monthly_deforest_m2[which(prim_allocated$year %in% 2003:2022)]) * 1e-6
test <- prim_allocated %>%
  filter(mun_exp == 110001 & year == 2004 & (month == 2 | month == 3))
View(test)
prim_simple <- prim_allocated %>%
  group_by(mun_exp) %>%
  summarise(prim_defo = sum(monthly_deforest_m2) * 1e-6)
sum(prim_allocated$monthly_deforest_m2) * 1e-6

# Load secondary deforestation data
sec_allocated_by_age_tot <- fread('~/Desktop/secDef/InteriorEdge/data/Res_Defo/sec_allocated_by_age_tot.csv')
sec_allocated_by_age_tot <- sec_allocated_by_age_tot %>% filter(year > 2002)
which(is.na(sec_allocated_by_age_tot$sec_defo_tot_m2_adj))
test <- sec_allocated_by_age_tot %>%
  filter(mun_exp == 110001 & year == 2004 & (month == 2 | month == 3))
View(test)
sec_simple <- sec_allocated_by_age_tot %>%
  group_by(mun_exp) %>%
  summarise(sec_defo = sum(sec_defo_tot_m2_adj) * 1e-6)

# Create map_data object
prim_simple$mun_exp <- as.character(prim_simple$mun_exp)
sec_simple$mun_exp <- as.character(sec_simple$mun_exp)
map_data <- all_muni %>%
  left_join(sec_simple, by = "mun_exp") %>%
  left_join(prim_simple, by = 'mun_exp')

# Load state lines
br_states <- ne_states(country = "Brazil", returnclass = "sf")
amazon_states <- br_states %>%
  filter(name %in% c("Acre", "Amapá", "Amazonas", "Maranhão", 
                     "Mato Grosso", "Pará", "Rondônia", "Roraima", "Tocantins"))

prim_threshold <- quantile(map_data$prim_defo, probs = 0.25, na.rm = TRUE)
sec_threshold <- quantile(map_data$sec_defo, probs = 0.25, na.rm = TRUE)
map_data <- map_data %>%
  mutate(
    prim_defo_plot = ifelse(prim_defo < prim_threshold, NA, prim_defo),
    sec_defo_plot = ifelse(sec_defo < sec_threshold, NA, sec_defo)
  )

p <- ggplot(data = map_data) +
  geom_sf(aes(fill = prim_defo_plot), color = NA) +
  geom_sf(data = amazon_states, fill = NA, color = "black", linewidth = 0.6) +
  # North Compass
  annotation_north_arrow(
    location = "bl", which_north = "true", 
    height = unit(1, "cm"), width = unit(1, "cm"), 
    pad_x = unit(0.05, "in"), pad_y = unit(0.05, "in"),
    style = north_arrow_fancy_orienteering
  ) +
  scale_fill_viridis_c(
    option = "plasma", 
    name = "Prim. Defo.\n(mill. km²)",
    na.value = "white"
  ) +
  theme_minimal() +
  labs(
    title = "A) Primary deforestation",
    x = NULL,
    y = NULL
  ) +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )
#p

# Load secondary deforestation heatmap
s <- ggplot(data = map_data) +
  geom_sf(aes(fill = sec_defo_plot), color = NA) +
  geom_sf(data = amazon_states, fill = NA, color = "black", linewidth = 0.6) +
  scale_fill_viridis_c(
    option = "viridis", 
    name = "Sec. Defo.\n(mill. km²)",
    na.value = "white"
  ) +
  theme_minimal() +
  labs(
    title = "B) Secondary deforestation",
    x = NULL,
    y = NULL
  ) +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )
#s

# Deforestation age profile
mb_age_prof_data <- fread('~/secVegDefo/data/plot_data/mb_age_prof.csv')
mb_age_prof_plot <- ggplot(mb_age_prof_data, aes(x = secondary_age, y = area_ha * 0.01, fill = location_type)) +
  # Adding position = "dodge" places the bars next to each other
  geom_col(position = "dodge", alpha = 0.85) +
  scale_fill_manual(
    values = c("total_edge_ha" = "indianred1", "total_int_ha" = "steelblue"),
    labels = c("Edge", "Interior")
  ) +
  theme_minimal() +
  labs(
    title = "C) Secondary Deforestation Age Profile",
    x = "Age of Secondary Vegetation (Years)",
    y = "Total Area Deforested (km²)",
    fill = "Location"
  ) +
  theme(legend.position = c(0.92, 0.72),
        legend.background = element_rect(color = "black", fill = "white", linewidth = 0.5),)
mb_age_prof_plot <- mb_age_prof_plot +
  theme(
    plot.margin = ggplot2::margin(t = 0.2, r = 0, b = 0.2, l = 1.5, unit = "cm")
  )
p <- p + 
  theme(plot.title.position = "plot")

mb_age_prof_plot <- mb_age_prof_plot + 
  theme(plot.title.position = "plot")
(p + s) / mb_age_prof_plot

bottom_row <- plot_spacer() + mb_age_prof_plot + plot_layout(widths = c(0, 10))
(p + s) / bottom_row

combined_maps <- (p + s) / (mb_age_prof_plot)

ggsave(
  filename = "~/secVegDefo/code_output/plots_descrip/Fig1.png",
  plot = combined_maps, 
  width = 9,    
  height = 6,     
  dpi = 900       
)
