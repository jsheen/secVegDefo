# Load libraries
library(dplyr)
library(reshape2)
library(lubridate)
library(zoo)
library(geobr)
library(ggplot2)
library(pscl)
library(sf)
library(data.table)
library(tidyr)
library(caret)
library(MASS)
library(randomForest)
library(dplyr)
library(gridExtra)
library(sdmTMB)
library(pak)
library(glmmTMB)
library(scales)
library(viridis)
library(patchwork)
set.seed(0)

# Seasonality profiles of AID across municipalities
aid_sec_edge <- fread('~/secVegDefo/data/plot_data/aid_sec_edge.csv')
aid_sec_interior <- fread('~/secVegDefo/data/plot_data/aid_sec_interior.csv')
global_edge_intensity <- aid_sec_edge %>%
  group_by(month) %>%
  summarise(
    mean_intensity = mean(normalized_intensity, na.rm = TRUE),
    sd_intensity = sd(normalized_intensity, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(month_label = factor(month, levels = 1:12, labels = month.abb))
seas_edge_alert <- ggplot(global_edge_intensity, aes(x = month_label, y = mean_intensity, group = 1)) +
  # Add a ribbon for the variation across municipalities
  geom_ribbon(aes(ymin = mean_intensity - sd_intensity, 
                  ymax = mean_intensity + sd_intensity), 
              fill = "red", alpha = 0.2) +
  geom_line(color = "red", linewidth = 1.2) +
  geom_point(color = "red", size = 3) +
  theme_minimal() +
  labs(
    title = "Seasonality Profile: Edge",
    x = "Month",
    y = "Normalized intensity across municipalities"
  ) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )
# Get seasonality profile of alert intensity distribution (interior)
global_interior_intensity <- aid_sec_interior %>%
  group_by(month) %>%
  summarise(
    mean_intensity = mean(normalized_intensity, na.rm = TRUE),
    sd_intensity = sd(normalized_intensity, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(month_label = factor(month, levels = 1:12, labels = month.abb))
seas_int_alert <- ggplot(global_interior_intensity, aes(x = month_label, y = mean_intensity, group = 1)) +
  # Add a ribbon for the variation across municipalities
  geom_ribbon(aes(ymin = mean_intensity - sd_intensity, 
                  ymax = mean_intensity + sd_intensity), 
              fill = "steelblue", alpha = 0.2) +
  geom_line(color = "steelblue", linewidth = 1.2) +
  geom_point(color = "steelblue", size = 3) +
  theme_minimal() +
  labs(
    title = "Seasonality Profile: Interior",
    x = "Month",
    y = ""
  ) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )
seas_profile <- seas_edge_alert + seas_int_alert
seas_profile
ggsave("~/secVegDefo/code_output/plots_descrip/seas_profile.png", 
       plot = seas_profile,
       width=9,
       height=4,
       units='in',
       dpi=300)

# Mapbiomas age profile of deforestation of secondary vegetation
plot_data <- fread('~/secVegDefo/data/plot_data/mb_age_prof.csv')
mb_age_prof_plot <- ggplot(plot_data, aes(x = secondary_age, y = area_ha, fill = location_type)) +
  # Adding position = "dodge" places the bars next to each other
  geom_col(position = "dodge", alpha = 0.85) +
  scale_fill_manual(
    values = c("total_edge_ha" = "indianred1", "total_int_ha" = "steelblue"),
    labels = c("Edge", "Interior")
  ) +
  theme_minimal() +
  labs(
    title = "Secondary Deforestation Age Profile (2003-2024)",
    x = "Age of Secondary Vegetation (Years)",
    y = "Total Area Deforested (ha)",
    fill = "Location"
  ) +
  theme(legend.position = "top")
mb_age_prof_plot
ggsave("~/secVegDefo/code_output/plots_descrip/mb_age_prof_plot.png", 
       plot = mb_age_prof_plot,
       width=9,
       height=4,
       units='in',
       dpi=300)




