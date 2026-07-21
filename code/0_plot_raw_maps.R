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
  summarise(prim_defo = sum(monthly_deforest_m2) * 0.0001)
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
  summarise(sec_defo = sum(sec_defo_tot_m2_adj) * 0.0001)

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

# First load primary deforestation heatmap
p <- ggplot(data = map_data) +
  # Map the fill color to your weighted average column
  geom_sf(aes(fill = prim_defo), color = "white", size = 0.1) +
  geom_sf(data = amazon_states, fill = NA, color = "black", linewidth = 0.6) +
  geom_sf_text(
    data = amazon_states, 
    aes(label = postal), # Replace 'postal' with your actual column name if different
    size = 4,            # Adjust text size
    color = "black",     # Text color
    fontface = "bold"    # Make it stand out
  ) +
  # Use a colorblind-friendly continuous color scale
  scale_fill_viridis_c(option = "plasma", name = "Prim. Defo.") +
  # Clean up the background
  theme_minimal() +
  # Add titles
  labs(
    title = "Primary deforestation by municipality",
    #subtitle = "Choropleth Map of mun_exp values"
  ) +
  # Remove axis text and grid lines for a cleaner map look
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )
p

s <- ggplot(data = map_data) +
  # Map the fill color to your weighted average column
  geom_sf(aes(fill = sec_defo), color = "white", size = 0.1) +
  geom_sf(data = amazon_states, fill = NA, color = "black", linewidth = 0.6) +
  geom_sf_text(
    data = amazon_states, 
    aes(label = postal), # Replace 'postal' with your actual column name if different
    size = 4,            # Adjust text size
    color = "black",     # Text color
    fontface = "bold"    # Make it stand out
  ) +
  # Use a colorblind-friendly continuous color scale
  scale_fill_viridis_c(option = "plasma", name = "Sec. Defo.") +
  # Clean up the background
  theme_minimal() +
  # Add titles
  labs(
    title = "Secondary deforestation by municipality",
    #subtitle = "Choropleth Map of mun_exp values"
  ) +
  # Remove axis text and grid lines for a cleaner map look
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )
s


# Should we also show the crossing of quartiles of cases and secondary deforestation?



# Take ideas from this
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

# Read in municipality
all_muni <- st_read("~/Desktop/secDef/muni_mun_exp/muni_mun_exp.shp")

# Read deforestation data
deforest <- fread('~/Desktop/secDef/mer_mon_age_final.csv')

# Get rid of rows that are not needed for analysis
deforest <- deforest[!is.na(deforest$sec_monthly_deforest_m2),]
deforest <- deforest[!is.na(deforest$population),]
deforest$sum_last_12_sec <- NULL
deforest$sum_prev_12_sec <- NULL
deforest$globalfund <- NULL
deforest$Bolsonaro <- NULL
deforest$bednets2007 <- NULL
deforest$ACT <- NULL
deforest$deforestation_change_12mo_period_secondary <- NULL
deforest$sum_last_12_pri <- NULL
deforest$sum_prev_12_pri <- NULL
deforest$sec_monthly_deforest_m2 <- NULL
deforest$df_perc_forest_secondary_prev <- NULL
deforest$prev_month_sec_deforestation <- NULL
deforest_save <- deforest

# Get one row for each mun_exp-year-month combination
deforest_wide <- deforest %>%
  pivot_wider(
    names_from = secondary_age, 
    values_from = df_perc_area_secondary_prev,
    names_prefix = "df_perc_area_secondary_prev_age_",
    values_fill = NA  # Kept as NA to preserve cohort logic!
  )
rm(deforest)


deforest_wide_yr <- deforest_wide %>% 
  group_by(mun_exp, year) %>%
  summarise(uf = first(uf),
            sum_2 = sum(df_perc_area_secondary_prev_age_2, na.rm=T),
            sum_3 = sum(df_perc_area_secondary_prev_age_3, na.rm=T),
            sum_4 = sum(df_perc_area_secondary_prev_age_4, na.rm=T),
            sum_5 = sum(df_perc_area_secondary_prev_age_5, na.rm=T),
            sum_6 = sum(df_perc_area_secondary_prev_age_6, na.rm=T),
            sum_7 = sum(df_perc_area_secondary_prev_age_7, na.rm=T),
            sum_8 = sum(df_perc_area_secondary_prev_age_8, na.rm=T),
            sum_9 = sum(df_perc_area_secondary_prev_age_9, na.rm=T),
            sum_10 = sum(df_perc_area_secondary_prev_age_10, na.rm=T),
            sum_11 = sum(df_perc_area_secondary_prev_age_11, na.rm=T),
            sum_12 = sum(df_perc_area_secondary_prev_age_12, na.rm=T),
            sum_13 = sum(df_perc_area_secondary_prev_age_13, na.rm=T),
            sum_14 = sum(df_perc_area_secondary_prev_age_14, na.rm=T),
            sum_15 = sum(df_perc_area_secondary_prev_age_15, na.rm=T),
            sum_16 = sum(df_perc_area_secondary_prev_age_16, na.rm=T),
            sum_17 = sum(df_perc_area_secondary_prev_age_17, na.rm=T),
            sum_18 = sum(df_perc_area_secondary_prev_age_18, na.rm=T),
            sum_19 = sum(df_perc_area_secondary_prev_age_19, na.rm=T),
            sum_20 = sum(df_perc_area_secondary_prev_age_20, na.rm=T),
            sum_21 = sum(df_perc_area_secondary_prev_age_21, na.rm=T),
            sum_22 = sum(df_perc_area_secondary_prev_age_22, na.rm=T),
            sum_23 = sum(df_perc_area_secondary_prev_age_23, na.rm=T),
            sum_24 = sum(df_perc_area_secondary_prev_age_24, na.rm=T),
            sum_25 = sum(df_perc_area_secondary_prev_age_25, na.rm=T),
            sum_26 = sum(df_perc_area_secondary_prev_age_26, na.rm=T),
            sum_27 = sum(df_perc_area_secondary_prev_age_27, na.rm=T),
            sum_28 = sum(df_perc_area_secondary_prev_age_28, na.rm=T),
            sum_29 = sum(df_perc_area_secondary_prev_age_29, na.rm=T),
            sum_30 = sum(df_perc_area_secondary_prev_age_30, na.rm=T),
            sum_31 = sum(df_perc_area_secondary_prev_age_31, na.rm=T),
            sum_32 = sum(df_perc_area_secondary_prev_age_32, na.rm=T),
            sum_33 = sum(df_perc_area_secondary_prev_age_33, na.rm=T),
            sum_34 = sum(df_perc_area_secondary_prev_age_34, na.rm=T),
            sum_35 = sum(df_perc_area_secondary_prev_age_35, na.rm=T),
            sum_36 = sum(df_perc_area_secondary_prev_age_36, na.rm=T))
deforest_wide_yr$tot_sum <- rowSums(deforest_wide_yr[,4:38])

deforest_wide_yr$weight_sum_2 <- deforest_wide_yr$sum_2 * 2
deforest_wide_yr$weight_sum_3 <- deforest_wide_yr$sum_3 * 3
deforest_wide_yr$weight_sum_4 <- deforest_wide_yr$sum_4 * 4
deforest_wide_yr$weight_sum_5 <- deforest_wide_yr$sum_5 * 5
deforest_wide_yr$weight_sum_6 <- deforest_wide_yr$sum_6 * 6
deforest_wide_yr$weight_sum_7 <- deforest_wide_yr$sum_7 * 7
deforest_wide_yr$weight_sum_8 <- deforest_wide_yr$sum_8 * 8
deforest_wide_yr$weight_sum_9 <- deforest_wide_yr$sum_9 * 9 
deforest_wide_yr$weight_sum_10 <- deforest_wide_yr$sum_10 * 10
deforest_wide_yr$weight_sum_11 <- deforest_wide_yr$sum_11 * 11
deforest_wide_yr$weight_sum_12 <- deforest_wide_yr$sum_12 * 12
deforest_wide_yr$weight_sum_13 <- deforest_wide_yr$sum_13 * 13
deforest_wide_yr$weight_sum_14 <- deforest_wide_yr$sum_14 * 14
deforest_wide_yr$weight_sum_15 <- deforest_wide_yr$sum_15 * 15
deforest_wide_yr$weight_sum_16 <- deforest_wide_yr$sum_16 * 16
deforest_wide_yr$weight_sum_17 <- deforest_wide_yr$sum_17 * 17
deforest_wide_yr$weight_sum_18 <- deforest_wide_yr$sum_18 * 18
deforest_wide_yr$weight_sum_19 <- deforest_wide_yr$sum_19 * 19
deforest_wide_yr$weight_sum_20 <- deforest_wide_yr$sum_20 * 20
deforest_wide_yr$weight_sum_21 <- deforest_wide_yr$sum_21 * 21
deforest_wide_yr$weight_sum_22 <- deforest_wide_yr$sum_22 * 22
deforest_wide_yr$weight_sum_23 <- deforest_wide_yr$sum_23 * 23
deforest_wide_yr$weight_sum_24 <- deforest_wide_yr$sum_24 * 24
deforest_wide_yr$weight_sum_25 <- deforest_wide_yr$sum_25 * 25
deforest_wide_yr$weight_sum_26 <- deforest_wide_yr$sum_26 * 26
deforest_wide_yr$weight_sum_27 <- deforest_wide_yr$sum_27 * 27
deforest_wide_yr$weight_sum_28 <- deforest_wide_yr$sum_28 * 28
deforest_wide_yr$weight_sum_29 <- deforest_wide_yr$sum_29 * 29
deforest_wide_yr$weight_sum_30 <- deforest_wide_yr$sum_30 * 30
deforest_wide_yr$weight_sum_31 <- deforest_wide_yr$sum_31 * 31
deforest_wide_yr$weight_sum_32 <- deforest_wide_yr$sum_32 * 32
deforest_wide_yr$weight_sum_33 <- deforest_wide_yr$sum_33 * 33
deforest_wide_yr$weight_sum_34 <- deforest_wide_yr$sum_34 * 34
deforest_wide_yr$weight_sum_35 <- deforest_wide_yr$sum_35 * 35
deforest_wide_yr$weight_sum_36 <- deforest_wide_yr$sum_36 * 36
deforest_wide_yr$tot_weight_sum <- rowSums(deforest_wide_yr[,40:74])

deforest_wide_yr$weighted_average <- deforest_wide_yr$tot_weight_sum / deforest_wide_yr$tot_sum

deforest_wide_yr$mun_exp <- as.character(deforest_wide_yr$mun_exp)
map_data <- all_muni %>%
  left_join(deforest_wide_yr, by = "mun_exp")

# 1. Download Brazilian states from Natural Earth
br_states <- ne_states(country = "Brazil", returnclass = "sf")
# 2. Filter for the 9 states in the Legal Amazon
amazon_states <- br_states %>%
  filter(name %in% c("Acre", "Amapá", "Amazonas", "Maranhão", 
                     "Mato Grosso", "Pará", "Rondônia", "Roraima", "Tocantins"))

p <- ggplot(data = map_data) +
  # Map the fill color to your weighted average column
  geom_sf(aes(fill = weighted_average), color = "white", size = 0.1) +
  
  geom_sf(data = amazon_states, fill = NA, color = "black", linewidth = 0.6) +
  
  geom_sf_text(
    data = amazon_states, 
    aes(label = postal), # Replace 'postal' with your actual column name if different
    size = 4,            # Adjust text size
    color = "black",     # Text color
    fontface = "bold"    # Make it stand out
  ) +
  
  # Use a colorblind-friendly continuous color scale
  scale_fill_viridis_c(option = "plasma", name = "Average Age\nSec. defo.") +
  
  # Clean up the background
  theme_minimal() +
  
  # Add titles
  labs(
    title = "Average age of secondary deforestation",
    #subtitle = "Choropleth Map of mun_exp values"
  ) +
  
  # Remove axis text and grid lines for a cleaner map look
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

ggsave(
  filename = "~/Desktop/deforestation_heatmap.pdf", 
  plot = p, 
  width = 10,     # Width of the PDF in inches
  height = 8,     # Height of the PDF in inches
  dpi = 300       # Resolution (good for print)
)

# Box plots per state
deforest_wide_uf <- deforest_wide_yr %>%
  group_by(uf, year) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE))

library(tidyverse)

# 1. Prepare the data
deforest_long <- deforest_wide_uf %>%
  pivot_longer(
    cols = starts_with("sum_"),
    names_to = "age",
    names_prefix = "sum_",
    values_to = "deforestation_amount"
  ) %>%
  mutate(
    age = as.numeric(age),      # Age must be numeric for the y-axis
    uf = as.character(uf)       # UF MUST be a character/factor so it creates separate groups!
  )

# 2. Build the faceted plot
ggplot(deforest_long, aes(x = "", y = age, weight = deforestation_amount, fill = uf)) +
  geom_violin(trim = TRUE, alpha = 0.7) +
  facet_wrap(~ uf) +            # This creates the separate panels for each UF
  theme_minimal() +
  labs(
    title = "Deforestation Intensity by Age per UF",
    x = NULL,                   # Removes the dummy x-axis label
    y = "Age of Forest",
    subtitle = "Wider sections indicate higher amounts of deforestation at that age"
  ) +
  theme(
    legend.position = "none",
    axis.text.x = element_blank(),  # Removes the empty text underneath the violins
    axis.ticks.x = element_blank(),
    panel.grid.major.x = element_blank() # Cleans up vertical grid lines
  )


library(tidyverse)

# 1. Prepare the data (same as before)
deforest_long <- deforest_wide_uf %>%
  pivot_longer(
    cols = starts_with("sum_"),
    names_to = "age",
    names_prefix = "sum_",
    values_to = "deforestation_amount"
  ) %>%
  mutate(
    age = as.numeric(age),      
    uf = as.character(uf)       
  )

# 2. Build the faceted boxplot
ggplot(deforest_long, aes(x = "", y = age, weight = deforestation_amount, fill = uf)) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap(~ uf) +
  theme_minimal() +
  labs(
    title = "Deforestation Age Distribution per UF",
    x = NULL, 
    y = "Age of Forest"#,
    #subtitle = "Box spans the 25th to 75th percentiles of deforestation age; middle line is the median"
  ) +
  theme(
    legend.position = "none",
    axis.text.x = element_blank(),       # Removes the empty x-axis text
    axis.ticks.x = element_blank(),      # Removes the little tick marks
    panel.grid.major.x = element_blank() # Cleans up the background
  )

