# Libraries
set.seed(324)
library(shiny)
library(cowplot)
library(shinyWidgets)
library(geobr)
library(randomForest)
library(sf)
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
# all_muni <- read_municipality(
#   year= 2020,
#   showProgress = F,
#   cache=F
# )
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
    values_fill = NA  # Crucial: fills ages not present in a month with NA
  )
age_counts <- deforest_wide %>%
  summarise(across(starts_with("df_perc_area_secondary_prev_age_"), 
                   ~ sum(!is.na(.)))) %>%
  pivot_longer(cols = everything(), 
               names_to = "Age", 
               values_to = "Non_NA_Count") %>%
  mutate(Age = as.numeric(gsub("df_perc_area_secondary_prev_age_", "", Age))) %>%
  dplyr::select(Age, Non_NA_Count) %>%
  arrange(Age)
print(age_counts, n=36)
plot(age_counts)
# Sum all age columns into one total column
deforest_wide <- deforest_wide %>%
  mutate(
    df_perc_area_secondary_prev = rowSums(
      dplyr::select(., starts_with("df_perc_area_secondary_prev_age_")), 
      na.rm = TRUE
    )
  )
# Optional: Remove the individual age columns if you no longer need them
deforest_wide <- deforest_wide %>%
  dplyr::select(-starts_with("df_perc_area_secondary_prev_age_"))

# Get 1 SD change in base units
sd(deforest_wide$df_perc_area_primary_prev)
sd(deforest_wide$df_perc_area_secondary_prev)
sd(deforest_wide$edge_change_lag1_perc, na.rm=T)
sd(deforest_wide$primary_deforest_m2) * 1e-6
sd(deforest_wide$secondary_deforest_m2) * 1e-6
sd(deforest_wide$edge_change, na.rm=T) * 0.01

amazon_states <- c('Rondônia', "Amazônas", 'Acre', "Amapá", "Maranhão",
                   "Mato Grosso", "Pará", "Roraima", "Tocantins")
# amazon_states <- c('All')

# Save data for each state
for (input_state in amazon_states) {
  state_abrev <- switch(EXPR = input_state, "Acre"="AC", "Amapá"="AP", "Amazônas"="AM", 
                        "Maranhão"="MA", "Mato Grosso"="MT", "Pará"="PA", 
                        "Rondônia"='RO', "Roraima"="RR", "Tocantins"="TO", "All"="All")
  if (state_abrev != 'All') {
    all_muni_sub <- all_muni[which(all_muni$abbrv_s == state_abrev),]
    all_muni_sub$mun_exp <- substr(all_muni_sub$code_mn,1,6)
    if (state_abrev == 'PA') {
      all_muni_sub <- all_muni_sub[all_muni_sub$code_mn!=1504752,]
    }
  } else {
    all_muni_sub <- all_muni[which(all_muni$abbrv_s %in% c('AC', 'AP', 'AM', 'MA', 'MT', 'PA', 'RO', 'RR', 'TO')),]
    all_muni_sub$mun_exp <- substr(all_muni_sub$code_mn,1,6)
    all_muni_sub <- all_muni_sub[all_muni_sub$code_mn!=1504752,]
  }
  deforest_sub <- deforest_wide[which(deforest_wide$mun_exp %in% all_muni_sub$mun_exp),]
  deforest_sub <- deforest_sub[order(deforest_sub$Date),]
  d1_full <- deforest_sub
  # Keep BOTH columns so we can link them to the data
  muns2 <- all_muni_sub[, c("code_mn", "mun_exp")]
  
  # Create the spatial neighborhood matrix (this is for INLA specification)
  temp <- poly2nb(muns2, row.names = row.names(muns2))
  nb2INLA("mun.graph", temp)
  mun.adj <- paste(getwd(),"/mun.graph",sep="")
  H <- inla.read.graph(filename="mun.graph")
  #image(inla.graph2matrix(H),xlab="",ylab="")
  cosine_formula <- function(t, input_state) {
    t <- t
    if (input_state == "Acre") {
      y <- -28.94 * sin(2*pi*(t+207.86)/213.79) + 29.78
    } else if (input_state == "Amapá") {
      y <- -0.53 * sin(2*pi*(t+7.23)/11.68) + 0.99
    } else if (input_state == "Amazônas") {
      y <- -0.25 * sin(2*pi*(t+7.42)/9.82) + 1.03
    } else if (input_state == "Maranhão") {
      y <- -0.48 * sin(2*pi*(t+8.14)/10.58) + 1.03
    } else if (input_state == "Mato Grosso") {
      y <- -0.27 * sin(2*pi*(t+10.34)/12.96) + 0.98
    } else if (input_state == "Pará") {
      y <- -0.21 * sin(2*pi*(t+6.08)/9.91) + 1
    } else if (input_state == "Rondônia") {
      y <- -0.22 * sin(2*pi*(t+9.61)/12.26) + 0.99
    } else if (input_state == "Roraima") {
      y <- 0.85 * sin(2*pi*(t+54.77)/41.24) + 1.73
    } else if (input_state == "Tocantins") {
      y <- 0.46 * sin(2*pi*(t+18.58)/23.24) + 0.7
    } else {
      amplitude <- (1.193519 - 0.8383214) / 2
      vertical_shift <- 1.02
      frequency <- pi / 5
      y <- amplitude * cos(frequency * (t -8.033213)) + vertical_shift
    }
    return(y)
  }
  d1_full$month_control_cosine <- cosine_formula(d1_full$month, input_state)
  muns_lookup <- data.frame(
    mun_exp = muns2$mun_exp, 
    munID_correct = 1:nrow(muns2)
  )
  muns_lookup$mun_exp <- as.character(muns_lookup$mun_exp)
  d1_full$mun_exp     <- as.character(d1_full$mun_exp)
  d1_full <- merge(d1_full, muns_lookup, by = "mun_exp", all.x = TRUE)
  if(any(is.na(d1_full$munID_correct))) {
    print(paste("Warning:", sum(is.na(d1_full$munID_correct)), "rows don't match."))
    d1_full <- d1_full[!is.na(d1_full$munID_correct), ]
  }
  d1_full$mun.1 <- d1_full$munID_correct
  d1_full$mun.2 <- d1_full$munID_correct
  
  # Equidistant bins
  n_bins <- 26
  # Temperature
  tmax_max_range <- range(d1_full$tmax_max, na.rm = TRUE)
  tmax_max_grid <- seq(tmax_max_range[1], tmax_max_range[2], length.out = n_bins)
  bin_tmax_max_indices <- findInterval(d1_full$tmax_max, tmax_max_grid, all.inside = T)
  d1_full$tmax_max_grouped <- tmax_max_grid[bin_tmax_max_indices]
  # Precipitation
  p_tot_range <- range(d1_full$p_tot, na.rm = TRUE)
  p_tot_grid <- seq(p_tot_range[1], p_tot_range[2], length.out = n_bins)
  bin_p_tot_indices <- findInterval(d1_full$p_tot, p_tot_grid, all.inside = T)
  d1_full$p_tot_grouped <- p_tot_grid[bin_p_tot_indices]
  # ONI
  n_bins_ONI <- 51
  ONI_range <- range(d1_full$ONI, na.rm = TRUE)
  ONI_grid <- seq(ONI_range[1], ONI_range[2], length.out = n_bins_ONI)
  bin_ONI_indices <- findInterval(d1_full$ONI, ONI_grid, all.inside = T)
  d1_full$ONI_grouped <- ONI_grid[bin_ONI_indices]
  # Priors
  pc_prec_prior <- list(prec = list(prior = "pc.prec", param = c(2, 0.01)))
  pc_ar1_prior  <- list(prec = list(prior = "pc.prec", param = c(2, 0.01)), 
                        rho  = list(prior = "pc.cor0", param = c(0.5, 0.5)))
  
  # Model equation
  if (state_abrev != 'TO' & state_abrev != 'AC') {
    m21_sub <- cases ~ 1 +
      scale(df_perc_area_primary_prev) +
      scale(df_perc_area_secondary_prev) +
      scale(area) +
      scale(deforestation_change_12mo_period_primary) +
      scale(forested_area_perc) +
      scale(population) +
      imp_local +
      scale(cons_area_perc) +
      scale(garimpo_cases_perc) +
      scale(settlement_cases_perc) +
      scale(rural_cases_perc) +
      scale(indigenous_cases_perc) +
      scale(Unpaved) +
      scale(Paved) +
      scale(edge_change_lag1_perc) +
      month_control_cosine +
      as.factor(year) +
      scale(df_perc_area_primary_prev)*scale(forested_area_perc) +
      f(tmax_max_grouped, model="ar1", values=tmax_max_grid, hyper = pc_ar1_prior) +
      f(p_tot_grouped, model="ar1", values=p_tot_grid, hyper = pc_ar1_prior) +
      f(mun.1, model="bym", graph=mun.adj, scale.model=T, hyper = list(theta1 = pc_prec_prior$prec, theta2 = pc_prec_prior$prec)) +
      f(int_mun_time, model="iid", hyper = pc_prec_prior) +
      f(mun.2, model="iid", hyper=pc_prec_prior) +
      f(ONI_grouped, model="ar1", values=ONI_grid, hyper = pc_ar1_prior)
  } else {
    m21_sub <- cases ~ 1 +
      scale(df_perc_area_primary_prev) +
      scale(df_perc_area_secondary_prev) +
      scale(area) +
      scale(deforestation_change_12mo_period_primary) +
      scale(forested_area_perc) +
      scale(population) +
      imp_local +
      scale(cons_area_perc) +
      scale(settlement_cases_perc) +
      scale(rural_cases_perc) +
      scale(indigenous_cases_perc) +
      scale(Unpaved) +
      scale(Paved) +
      scale(edge_change_lag1_perc) +
      month_control_cosine +
      as.factor(year) +
      scale(df_perc_area_primary_prev)*scale(forested_area_perc) +
      f(tmax_max_grouped, model="ar1", values=tmax_max_grid, hyper = pc_ar1_prior) +
      f(p_tot_grouped, model="ar1", values=p_tot_grid, hyper = pc_ar1_prior) +
      f(mun.1, model="bym", graph=mun.adj, scale.model=T, hyper = list(theta1 = pc_prec_prior$prec, theta2 = pc_prec_prior$prec)) +
      f(int_mun_time, model="iid", hyper = pc_prec_prior) +
      f(mun.2, model="iid", hyper=pc_prec_prior) +
      f(ONI_grouped, model="ar1", values=ONI_grid, hyper = pc_ar1_prior)
  }
  d1_full_sub <- d1_full
  if (state_abrev != 'All') {
    m21_sub_results_sub <- inla(m21_sub, family="zeroinflatednbinomial0", data=d1_full_sub,
                                # control.predictor=list(compute=TRUE,link=NA),
                                control.compute=list(dic=TRUE, waic = TRUE, cpo = TRUE, config=TRUE),
                                # control.predictor = list(compute = TRUE, link=1), # This defines family connection for unobserved values https://rdrr.io/github/inbo/INLA/man/control.predictor.html 
                                verbose = T, safe = T, 
                                control.inla = list(
                                  tolerance = 1e-3,
                                  int.strategy = "eb"),
                                num.threads=6)
  } else {
    current_theta <- c(-0.1550, 0.8479, 1.9254, 5.4250, 3.8419, -1.8360,
                       1.5691, -1.2000, 2.1426, 0.9960, 3.5121, 4.0349)
    m21_sub_results_sub <- inla(
      m21_sub, family = "zeroinflatednbinomial0", data = d1_full_sub,
      control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE, config = TRUE),
      verbose = TRUE, safe = TRUE, # You can keep this, but diagonal=1e-2 is often more effective
      control.mode = list(theta = current_theta, restart = TRUE), # START HERE
      control.inla = list(
        tolerance = 1e-3,
        int.strategy = "eb"
      ),
      num.threads = 6
    )
  }
  
  # Write results
  m21_sub_summary <- exp(m21_sub_results_sub$summary.fixed)
  fwrite(m21_sub_summary, file=paste0('~/Desktop/secDef/mon_results/', state_abrev, '_monNew.csv'), row.names=T)
  if (state_abrev == 'All') {
    save(m21_sub_results_sub, file='~/Desktop/secDef/mon_results/All_monNew.RData')
  }
}


