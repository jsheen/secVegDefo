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
library(raster)
library(spacetime)
library(data.table)
library(INLA)
library(dplyr)
library(splines)
library(tidyverse)

deforest_wide <- fread(file="~/Desktop/secDef/deforest_wide_InteriorEdge.csv")
all_muni <- st_read("~/Desktop/secDef/muni_mun_exp/muni_mun_exp.shp")

load('~/Desktop/secDef/mon_results/age_prof/smooth_All_sub_InteriorEdge.RData')
beta_matrix <- spline_outputs$beta_matrix_raw 
as_sds      <- spline_outputs$as_sds
sd_raw      <- spline_outputs$sd_by_age_raw

# Draw from posterior
load(paste0('~/Desktop/secDef/mon_results/All_age_smooth_sub_InteriorEdge.RData'))
target_vars <- list(as1 = 1, as2 = 1, as3 = 1, as4 = 1, Prop_Edge = 1)
samples <- inla.posterior.sample(
  n = 1000,
  result = m21_sub_results_sub,
  selection = target_vars,
  parallel.configs = FALSE, 
  num.threads = "1:1"        
)
spline_names <- c("as1", "as2", "as3", "as4", "Prop_Edge")
latent_names_sample1 <- rownames(samples[[1]]$latent)
match_indices <- numeric(5)
for(j in 1:5) {
  target_name <- paste0("^", spline_names[j], ":1$")
  idx <- grep(target_name, latent_names_sample1)
  
  if(length(idx) == 0) {
    idx <- grep(paste0("^", spline_names[j], "$"), latent_names_sample1)
  }
  
  if(length(idx) == 1) {
    match_indices[j] <- idx
  } else {
    stop(paste("Could not uniquely find coefficient for", spline_names[j]))
  }
}
beta_matrix_raw <- sapply(samples, function(s) s$latent[match_indices, 1])

state_name <- 'All'
all_muni_sub <- all_muni[which(all_muni$abbrv_s %in% c('AC', 'AP', 'AM', 'MA', 'MT', 'PA', 'RO', 'RR', 'TO')),]
all_muni_sub$mun_exp <- substr(all_muni_sub$code_mn,1,6)
all_muni_sub <- all_muni_sub[all_muni_sub$code_mn!=1504752,]

deforest_sub <- deforest_wide[which(deforest_wide$mun_exp %in% all_muni_sub$mun_exp),]
muns2 <- all_muni_sub[, c("code_mn", "mun_exp")]
muns_lookup <- data.frame(mun_exp = as.character(muns2$mun_exp), munID_correct = 1:nrow(muns2))
deforest_sub$mun_exp <- as.character(deforest_sub$mun_exp)
d1_full <- merge(deforest_sub, muns_lookup, by = "mun_exp", all.x = TRUE)
d1_full <- d1_full[!is.na(d1_full$munID_correct), ]

age_range <- 3:33
B_4 <- bs(age_range, df = 4, intercept = TRUE)
age_cols_names <- paste0("df_perc_area_secondary_prev_age_", age_range)
age_matrix <- as.matrix(d1_full[, ..age_cols_names])

W <- !is.na(age_matrix)
actual_basis_sum_matrix <- W %*% B_4
actual_basis_sum_matrix[actual_basis_sum_matrix == 0] <- 1
actual_basis_sum <- colMeans(actual_basis_sum_matrix, na.rm = TRUE)

# Show for several age, if we set the age, then we get back the estimated confidence interval. And if we 
# multiply by 10, we get 10 times the risk estimate
back_project <- function(target_age, defo_factor_increase) {
  age_index <- target_age - 2
  age_basis <- B_4[age_index, ]
  
  sd_defo <- sd(age_matrix[,age_index], na.rm=T)
  projected_increase <- sd_defo * defo_factor_increase * age_basis
  
  age_scaled <- (projected_increase) / (as_sds * actual_basis_sum)
  
  log_rr_samples <- t(age_scaled) %*% beta_matrix_raw[1:4, ]
  rr_samples <- as.numeric(exp(log_rr_samples))
  
  df_samples <- data.frame(RR = rr_samples)
  
  # Calculate summary statistics for the plot lines
  med_rr <- median(df_samples$RR)
  lower_ci <- quantile(df_samples$RR, 0.025)
  upper_ci <- quantile(df_samples$RR, 0.975)
  print(paste0(round(med_rr, 3), ' [ ', round(lower_ci, 3), ', ', round(upper_ci, 3), ' ]'))
  
  # Create the density plot
  ggplot(df_samples, aes(x = RR)) +
    geom_density(fill = "#00BFC4", alpha = 0.5, color = NA) +
    geom_vline(xintercept = 1, linetype = "dashed", color = "black", size = 1) +
    geom_vline(xintercept = med_rr, color = "blue", size = 1.2) +
    geom_vline(xintercept = c(lower_ci, upper_ci), color = "blue", linetype = "dotted", size = 1) +
    theme_minimal() +
    labs(
      title = "Posterior Distribution of Relative Risk",
      subtitle = paste("Median:", round(med_rr, 2), " (95% CI:", round(lower_ci, 2), "-", round(upper_ci, 2), ")"),
      x = "Relative Risk",
      y = "Density"
    )
}

back_project(8, 1)
back_project(8, 10)
back_project(17, 1)
back_project(17, 10)
back_project(30, 1)
back_project(30, 10)






