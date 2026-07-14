library(ggplot2)
library(dplyr)
library(splines)
library(data.table)
library(sf)

# Load Data
data_dir <- "~/Desktop/secDef/mon_results/age_prof/"
files <- list.files(data_dir, pattern = "smooth_.*_sub_InteriorEdge.RData", full.names = TRUE)

deforest_wide <- fread(file="~/Desktop/secDef/deforest_wide_InteriorEdge.csv")
all_muni <- st_read("~/Desktop/secDef/muni_mun_exp/muni_mun_exp.shp")

all_plot_data <- data.frame()

for (f in files) {
  load(f) 
  state_name <- gsub(".*smooth_(.*)_sub_InteriorEdge.RData", "\\1", basename(f))
  mean_edge_val <- 0.755
  
  if (state_name == 'All') {
    all_muni_sub <- all_muni[which(all_muni$abbrv_s %in% c('AC', 'AP', 'AM', 'MA', 'MT', 'PA', 'RO', 'RR', 'TO')),]
  } else {
    all_muni_sub <- all_muni[which(all_muni$abbrv_s == state_name),]
  }
  all_muni_sub$mun_exp <- substr(all_muni_sub$code_mn,1,6)
  
  if (state_name == 'PA' || state_name == 'All') {
    all_muni_sub <- all_muni_sub[all_muni_sub$code_mn!=1504752,]
  }
  
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
  
  beta_matrix <- spline_outputs$beta_matrix_raw
  as_sds      <- spline_outputs$as_sds
  sd_raw      <- spline_outputs$sd_by_age_raw
  
  B_scaled_corrected <- t(t(as.matrix(B_4)) / (as_sds * actual_basis_sum))
  
  sd_smooth <- pmax(smooth.spline(age_range, sd_raw, spar = 0.7)$y, 1e-8)
  
  log_rr_base <- (B_scaled_corrected %*% beta_matrix[1:4, ]) * sd_smooth
  log_rr_avg  <- sweep(log_rr_base, MARGIN = 2, STATS = beta_matrix[5, ] * mean_edge_val, FUN = "+")
  
  df_temp <- data.frame(
    Age = age_range,
    Effect = exp(apply(log_rr_avg, 1, median)),
    Lower  = exp(apply(log_rr_avg, 1, function(x) quantile(x, 0.025))),
    Upper  = exp(apply(log_rr_avg, 1, function(x) quantile(x, 0.975))),
    State  = state_name
  )
  all_plot_data <- rbind(all_plot_data, df_temp)
}

p1 <- ggplot(all_plot_data %>% filter(State == "All"), aes(x = Age, y = Effect)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "black", alpha = 0.1, color = NA) +
  geom_line(color = "black", size = 1.2) +
  theme_minimal() +
  theme(aspect.ratio = 0.5) +
  labs(title = "Relative Risk (All States): Population-Average Trajectory", y = "Relative Risk")

p2 <- ggplot(all_plot_data %>% filter(State != "All"), aes(x = Age, y = Effect)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "black", alpha = 0.1, color = NA) +
  geom_line(color = "black", size = 1.0) +
  facet_wrap(~State, ncol = 3, scales = "free_y") +
  theme_minimal() + 
  theme(
    aspect.ratio = 0.5, 
    panel.spacing = unit(1.5, "lines"), 
    plot.title = element_text(size = 16, face = "bold")
  ) + 
  labs(title = "Relative Risk by State: Population-Average Trajectory", y = "Relative Risk")

output_pdf <- "~/secVegDefo/code_output/plots_mod/IntEdge_AvgOnly.pdf"
# Adding dir.create just in case the folder doesn't exist
dir.create(dirname(output_pdf), showWarnings = FALSE, recursive = TRUE)

pdf(output_pdf, width = 16, height = 9)
print(p1)
print(p2)
dev.off()
print(paste("Plots saved to", output_pdf))