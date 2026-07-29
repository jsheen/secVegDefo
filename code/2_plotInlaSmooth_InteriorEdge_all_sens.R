library(ggplot2)
library(dplyr)
library(splines)
library(data.table)
library(sf)
library(patchwork)

# ==============================================================================
# Load data
# ==============================================================================
deforest_wide <- fread(file="~/Desktop/secDef/deforest_wide_InteriorEdge.csv")
all_muni <- st_read("~/secVegDefo/data/muni_mun_exp/muni_mun_exp.shp")


# ==============================================================================
# Penalized
# ==============================================================================
data_dir <- "~/Desktop/secDef/mon_results/age_prof/"
files_alt <- list.files(data_dir, pattern = "smooth_.*_sub_altInteriorEdge.RData", full.names = TRUE)

all_plot_data_alt <- data.frame()

for (f in files_alt) {
  load(f) 
  state_name <- gsub(".*smooth_(.*)_sub_altInteriorEdge.RData", "\\1", basename(f))
  
  beta_matrix <- spline_outputs$beta_matrix_raw 
  as_sds      <- spline_outputs$as_sds
  sd_raw      <- spline_outputs$sd_by_age_raw
  
  num_nodes <- nrow(beta_matrix) - 1 
  
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
  B_mat <- bs(age_range, df = num_nodes, intercept = TRUE)
  age_cols_names <- paste0("df_perc_area_secondary_prev_age_", age_range)
  age_matrix <- as.matrix(d1_full[, ..age_cols_names])
  
  W <- !is.na(age_matrix)
  actual_basis_sum_matrix <- W %*% B_mat
  actual_basis_sum_matrix[actual_basis_sum_matrix == 0] <- 1
  actual_basis_sum <- colMeans(actual_basis_sum_matrix, na.rm = TRUE)
  
  B_scaled_corrected <- t(t(as.matrix(B_mat)) / (as_sds * actual_basis_sum))
  sd_smooth <- pmax(smooth.spline(age_range, sd_raw, spar = 0.7)$y, 1e-8)
  
  log_rr_base <- (B_scaled_corrected %*% beta_matrix[1:num_nodes, ]) * sd_smooth
  log_rr_edge <- sweep(log_rr_base, MARGIN = 2, STATS = beta_matrix[num_nodes + 1, ], FUN = "+")
  
  df_base <- data.frame(
    Age = age_range, Effect = exp(apply(log_rr_base, 1, median)),
    Lower = exp(apply(log_rr_base, 1, function(x) quantile(x, 0.025))),
    Upper = exp(apply(log_rr_base, 1, function(x) quantile(x, 0.975))),
    State = state_name, Type = "Interior (0% Edge)"
  )
  
  df_edge <- data.frame(
    Age = age_range, Effect = exp(apply(log_rr_edge, 1, median)),
    Lower = exp(apply(log_rr_edge, 1, function(x) quantile(x, 0.025))),
    Upper = exp(apply(log_rr_edge, 1, function(x) quantile(x, 0.975))),
    State = state_name, Type = "100% Edge"
  )
  
  all_plot_data_alt <- rbind(all_plot_data_alt, df_base, df_edge)
}

p_alt <- ggplot(all_plot_data_alt %>% filter(State == "All"), aes(x = Age, y = Effect, color = Type, fill = Type)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = 0.15, color = NA) +
  geom_line(size = 1.2) +
  scale_color_manual(values = c("Interior (0% Edge)" = "#00BFC4", "100% Edge" = "#C77CFF")) +
  scale_fill_manual(values = c("Interior (0% Edge)" = "#00BFC4", "100% Edge" = "#C77CFF")) +
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  labs(title = "A) Penalized B-spline (degree = 4)", y = "Relative Risk")


# ==============================================================================
# No Proportion Edge (Base)
# ==============================================================================
files_base <- list.files(data_dir, pattern = "smooth_.*_sub.RData", full.names = TRUE)
all_plot_data_base <- data.frame()

for (f in files_base) {
  load(f) 
  state_abrev <- gsub(".*smooth_(.*)_sub.RData", "\\1", basename(f))
  
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
  actual_basis_sum <- W %*% B_4
  actual_basis_sum[actual_basis_sum == 0] <- 1
  mean_basis_sum <- colMeans(actual_basis_sum, na.rm = TRUE)
  
  beta_raw <- spline_outputs[[1]]
  as_sds   <- spline_outputs[[2]]
  sd_raw   <- spline_outputs[[4]]
  
  B_4_scaled_corrected <- t(t(as.matrix(B_4)) / (as_sds * mean_basis_sum))
  log_rr_raw <- B_4_scaled_corrected %*% beta_raw
  sd_smooth  <- pmax(smooth.spline(age_range, sd_raw, spar = 0.7)$y, 1e-8)
  log_rr_1SD <- log_rr_raw * sd_smooth
  
  df_temp <- data.frame(
    Age = age_range, Effect = exp(apply(log_rr_1SD, 1, median)),
    Lower = exp(apply(log_rr_1SD, 1, function(x) quantile(x, 0.025))),
    Upper = exp(apply(log_rr_1SD, 1, function(x) quantile(x, 0.975))),
    State = state_abrev
  )
  all_plot_data_base <- rbind(all_plot_data_base, df_temp)
}

p_base <- ggplot(all_plot_data_base %>% filter(State == "All"), aes(x = Age, y = Effect)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "blue", alpha = 0.2) +
  geom_line(color = "blue", size = 1.2) +
  theme_minimal() + 
  labs(title = "B) No Prop. Edge (B-spline, degree = 4)", y = "Relative Risk")


# ==============================================================================
# 5 Degrees of Freedom
# ==============================================================================
files_5df <- list.files(data_dir, pattern = "smooth_.*_sub_5df.RData", full.names = TRUE) 
all_plot_data_5df <- data.frame()

for (f in files_5df) {
  load(f) 
  state_name <- gsub(".*smooth_(.*)_sub_5df.RData", "\\1", basename(f)) 
  
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
  B_5 <- bs(age_range, df = 5, intercept = TRUE)
  age_cols_names <- paste0("df_perc_area_secondary_prev_age_", age_range)
  age_matrix <- as.matrix(d1_full[, ..age_cols_names])
  
  W <- !is.na(age_matrix)
  actual_basis_sum_matrix <- W %*% B_5
  actual_basis_sum_matrix[actual_basis_sum_matrix == 0] <- 1
  actual_basis_sum <- colMeans(actual_basis_sum_matrix, na.rm = TRUE)
  
  beta_raw <- spline_outputs[[1]]
  as_sds   <- spline_outputs[[2]]
  sd_raw   <- spline_outputs[[4]]
  
  B_5_scaled_corrected <- t(t(as.matrix(B_5)) / (as_sds * actual_basis_sum)) 
  log_rr_raw <- B_5_scaled_corrected %*% beta_raw
  sd_smooth  <- pmax(smooth.spline(age_range, sd_raw, spar = 0.7)$y, 1e-8)
  log_rr_1SD <- log_rr_raw * sd_smooth
  
  df_temp <- data.frame(
    Age = age_range, Effect = exp(apply(log_rr_1SD, 1, median)),
    Lower = exp(apply(log_rr_1SD, 1, function(x) quantile(x, 0.025))),
    Upper = exp(apply(log_rr_1SD, 1, function(x) quantile(x, 0.975))),
    State = state_name
  )
  all_plot_data_5df <- rbind(all_plot_data_5df, df_temp)
}

p_5df <- ggplot(all_plot_data_5df %>% filter(State == "All"), aes(x = Age, y = Effect)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "blue", alpha = 0.2) +
  geom_line(color = "blue", size = 1.2) +
  theme_minimal() + 
  labs(title = "C) No Prop. Edge (B-spline, degree = 5)", y = "Relative Risk")


# ==============================================================================
# Population
# ==============================================================================
files_pop <- list.files(data_dir, pattern = "smooth_.*_sub_sensPopInteriorEdge.RData", full.names = TRUE)
all_plot_data_pop <- data.frame()

for (f in files_pop) {
  load(f) 
  state_name <- gsub(".*smooth_(.*)_sub_sensPopInteriorEdge.RData", "\\1", basename(f))
  
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
  log_rr_edge <- sweep(log_rr_base, MARGIN = 2, STATS = beta_matrix[5, ], FUN = "+")
  
  df_base <- data.frame(
    Age = age_range, Effect = exp(apply(log_rr_base, 1, median)),
    Lower = exp(apply(log_rr_base, 1, function(x) quantile(x, 0.025))),
    Upper = exp(apply(log_rr_base, 1, function(x) quantile(x, 0.975))),
    State = state_name, Type = "Interior (0% Edge)"
  )
  
  df_edge <- data.frame(
    Age = age_range, Effect = exp(apply(log_rr_edge, 1, median)),
    Lower = exp(apply(log_rr_edge, 1, function(x) quantile(x, 0.025))),
    Upper = exp(apply(log_rr_edge, 1, function(x) quantile(x, 0.975))),
    State = state_name, Type = "100% Edge"
  )
  
  all_plot_data_pop <- rbind(all_plot_data_pop, df_base, df_edge)
}

p_pop <- ggplot(all_plot_data_pop %>% filter(State == "All"), aes(x = Age, y = Effect, color = Type, fill = Type)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = 0.15, color = NA) +
  geom_line(size = 1.2) +
  scale_color_manual(values = c("Interior (0% Edge)" = "#00BFC4", "100% Edge" = "#C77CFF")) +
  scale_fill_manual(values = c("Interior (0% Edge)" = "#00BFC4", "100% Edge" = "#C77CFF")) +
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  labs(title = "D) Pop. offset (B-spline, degree = 4)", y = "Relative Risk")


# ==============================================================================
# Truncate
# ==============================================================================
files_trunc <- list.files(data_dir, pattern = "smooth_.*_sub_sensTruncInteriorEdge.RData", full.names = TRUE)
all_plot_data_trunc <- data.frame()

for (f in files_trunc) {
  load(f) 
  state_name <- gsub(".*smooth_(.*)_sub_sensTruncInteriorEdge.RData", "\\1", basename(f))
  
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
  
  age_range <- 3:17 
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
  log_rr_edge <- sweep(log_rr_base, MARGIN = 2, STATS = beta_matrix[5, ], FUN = "+")
  
  df_base <- data.frame(
    Age = age_range, Effect = exp(apply(log_rr_base, 1, median)),
    Lower = exp(apply(log_rr_base, 1, function(x) quantile(x, 0.025))),
    Upper = exp(apply(log_rr_base, 1, function(x) quantile(x, 0.975))),
    State = state_name, Type = "Interior (0% Edge)"
  )
  
  df_edge <- data.frame(
    Age = age_range, Effect = exp(apply(log_rr_edge, 1, median)),
    Lower = exp(apply(log_rr_edge, 1, function(x) quantile(x, 0.025))),
    Upper = exp(apply(log_rr_edge, 1, function(x) quantile(x, 0.975))),
    State = state_name, Type = "100% Edge"
  )
  
  all_plot_data_trunc <- rbind(all_plot_data_trunc, df_base, df_edge)
}

p_trunc <- ggplot(all_plot_data_trunc %>% filter(State == "All"), aes(x = Age, y = Effect, color = Type, fill = Type)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = 0.15, color = NA) +
  geom_line(size = 1.2) +
  scale_color_manual(values = c("Interior (0% Edge)" = "#00BFC4", "100% Edge" = "#C77CFF")) +
  scale_fill_manual(values = c("Interior (0% Edge)" = "#00BFC4", "100% Edge" = "#C77CFF")) +
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  labs(title = "E) Truncate (Age 3-17) (B-spline, degree = 4)", y = "Relative Risk")


# ==============================================================================
# Combined plot
# ==============================================================================

combined_plot <- p_alt / p_base / p_5df / p_pop #/ p_trunc

output_pdf <- "~/secVegDefo/code_output/plots_mod/all_sens.png"
dir.create(dirname(output_pdf), showWarnings = FALSE, recursive = TRUE)

ggsave(
  filename = output_pdf,
  plot = combined_plot,
  width = 8,
  height = 13, 
  dpi = 300
)

print(paste("Successfully saved all 5 stacked plots to:", output_pdf))
