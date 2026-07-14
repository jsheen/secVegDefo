# Libraries
set.seed(324)
library(data.table)
library(sf)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(splines)

# Load these once so we don't repeatedly read them into memory
deforest_wide <- fread(file="~/Desktop/secDef/deforest_wide_InteriorEdge.csv")
all_muni <- st_read("~/Desktop/secDef/muni_mun_exp/muni_mun_exp.shp")

data_dir <- "~/Desktop/secDef/mon_results/age_prof/"
files <- list.files(data_dir, pattern = "smooth_.*_sub.RData", full.names = TRUE)

all_plot_data <- data.frame()

for (f in files) {
  load(f) # Loads 'spline_outputs'
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
    Age = age_range,
    Effect = exp(apply(log_rr_1SD, 1, median)),
    Lower  = exp(apply(log_rr_1SD, 1, function(x) quantile(x, 0.025))),
    Upper  = exp(apply(log_rr_1SD, 1, function(x) quantile(x, 0.975))),
    State  = state_abrev
  )
  all_plot_data <- rbind(all_plot_data, df_temp)
}

p1 <- ggplot(all_plot_data %>% filter(State == "All"), aes(x = Age, y = Effect)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "blue", alpha = 0.2) +
  geom_line(color = "blue", size = 1.2) +
  theme_minimal() + 
  labs(title = "Relative Risk (All States)", y = "Relative Risk")

p2_data <- all_plot_data %>% filter(State != "All")

p2 <- ggplot(p2_data, aes(x = Age, y = Effect)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "blue", alpha = 0.2) +
  geom_line(color = "blue", size = 1.2) +
  facet_wrap(~State, ncol = 3, scales = "free_y") +
  theme_minimal() + 
  theme(
    aspect.ratio = 0.5, 
    panel.spacing = unit(2, "lines")
  ) + 
  labs(title = "Relative Risk by State",
       y = "Relative Risk")

pdf("~/secVegDefo/NoPropEdge.pdf", width = 16, height = 9)
print(p1 + theme(aspect.ratio = 0.5)) 
print(p2)
dev.off()

print("PDF saved to ~/secVegDefo/NoPropEdge.pdf")