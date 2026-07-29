library(ggplot2)
library(dplyr)
library(splines)
library(data.table)
library(sf)

# Load data
data_dir <- "~/Desktop/secDef/mon_results/age_prof/"
files <- list.files(data_dir, pattern = "smooth_.*_sub_InteriorEdge.RData", full.names = TRUE)

deforest_wide <- fread(file="~/Desktop/secDef/deforest_wide_InteriorEdge.csv")
#deforest_wide <- fread(file="~/secVegDefo/data/deforest_wide_InteriorEdge.csv")
all_muni <- st_read("~/secVegDefo/data/muni_mun_exp/muni_mun_exp.shp")

# Compile data
all_plot_data <- data.frame()

for (f in files) {
  load(f) 
  state_name <- gsub(".*smooth_(.*)_sub_InteriorEdge.RData", "\\1", basename(f))
  
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
    Age = age_range,
    Effect = exp(apply(log_rr_base, 1, median)),
    Lower  = exp(apply(log_rr_base, 1, function(x) quantile(x, 0.025))),
    Upper  = exp(apply(log_rr_base, 1, function(x) quantile(x, 0.975))),
    State  = state_name,
    Type   = "Interior (0% Edge)"
  )
  
  df_edge <- data.frame(
    Age = age_range,
    Effect = exp(apply(log_rr_edge, 1, median)),
    Lower  = exp(apply(log_rr_edge, 1, function(x) quantile(x, 0.025))),
    Upper  = exp(apply(log_rr_edge, 1, function(x) quantile(x, 0.975))),
    State  = state_name,
    Type   = "100% Edge"
  )
  
  all_plot_data <- rbind(all_plot_data, df_base, df_edge)
}

p1 <- ggplot(all_plot_data %>% filter(State == "All"), aes(x = Age, y = Effect, color = Type, fill = Type)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = 0.15, color = NA) +
  geom_line(size = 1.2) +
  scale_color_manual(values = c("Interior (0% Edge)" = "#00BFC4", "100% Edge" = "#C77CFF")) +
  scale_fill_manual(values = c("Interior (0% Edge)" = "#00BFC4", "100% Edge" = "#C77CFF")) +
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  labs(title = "Sec. Defo.: Interior vs. Edge Deforestation", y = "Relative Risk")

all_res_paper <- all_plot_data %>% filter(State == "All")

file_path <- "~/Desktop/secDef/mon_results/"
files <- list.files(path = file_path, pattern = "_monNew.csv", full.names = TRUE)
all_results <- files %>%
  map_df(~{
    read_csv(.x) %>%
      rename(parameter = 1) %>%
      mutate(state = str_remove(basename(.x), "_monNew.csv"))
  })

other_states <- sort(unique(all_results$state[all_results$state != "All"]))
state_levels <- c(rev(other_states), "All") 

plot_data <- all_results %>%
  filter(parameter %in% c("scale(df_perc_area_primary_prev)", 
                          "scale(df_perc_area_secondary_prev)")) %>%
  mutate(
    state = factor(state, levels = state_levels),
    parameter = case_when(
      parameter == "scale(df_perc_area_primary_prev)" ~ "Primary deforestation",
      parameter == "scale(df_perc_area_secondary_prev)" ~ "Secondary deforestation"
    ),
    parameter = factor(parameter, levels = c("Primary deforestation", "Secondary deforestation"))
  )

forest <- ggplot(plot_data, aes(y = state, x = mean)) +
  geom_point(color = "steelblue", size = 2) +
  geom_errorbarh(aes(xmin = `0.025quant`, xmax = `0.975quant`), 
                 height = 0.2, color = "gray40") +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
  facet_wrap(~parameter, scales = "free_x") +
  labs(title = "Posterior Estimates across States",
       x = "Posterior Mean (95% Credible Interval)",
       y = "State") +
  theme_minimal() +
  theme(panel.spacing = unit(2, "lines"))
combined_plot <- forest / p1
combined_plot <- combined_plot + 
  plot_annotation(tag_levels = 'A') + 
  plot_layout(heights = c(1.2, 1))
combined_plot
ggsave(
  filename = "~/secVegDefo/code_output/plots_mod/Fig2.pdf", 
  plot = combined_plot,
  width = 5,
  height = 6,
  dpi = 300
)
all_res_paper <- all_plot_data %>% filter(State == "All")
1-c(0.922122, 0.93958, 0.9573)
1 - c(0.9846, 0.9756, 0.9975)

1 - c(1.0166382, 0.9947599, 1.0377197)

# Stratified -------------------------------------------------------------------
p2 <- ggplot(all_plot_data %>% filter(State != "All"), aes(x = Age, y = Effect, color = Type, fill = Type)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = 0.15, color = NA) +
  geom_line(size = 1.0) +
  scale_color_manual(values = c("Interior (0% Edge)" = "#00BFC4", "100% Edge" = "#C77CFF")) +
  scale_fill_manual(values = c("Interior (0% Edge)" = "#00BFC4", "100% Edge" = "#C77CFF")) +
  facet_wrap(~State, ncol = 3, nrow = 3, scales = "free_y") +
  theme_minimal() + 
  theme(aspect.ratio = 0.5, panel.spacing = unit(1.5, "lines"), plot.title = element_text(size = 16, face = "bold"), legend.position = "bottom", legend.title = element_blank()) + 
  labs(title = "Relative Risk by State: Interior vs. Edge Deforestation", y = "Relative Risk")

output_pdf <- "~/secVegDefo/code_output/plots_mod/IntEdge.png"
dir.create(dirname(output_pdf), showWarnings = FALSE, recursive = TRUE)
pdf(output_pdf, width = 7, height = 7)
print(p2)
dev.off()
print(paste("PDF saved to", output_pdf))

