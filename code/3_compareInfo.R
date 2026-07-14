library(data.table)

file_4df <- "~/Desktop/secDef/mon_results/All_age_smooth_sub.RData"
file_5df <- "~/Desktop/secDef/mon_results/All_age_smooth_sub_5df.RData"
file_IE <- "~/Desktop/secDef/mon_results/All_age_smooth_sub_InteriorEdge.RData"
file_IEalt <- "~/Desktop/secDef/mon_results/All_age_smooth_sub_altInteriorEdge.RData"

extract_metrics <- function(filepath, model_name) {
  # Check if file exists to prevent crashing
  if (!file.exists(filepath)) {
    return(data.frame(Model = model_name, DIC = NA, WAIC = NA))
  }
  
  temp_env <- new.env()
  load(filepath, envir = temp_env)
  
  inla_obj <- temp_env$m21_sub_results_sub
  
  dic_val  <- inla_obj$dic$dic
  waic_val <- inla_obj$waic$waic
  
  return(data.frame(Model = model_name, DIC = dic_val, WAIC = waic_val))
}

cat("Extracting metrics from 4 DoF model...\n")
res_4df <- extract_metrics(file_4df, "4 DoF (Original Floating)")

cat("Extracting metrics from 5 DoF model...\n")
res_5df <- extract_metrics(file_5df, "5 DoF (Extra Flexibility)")

cat("Extracting metrics from IE model...\n")
res_IE <- extract_metrics(file_IE, "IE")

cat("Extracting metrics from alt IE model...\n")
res_IEalt <- extract_metrics(file_IEalt, "IE alt")

comparison_table <- rbind(res_4df, res_5df, res_IE, res_IEalt)

# Lower is better, so we subtract the minimum score from all scores
best_dic <- min(comparison_table$DIC, na.rm = TRUE)
best_waic <- min(comparison_table$WAIC, na.rm = TRUE)

comparison_table$Delta_DIC  <- comparison_table$DIC - best_dic
comparison_table$Delta_WAIC <- comparison_table$WAIC - best_waic

comparison_table$DIC <- round(comparison_table$DIC, 1)
comparison_table$WAIC <- round(comparison_table$WAIC, 1)
comparison_table$Delta_DIC <- round(comparison_table$Delta_DIC, 1)
comparison_table$Delta_WAIC <- round(comparison_table$Delta_WAIC, 1)

cat("\n======================================================\n")
cat("                MODEL COMPARISON RESULTS                \n")
cat("======================================================\n")
print(comparison_table, row.names = FALSE)
cat("======================================================\n")

out_file <- "~/Desktop/secDef/mon_results/model_comparison_metrics.csv"
fwrite(comparison_table, file = out_file)
cat(paste("Saved summary table to:", out_file, "\n"))
