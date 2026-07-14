# Code to characterize effects of deforestation of secondary vegetation in the Brazilian Amazon
- Accompanies Sheen, Arisco, De Nicola, Castro (2026) "The effects of deforestation of secondary vegetation on malaria risk in the Brazilian Amazon."
- Codebase below was used to generate model results and plots. User must create directory mon_results and mon_results/age_prof to store model results, which are in .RData form, before running model scripts. These .RData objects are then used for plotting scripts.
## code
- 1_inlaSmooth_InteriorEdge_Penalized.R: model results with RW2 penalization on B-spline
- 1_inlaSmooth_InteriorEdge.R: main model results
- 1_inlaSmooth_NoPropEdge.R: sensitivity analysis with no PropEdge
- 1_inlaSmooth_sens5.R: sensitivity analysis with 5 degrees of freedom
- 2_plot...R: plotting scripts of above model results
- 3_compareInfo.R: comparison of WAIC and DIC information of above models
## code_output
- model_comparison.csv: .csv of model comparison of information
- plots_mod: plotting results of above models
## data
- deforest_wide_InteriorEdge.csv: data used for each of models
- muni_mun_exp: shapefile of Brazilian municipalities

These programs are a work in progress, as we work to improve usability, error-catching, and speed of analysis. If you find errors, please contact Justin Sheen at jsheen (at) hsph (dot) harvard (dot) edu.

Last Update: July 14, 2026
