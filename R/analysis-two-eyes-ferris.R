#-------------------------------------------------------------------------------
library(haven)
library(data.table)
library(checkmate)
library(forcats)
library(lme4)
library(this.path)  # path via rstudioapi not available if called via terminal
#-------------------------------------------------------------------------------
root_folder <- this.path::this.dir()
root_folder <- dirname(root_folder)
data_folder <- file.path(root_folder, "data")
r_folder <- file.path(root_folder, "R")
assets_folder <- file.path(root_folder, "assets")
path_data <- file.path(data_folder, "20230222_KORA_S4_FF4_FIT_StaBLab_with_riskfactors.sav")
path_dictionary <- file.path(data_folder, "vars_to_select")
source(file.path(r_folder, "functions.R"))
#-------------------------------------------------------------------------------
#                                 subset data
#-------------------------------------------------------------------------------
data_fit_ferris_long <-  wide_to_long(data_fit, study = "fit", score = "ferris" )
data_ff4_ferris_long <- wide_to_long(data_ff4, study = "ff4", score = "ferris")
data_fit_ferris_long <- data_fit_ferris_long[order(person_id)]
data_ff4_ferris_long <- data_ff4_ferris_long[order(person_id)]
#######
# FIT #
#######

# Case 1: Incidence early AMD (no early amd follow up)
#data_fit_ferris_long_1 <- subset_simplify_factor(data_fit_ferris_long,
#                                                score_bl_levels = "no_amd",
#                                                score_bl = "amd_status_bl",
#                                                score_fu = "amd_status_fu",
#                                                score_fu_levels = c("no_amd", "early_amd"))
#data_fit_ferris_long_1 <- clear_factors(data_fit_ferris_long_1)

# Case 2: Incidence late AMD among no AMD at BL (definition 1)
data_fit_ferris_long_2 <- subset_simplify_factor(data_fit_ferris_long,
                                                score_bl_levels = "no_amd",
                                                score_bl = "amd_status_bl",
                                                score_fu = "amd_status_fu",
                                                score_fu_levels = c("no_amd", "late_amd"))
data_fit_ferris_long_2 <- clear_factors(data_fit_ferris_long_2)

# Case 3: Incidence late AMD among no AMD at BL or early AMD at BL (definition 2)
## create dummy variable indicating if early or no amd at baseline
#data_fit_ferris_long_3 <- copy(data_fit_ferris_long)
#data_fit_ferris_long_3[, amd_bl := amd_status_bl]
#data_fit_ferris_long_3[, amd_bl := fct_drop(amd_bl)]
#data_fit_ferris_long_3 <- subset_simplify_factor(data_fit_ferris_long_3,
#                                                score_bl_levels = c("no_amd", "early_amd"),
#                                                score_fu = "amd_status_fu",
#                                                score_bl = "amd_status_bl",
#                                                score_fu_levels = c("no_amd", "early_amd", "late_amd"))
#data_fit_ferris_long_3 <- clear_factors(data_fit_ferris_long_3)

# Case 4: Progression from early AMD to late AMD
#data_fit_ferris_long_4 <- subset_simplify_factor(data_fit_ferris_long,
#                                                score_bl_levels = "early_amd",
#                                                score_fu = "amd_status_fu",
#                                                score_bl = "amd_status_bl",
#                                                score_fu_levels = c("early_amd", "late_amd"))
#data_fit_ferris_long_4 <- clear_factors(data_fit_ferris_long_4)

#######
# FF4 #
#######

# Case 1: Incidence early AMD
#data_ff4_ferris_long_1 <- subset_simplify_factor(data_ff4_ferris_long,
#                                                score_bl_levels = "no_amd",
#                                                score_bl = "amd_status_bl",
#                                                score_fu = "amd_status_fu",
#                                                score_fu_levels = c("no_amd", "early_amd"))
#data_ff4_ferris_long_1 <- clear_factors(data_ff4_ferris_long_1)

# Case 2: Incidence late AMD among no AMD at BL (definition 1)
data_ff4_ferris_long_2 <- subset_simplify_factor(data_ff4_ferris_long,
                                                score_bl_levels = c("no_amd"),
                                                score_bl = "amd_status_bl",
                                                score_fu = "amd_status_fu",
                                                score_fu_levels = c("no_amd", "late_amd"))
data_ff4_ferris_long_2 <- clear_factors(data_ff4_ferris_long_2)

# Case 3: Incidence late AMD among no AMD at BL or early AMD at BL (definition 2)
## create dummy variable indicating if early or no amd at baseline
#data_ff4_ferris_long_3 <- copy(data_ff4_ferris_long)
#data_ff4_ferris_long_3[, amd_bl := amd_status_bl]
#data_ff4_ferris_long_3[, amd_bl := fct_drop(amd_bl)]
#data_ff4_ferris_long_3 <- subset_simplify_factor(data_ff4_ferris_long_3,
#                                                score_bl_levels = c("no_amd", "early_amd"),
#                                                score_fu = "amd_status_fu",
#                                                score_bl = "amd_status_bl",
#                                                score_fu_levels = c("no_amd", "early_amd","late_amd"))
#data_ff4_ferris_long_3 <- clear_factors(data_ff4_ferris_long_3)

# Case 4: Progression from early AMD to late AMD
#data_ff4_ferris_long_4 <- subset_simplify_factor(data_ff4_ferris_long,
#                                                score_bl_levels = "early_amd",
#                                                score_fu = "amd_status_fu",
#                                                score_bl = "amd_status_bl",
#                                                score_fu_levels = c("early_amd", "late_amd"))
#data_ff4_ferris_long_4 <- clear_factors(data_ff4_ferris_long_4)
#-------------------------------------------------------------------------------
#                                 Marginal Model
#-------------------------------------------------------------------------------
library(gee)
#######
# FIT #
#######

## Case 1
#model_gee_fit_ferris_1 <- gee(formula = ifelse(amd_status_fu == "early_amd", 0, 1) ~ ltalteru + lcsex + time_bl_fu +ltcigreg_sf + ll_hdla + time_bl_fu,
#                             id = person_id,
#                             family = "binomial",
#                             corstr = "exchangeable",
#                             data = data_fit_ferris_long_1)
#
#model_gee_fit_ferris_1_grs <- gee(formula = ifelse(amd_status_fu == "early_amd", 0, 1) ~ ltalteru + lcsex + time_bl_fu + ltcigreg_sf + ll_hdla + time_bl_fu + GrsWeightedByBeta,
#                                 id = person_id,
#                                 family = "binomial",
#                                 corstr = "exchangeable",
#                                 data = data_fit_ferris_long_1[!is.na(GrsWeightedByBeta)])

## Case 2 (yielding warnings)
tryCatch(
  {
    model_gee_fit_ferris_2 <- gee(formula = ifelse(amd_status_fu == "late_amd", 0, 1) ~ ltalteru + lcsex + time_bl_fu + ltcigreg_sf + ll_hdla + time_bl_fu,
                                 id = person_id,
                                 family = "binomial",
                                 corstr = "exchangeable",
                                 data = data_fit_ferris_long_2)
    
    model_gee_fit_ferris_2_grs <- gee(formula = ifelse(amd_status_fu == "late_amd", 0, 1) ~ ltalteru + lcsex + time_bl_fu + ltcigreg_sf + ll_hdla + time_bl_fu + GrsWeightedByBeta,
                                     id = person_id,
                                     family = "binomial",
                                     corstr = "exchangeable",
                                     data = data_fit_ferris_long_2[!is.na(GrsWeightedByBeta)])
  },
  error = function(e){
    message("model_gee_fit_ferris_2 DID NOT CONVERGE.")
  }
)
## Case 3 (yielding warnings)
tryCatch(
  {
    model_gee_fit_ferris_3 <- gee(formula = ifelse(amd_status_fu == "late_amd", 0, 1) ~ ltalteru + lcsex + ltcigreg_sf + ll_hdla + time_bl_fu + amd_bl,
                                 id = person_id,
                                 family = "binomial",
                                 corstr = "exchangeable",
                                 data = data_fit_ferris_long_3)
    
    model_gee_fit_ferris_3_grs <- gee(formula = ifelse(amd_status_fu == "late_amd", 0, 1) ~ ltalteru + lcsex + ltcigreg_sf + ll_hdla + time_bl_fu + amd_bl + GrsWeightedByBeta,
                                     id = person_id,
                                     family = "binomial",
                                     corstr = "exchangeable",
                                     data = data_fit_ferris_long_3[!is.na(GrsWeightedByBeta)])
    
  },
  error = function(e){
    message("model_gee_fit_ferris_3 DID NOT CONVERGE.")
  }
)
## Case 4 (yielding warnings)
tryCatch(
  {
    model_gee_fit_ferris_4 <- geeglm(formula = ifelse(amd_status_fu == "late_amd", 0, 1) ~ ltalteru + lcsex + ltcigreg_sf + ll_hdla + time_bl_fu,
                                    id = person_id,
                                    family = "binomial",
                                    corstr = "exchangeable",
                                    data = data_fit_ferris_long_4)
    
    model_gee_fit_ferris_4_grs <- geeglm(formula = ifelse(amd_status_fu == "late_amd", 0, 1) ~ ltalteru + lcsex + ltcigreg_sf + ll_hdla + time_bl_fu,
                                        id = person_id,
                                        family = "binomial",
                                        corstr = "exchangeable",
                                        data = data_fit_ferris_long_4[!is.na(GrsWeightedByBeta)])
  },
  error = function(e){
    message("model_gee_fit_ferris_4 DID NOT CONVERGE.")
  }
)
#######
# FF4 #
#######

## Case 1
tryCatch(
  {
    model_gee_ff4_ferris_1 <- gee(formula = ifelse(amd_status_fu == "early_amd", 0, 1) ~ ltalteru + lcsex + time_bl_fu + ltcigreg_sf + ll_hdla + time_bl_fu,
                                  id = person_id,
                                  family = "binomial",
                                  corstr = "exchangeable",
                                  data = data_ff4_ferris_long_1[!is.na(ll_hdla)])
    model_gee_ff4_ferris_1_grs <- gee(formula = ifelse(amd_status_fu == "early_amd", 0, 1) ~ ltalteru + lcsex + time_bl_fu + ltcigreg_sf + ll_hdla +time_bl_fu + GrsWeightedByBeta,
                                      id = person_id,
                                      family = "binomial",
                                      corstr = "exchangeable",
                                      data = data_ff4_ferris_long_1[!is.na(ll_hdla) & !is.na(GrsWeightedByBeta)])
    
  },
  error = function(e)
    message(paste0("model_gee_ff4_ferris_1", " DID NOT CONVERGE"))
)
## Case 2 
tryCatch(
  {
    model_gee_ff4_ferris_2 <- gee(formula = ifelse(amd_status_fu == "early_amd", 0, 1) ~ ltalteru + lcsex + ltcigreg_sf + ll_hdla + time_bl_fu,
                                 id = person_id,
                                 family = "binomial",
                                 corstr = "exchangeable",
                                 data = data_ff4_ferris_long_2[!is.na(ll_hdla)])
    
    model_gee_ff4_ferris_2_grs <- gee(formula = ifelse(amd_status_fu == "early_amd", 0, 1) ~ ltalteru + lcsex + ltcigreg_sf + ll_hdla + time_bl_fu + GrsWeightedByBeta,
                                     id = person_id,
                                     family = "binomial",
                                     corstr = "exchangeable",
                                     data = data_ff4_ferris_long_2[!is.na(ll_hdla) & !is.na(GrsWeightedByBeta)])
  },
  error = function(e) {
    message("model_gee_ff4_ferris_2 DID NOT CONVERGE.")
  }
)
## Case 3 
tryCatch(
  {
    model_gee_ff4_ferris_3 <- gee(formula = ifelse(amd_status_fu == "early_amd", 0, 1) ~ ltalteru + lcsex + time_bl_fu + ltcigreg_sf + ll_hdla + time_bl_fu + amd_bl,
                                 id = person_id,
                                 family = "binomial",
                                 corstr = "exchangeable",
                                 data = data_ff4_ferris_long_3[!is.na(ll_hdla)])
    
    model_gee_ff4_ferris_3_grs <- gee(formula = ifelse(amd_status_fu == "no_amd", 0, 1) ~ ltalteru + lcsex + ltcigreg_sf + ll_hdla + time_bl_fu + GrsWeightedByBeta,
                                     id = person_id,
                                     family = "binomial",
                                     corstr = "exchangeable",
                                     data = data_ff4_ferris_long_3[!is.na(ll_hdla) & !is.na(GrsWeightedByBeta)])  
    
    
  },
  error = function(e) {
    message("model_gee_ff4_ferris_3 DID NOT CONVERGE.")
  }
)
## Case 4 
tryCatch(
  {
    model_gee_ff4_ferris_4 <- gee(formula = ifelse(amd_status_fu == "no_amd", 0, 1) ~ ltalteru + lcsex + ltcigreg_sf + ll_hdla + time_bl_fu,
                                 id = person_id,
                                 family = "binomial",
                                 corstr = "independence",
                                 data = data_ff4_ferris_long_4)
    
    model_gee_ff4_ferris_4_grs <- gee(formula = ifelse(amd_status_fu == "no_amd", 0, 1) ~ ltalteru + lcsex + ltcigreg_sf + ll_hdla + time_bl_fu +  GrsWeightedByBeta,
                                     id = person_id,
                                     family = "binomial",
                                     corstr = "independence",
                                     data = data_ff4_ferris_long_4[!is.na(GrsWeightedByBeta)])
  },
  error = function(e) {
    message("model_gee_ff4_ferris_4 DID NOT CONVERGE.")
  }
)
