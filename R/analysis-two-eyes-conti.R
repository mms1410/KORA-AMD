#-------------------------------------------------------------------------------
rm(list = ls())
library(haven)
library(data.table)
library(checkmate)
library(forcats)
library(this.path)  # path via rstudioapi not available if called via terminal
#library(nlme)
#-------------------------------------------------------------------------------
root_folder <- this.path::this.dir()
root_folder <- dirname(root_folder)
data_folder <- file.path(root_folder, "data")
r_folder <- file.path(root_folder, "R")
assets_folder <- file.path(root_folder, "assets")
path_data <- file.path(data_folder, "20230222_KORA_S4_FF4_FIT_StaBLab_with_riskfactors.sav")
path_dictionary <- file.path(data_folder, "vars_to_select")
source(file.path(r_folder, "preprocess.R"))
rm(list = c(
  "age_groups_ff4",
  "age_groups_fit",
  "data_dictionary",
  "to_group",
  "def_amd_ferris",
  "def_amd_continental",
  "data"
))
#-------------------------------------------------------------------------------
#                                 subset data
#-------------------------------------------------------------------------------
data_fit_long <-  wide_to_long(data_fit, study = "fit", score = "continental")
data_ff4_long <- wide_to_long(data_ff4, study = "ff4", score = "continental")
rm(list = c(
  "data_fit",
  "data_ff4"
))

# Case 1: Incidence early AMD
data_fit_1 <- subset_simplify_factor(data_fit_long,
                                     score_bl_levels = "no_amd",
                                     score_bl = "amd_status_bl",
                                     score_fu = "amd_status_fu",
                                     score_fu_levels = c("no_amd", "early_amd"))

# Case 2: Incidence late AMD among no AMD at BL (definition 1)
data_fit_2 <- subset_simplify_factor(data_fit_long,
                                     score_bl_levels = "no_amd",
                                     score_bl = "amd_status_bl",
                                     score_fu = "amd_status_fu",
                                     score_fu_levels = c("no_amd", "late_amd"))

# Case 3: Incidence late AMD among no AMD at BL or early AMD at BL (definition 2)
## create dummy variable indicating if early or no amd at baseline
data_fit_3 <- copy(data_fit_long)
data_fit_3[, amd_bl := amd_status_bl]
data_fit_3[, amd_bl := fct_drop(amd_bl)]
data_fit_3 <- subset_simplify_factor(data_fit_3,
                                     score_bl_levels = c(no_amdD2 = "no_amd", no_amdD2 = "early_amd"),
                                     score_fu = "amd_status_fu",
                                     score_bl = "amd_status_bl",
                                     score_fu_levels = c(no_amdD2 = "no_amd", no_amdD2 = "early_amd", late_amd = "late_amd"))

# Case 4: Progression from early AMD to late AMD
data_fit_4 <- subset_simplify_factor(data_fit_long,
                                     score_bl_levels = "early_amd",
                                     score_fu = "amd_status_fu",
                                     score_bl = "amd_status_bl",
                                     score_fu_levels = c("early_amd", "late_amd"))
#-------------------------------------------------------------------------------
#                                 Random Effects Model
#-------------------------------------------------------------------------------
library(lme4)
# checkConv warning if included time_bl_fu
model_glmer_fit_1 <- glmer(formula = amd_status_fu ~ ltalteru + lcsex + ltrauchp + ll_hdla  + (1|person_id),
                        family = binomial(link = "logit"),
                        data = data_fit_1)
summary(model_glmer_fit_1)
#-------------------------------------------------------------------------------
#                                 Marginal Model
#-------------------------------------------------------------------------------
library(gee)
data_fit_1$amd_status_fu <- ifelse(data_fit_1$amd_status_fu == "no_amd", 0, 1)

model_gee_fit_1 <- gee(formula = amd_status_fu ~ ltalteru + lcsex + ltrauchp + ll_hdla + time_bl_fu,
    id = person_id,
    family = "binomial",
    corstr = "exchangeable",
    data = data_fit_1)
summary(model_gee_fit_1)
