#-------------------------------------------------------------------------------
library(haven)
library(data.table)
library(checkmate)
library(forcats)
library(this.path)  # path via rstudioapi not available if called via terminal
library(lme4)
#-------------------------------------------------------------------------------
root_folder <- this.path::this.dir()
root_folder <- dirname(root_folder)
data_folder <- file.path(root_folder, "data")
r_folder <- file.path(root_folder, "R")
assets_folder <- file.path(root_folder, "assets")
path_data <- file.path(data_folder, "20230222_KORA_S4_FF4_FIT_StaBLab_with_riskfactors.sav")
path_dictionary <- file.path(data_folder, "vars_to_select")
source(file.path(r_folder, "functions.R"))
source(file.path(r_folder, "preprocess.R"))
#-------------------------------------------------------------------------------
#                                 subset data
#-------------------------------------------------------------------------------
data_fit_long <-  wide_to_long(data_fit, study = "fit", score = "continental")
data_ff4_long <- wide_to_long(data_ff4, study = "ff4", score = "continental")
data_fit_long <- data_fit_long[order(person_id)]
data_ff4_long <- data_ff4_long[order(person_id)]
#------------------------------------ FIT --------------------------------------
# Case 1: Incidence early AMD
data_fit_long_1 <- subset_simplify_factor(data_fit_long,
                                     score_bl_levels = "no_amd",
                                     score_bl = "amd_status_bl",
                                     score_fu = "amd_status_fu",
                                     score_fu_levels = c("no_amd", "early_amd"))
data_fit_long_1 <- clear_factors(data_fit_long_1)

# Case 2: Incidence late AMD among no AMD at BL (definition 1)
data_fit_long_2 <- subset_simplify_factor(data_fit_long,
                                     score_bl_levels = c("no_amd"),
                                     score_bl = "amd_status_bl",
                                     score_fu = "amd_status_fu",
                                     score_fu_levels = c("no_amd", "late_amd"))
data_fit_long_2 <- clear_factors(data_fit_long_2)

# Case 3: Incidence late AMD among no AMD at BL or early AMD at BL (definition 2)
## create dummy variable indicating if early or no amd at baseline
data_fit_long_3 <- copy(data_fit_long)
data_fit_long_3[, amd_bl := amd_status_bl]
data_fit_long_3[, amd_bl := fct_drop(amd_bl)]
data_fit_long_3 <- subset_simplify_factor(data_fit_long_3,
                                     score_bl_levels = c(no_amdD2 = "no_amd", no_amdD2 = "early_amd"),
                                     score_fu = "amd_status_fu",
                                     score_bl = "amd_status_bl",
                                     score_fu_levels = c(no_amdD2 = "no_amd", no_amdD2 = "early_amd", late_amd = "late_amd"))
data_fit_long_3 <- clear_factors(data_fit_long_3)

# Case 4: Progression from early AMD to late AMD
data_fit_long_4 <- subset_simplify_factor(data_fit_long,
                                     score_bl_levels = "early_amd",
                                     score_fu = "amd_status_fu",
                                     score_bl = "amd_status_bl",
                                     score_fu_levels = c("early_amd", "late_amd"))
data_fit_long_4 <- clear_factors(data_fit_long_4)
#------------------------------------ FF4 --------------------------------------
# Case 1: Incidence early AMD
data_ff4_long_1 <- subset_simplify_factor(data_fit_long,
                                          score_bl_levels = "no_amd",
                                          score_bl = "amd_status_bl",
                                          score_fu = "amd_status_fu",
                                          score_fu_levels = c("no_amd", "early_amd"))
data_ff4_long_1 <- clear_factors(data_ff4_long_1)

# Case 2: Incidence late AMD among no AMD at BL (definition 1)
data_ff4_long_2 <- subset_simplify_factor(data_fit_long,
                                          score_bl_levels = c("no_amd"),
                                          score_bl = "amd_status_bl",
                                          score_fu = "amd_status_fu",
                                          score_fu_levels = c("no_amd", "late_amd"))
data_ff4_long_2 <- clear_factors(data_ff4_long_2)

# Case 3: Incidence late AMD among no AMD at BL or early AMD at BL (definition 2)
## create dummy variable indicating if early or no amd at baseline
data_ff4_long_3 <- copy(data_ff4_long)
data_ff4_long_3[, amd_bl := amd_status_bl]
data_ff4_long_3[, amd_bl := fct_drop(amd_bl)]
data_ff4_long_3 <- subset_simplify_factor(data_ff4_long_3,
                                          score_bl_levels = c(no_amdD2 = "no_amd", no_amdD2 = "early_amd"),
                                          score_fu = "amd_status_fu",
                                          score_bl = "amd_status_bl",
                                          score_fu_levels = c(no_amdD2 = "no_amd", no_amdD2 = "early_amd", late_amd = "late_amd"))
data_ff4_long_3 <- clear_factors(data_ff4_long_3)

# Case 4: Progression from early AMD to late AMD
data_ff4_long_4 <- subset_simplify_factor(data_ff4_long,
                                          score_bl_levels = "early_amd",
                                          score_fu = "amd_status_fu",
                                          score_bl = "amd_status_bl",
                                          score_fu_levels = c("early_amd", "late_amd"))
data_ff4_long_4 <- clear_factors(data_ff4_long_4)
#-------------------------------------------------------------------------------
#                                 Random Effects Model
#-------------------------------------------------------------------------------
#######
# FIT #
#######

# checkConv warning if included time_bl_fu
## Case 1
model_glmer_fit_1 <- glmer(formula = amd_status_fu ~ ltalteru + lcsex + ltrauchp + ll_hdla  + (1|person_id),
                        family = binomial(link = "logit"),
                        data = data_fit_long_1)
## Case 2
model_glmer_fit_2 <- glmer(formula = amd_status_fu ~ ltalteru + lcsex + ltrauchp + ll_hdla  + (1|person_id),
                           family = binomial(link = "logit"),
                           data = data_fit_long_2)
## Case 3
model_glmer_fit_3 <- glmer(formula = amd_status_fu ~ ltalteru + lcsex + ltrauchp + ll_hdla  + (1|person_id),
                           family = binomial(link = "logit"),
                           data = data_fit_long_3)
## Case 4
model_glmer_fit_4 <- glmer(formula = amd_status_fu ~ ltalteru + lcsex + ltrauchp + ll_hdla  + (1|person_id),
                           family = binomial(link = "logit"),
                           data = data_fit_long_4)
#######
# FF4 #
#######

## Case 1
model_glmer_ff4_1 <- glmer(formula = amd_status_fu ~ ltalteru + lcsex + ltrauchp + ll_hdla  + (1|person_id),
                           family = binomial(link = "logit"),
                           data = data_ff4_long_1)
## Case2
model_glmer_ff4_2 <- glmer(formula = amd_status_fu ~ ltalteru + lcsex + ltrauchp + ll_hdla  + (1|person_id),
                           family = binomial(link = "logit"),
                           data = data_ff4_long_2)
## Case3
model_glmer_ff4_3 <- glmer(formula = amd_status_fu ~ ltalteru + lcsex + ltrauchp + ll_hdla  + (1|person_id),
                           family = binomial(link = "logit"),
                           data = data_ff4_long_3)
## Case4
model_glmer_ff4_4 <- glmer(formula = amd_status_fu ~ ltalteru + lcsex + ltrauchp + ll_hdla  + (1|person_id),
                           family = binomial(link = "logit"),
                           data = data_ff4_long_4)
#-------------------------------------------------------------------------------
#                                 Marginal Model
#-------------------------------------------------------------------------------
library(gee)
library(geepack)
library(multgee)
#######
# FIT #
#######

## Case 1
model_gee_fit_1 <- gee(formula = ifelse(amd_status_fu == "no_amd", 0, 1) ~ ltalteru + lcsex + ltcigreg_sf + ll_hdla + time_bl_fu,
    id = person_id,
    family = "binomial",
    corstr = "exchangeable",
    data = data_fit_long_1)


## Case 2
model_gee_fit_2 <- gee(formula = ifelse(amd_status_fu == "no_amd", 0, 1) ~ ltalteru + lcsex + ltcigreg_sf + ll_hdla + time_bl_fu,
       id = person_id,
       family = "binomial",
       corstr = "exchangeable",
       data = data_fit_long_2)

## Case 3
model_gee_fit_3 <- gee(formula = ifelse(amd_status_fu == "no_amd", 0, 1) ~ ltalteru + lcsex + ltcigreg_sf + ll_hdla + time_bl_fu + amd_bl,
                          id = person_id,
                          family = "binomial",
                         corstr = "exchangeable",
                          data = data_fit_long_3)

## Case 4
model_gee_fit_4 <- geeglm(formula = ifelse(amd_status_fu == "no_amd", 0, 1) ~ ltalteru + lcsex + ltcigreg_sf + ll_hdla + time_bl_fu,
       id = person_id,
       family = "binomial",
       corstr = "exchangeable",
       data = data_fit_long_4)

#######
# FF4 #
#######

## Case 1
model_gee_ff4_1 <- geeglm(formula = ifelse(amd_status_fu == "no_amd", 0, 1) ~ ltalteru + lcsex + ltrauchp + ll_hdla + time_bl_fu,
                          id = person_id,
                          family = "binomial",
                          corstr = "exchangeable",
                          data = data_ff4_long_1)
## Case 2
model_gee_ff4_2 <- geeglm(formula = ifelse(amd_status_fu == "no_amd", 0, 1) ~ ltalteru + lcsex + ltrauchp + ll_hdla + time_bl_fu,
                          id = person_id,
                          family = "binomial",
                          corstr = "exchangeable",
                          data = data_ff4_long_2)

## Case 3
#model_gee_ff4_3 <- geeglm(formula = ifelse(amd_status_fu == "no_amd", 0, 1) ~ ltalteru + lcsex + ltrauchp + ll_hdla + time_bl_fu,
#                             id = person_id,
#                             family = "binomial",
#                             corstr = "exchangeable",
#                             data = data_ff4_long_3)

## Case 4
model_gee_ff4_4 <- geeglm(formula = ifelse(amd_status_fu == "no_amd", 0, 1) ~ ltalteru + lcsex + ltrauchp + ll_hdla + time_bl_fu,
                             id = person_id,
                             family = "binomial",
                             corstr = "independence",
                             data = data_ff4_long_4)
