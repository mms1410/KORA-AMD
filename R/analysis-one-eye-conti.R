#-------------------------------------------------------------------------------
library(haven)
library(data.table)
library(checkmate)
library(forcats)
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
#######
# FIT #
#######

# Case 1: Incidence early AMD
## note: here n risk set is 481 and not 484 as in original paper
## 3 participants had late amd at FU
data_fit_1.1 <- subset_simplify_factor(data_fit,
                                     score_bl_levels = "no_amd",
                                     score_fu = "PT_conti_worst_eye",
                                     score_fu_levels = c("no_amd", "early_amd"))
## as in original paper
data_fit_1 <- subset_simplify_factor(data_fit,
                                       score_bl_levels = "no_amd",
                                       score_fu = "PT_conti_worst_eye",
                                       score_fu_levels = c(no_amd = "no_amd", early_amd = "early_amd", early_amd = "late_amd"))

# Case 2: Incidence late AMD among no AMD at BL (definition 1)
data_fit_2.1 <- subset_simplify_factor(data_fit,
                                     score_bl_levels = "no_amd",
                                     score_fu = "PT_conti_worst_eye",
                                     score_fu_levels = c("no_amd", "late_amd"))

data_fit_2 <- subset_simplify_factor(data_fit,
                                      score_bl_levels = "no_amd",
                                      score_fu = "PT_conti_worst_eye",
                                      score_fu_levels = c(no_amd = "no_amd", late_amd = "late_amd", late_amd = "early_amd"))

# Case 3: Incidence late AMD among no AMD at BL or early AMD at BL (definition 2)
## create dummy variable indicating if early or no amd at baseline
data_fit_3 <- copy(data_fit)
data_fit_3[, amd_bl := LT_conti_worst_eye]
data_fit_3[, amd_bl := fct_drop(amd_bl)]
data_fit_3 <- subset_simplify_factor(data_fit_3,
                                     score_bl_levels = c(no_amdD2 = "no_amd", no_amdD2 = "early_amd"),
                                     score_fu = "PT_conti_worst_eye",
                                     score_fu_levels = c(no_amdD2 = "no_amd", no_amdD2 = "early_amd", late_amd = "late_amd"))

# Case 4: Progression from early AMD to late AMD
data_fit_4 <- subset_simplify_factor(data_fit,
                                     score_bl_levels = "early_amd",
                                     score_fu = "PT_conti_worst_eye",
                                     score_fu_levels = c("early_amd", "late_amd"))
#######
# FF4 #
#######

# Case 1: Incidence early AMD
## note: here n risk set is 481 and not 484 as in original paper
## 3 participants had late amd at FU
data_ff4_1.1 <- subset_simplify_factor(data_ff4,
                                     score_bl_levels = "no_amd",
                                     score_fu = "U3T_conti_worst_eye",
                                     score_fu_levels = c("no_amd", "early_amd"))
## as in original paper
data_ff4_1 <- subset_simplify_factor(data_ff4,
                                       score_bl_levels = "no_amd",
                                       score_fu = "U3T_conti_worst_eye",
                                       score_fu_levels = c(no_amd = "no_amd", early_amd = "early_amd", early_amd = "late_amd"))

# Case 2: Incidence late AMD among no AMD at BL (definition 1)
data_ff4_2.1 <- subset_simplify_factor(data_ff4,
                                     score_bl_levels = "no_amd",
                                     score_fu = "U3T_conti_worst_eye",
                                     score_fu_levels = c("no_amd", "late_amd"))

data_ff4_2<- subset_simplify_factor(data_ff4,
                                       score_bl_levels = "no_amd",
                                       score_fu = "U3T_conti_worst_eye",
                                       score_fu_levels = c(no_amd = "no_amd", late_amd = "late_amd", late_amd = "early_amd"))

# Case 3: Incidence late AMD among no AMD at BL or early AMD at BL (definition 2)
## create dummy variable indicating if early or no amd at baseline
data_ff4_3 <- copy(data_ff4)
data_ff4_3[, amd_bl := LT_conti_worst_eye]
data_ff4_3[, amd_bl := fct_drop(amd_bl)]
data_ff4_3 <- subset_simplify_factor(data_ff4_3,
                                     score_bl_levels = c(no_amdD2 = "no_amd", no_amdD2 = "early_amd"),
                                     score_fu = "U3T_conti_worst_eye",
                                     score_fu_levels = c(no_amdD2 = "no_amd", no_amdD2 = "early_amd", late_amd = "late_amd"))

# Case 4: Progression from early AMD to late AMD
data_ff4_4 <- subset_simplify_factor(data_ff4,
                                     score_bl_levels = "early_amd",
                                     score_fu = "U3T_conti_worst_eye",
                                     score_fu_levels = c("early_amd", "late_amd"))
#-------------------------------------------------------------------------------
#                                 fit model
#-------------------------------------------------------------------------------
#######
# FIT #
#######

## Case 1
model_glm_fit_1 <- glm(PT_conti_worst_eye ~ ltalteru + lcsex + time_bl_fu + ltrauchp + ll_hdla,
                   data = data_fit_1,
                   family = binomial(link="logit"))
model_glm_fit_1_intercept <- glm(PT_conti_worst_eye ~ 1,
                                        data = data_fit_1,
                                        family = binomial(link = "logit"))
## Case 2
model_glm_fit_2 <- glm(PT_conti_worst_eye ~ ltalteru + lcsex + time_bl_fu + ltrauchp + ll_hdla,
                   data = data_fit_2,
                   family = binomial(link="logit"))
model_glm_fit_2_intercept <- glm(PT_conti_worst_eye ~ 1,
                                 data = data_fit_2,
                                 family = binomial(link="logit"))
## Case 3
model_glm_fit_3 <- glm(PT_conti_worst_eye ~ ltalteru + lcsex + time_bl_fu + ltrauchp + ll_hdla + amd_bl,
                  data = data_fit_3,
                  family = binomial(link="logit"))
model_glm_fit_3_intercept <- glm(PT_conti_worst_eye ~ 1,
                                       data = data_fit_3,
                                       family = binomial(link = "logit"))
## Case 4
model_glm_fit_4 <- glm(PT_conti_worst_eye ~ ltalteru + lcsex + time_bl_fu + ltrauchp + ll_hdla,
                  data = data_fit_4,
                  family = binomial(link="logit"))
model_glm_fit_4_intercept <- glm(PT_conti_worst_eye ~ 1,
                                        data = data_fit_4,
                                        family = binomial(link = "logit"))

#######
# FF4 #
#######

## Case 1
model_glm_ff4_1 <- glm(U3T_conti_worst_eye ~ ltalteru + lcsex + time_bl_fu + ltrauchp + ll_hdla,
                       data = data_ff4_1,
                       family = binomial(link="logit"))
model_glm_ff4_1_intercept <- glm(U3T_conti_worst_eye ~ 1,
                                        data = data_ff4_1,
                                        family = binomial(link = "logit"))

## Case 2
model_glm_ff4_2 <- glm(U3T_conti_worst_eye ~ ltalteru + lcsex + time_bl_fu + ltrauchp + ll_hdla,
                       data = data_ff4_2,
                       family = binomial(link="logit"))
model_glm_ff4_2_intercept <- glm(U3T_conti_worst_eye ~ 1,
                                        data = data_ff4_2,
                                        family = binomial(link = "logit"))

## Case 3
model_glm_ff4_3 <- glm(U3T_conti_worst_eye ~ ltalteru + lcsex + time_bl_fu + ltrauchp + ll_hdla + amd_bl,
                       data = data_ff4_3,
                       family = binomial(link="logit"))
model_glm_ff4_3_intercept <- glm(U3T_conti_worst_eye ~ 1,
                                        data = data_ff4_3,
                                        family = binomial(link = "logit"))

## Case 4
#model_glm_ff4_4 <- glm(U3T_conti_worst_eye ~ ltalteru + lcsex + time_bl_fu + ltrauchp + ll_hdla,
#                       data = data_ff4_4,
#                       family = binomial(link="logit"))
#-------------------------------------------------------------------------------