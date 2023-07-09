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

## Case1: Incidence early AMD, no AMD at BL
data_fit_conti_1 <- subset_simplify_factor(data_fit,
                                       score_bl_levels = "no_amd",
                                       score_fu = "PT_conti_worst_eye",
                                       score_fu_levels = c("no_amd", "early_amd"))

# Case 2: Incidence late AMD among no AMD at BL (definition 1)
data_fit_conti_2 <- subset_simplify_factor(data_fit,
                                      score_bl_levels = "no_amd",
                                      score_fu = "PT_conti_worst_eye",
                                      score_fu_levels = c("no_amd", "late_amd"))

# Case 3: Incidence late AMD among no AMD at BL or early AMD at BL (definition 2)
## create dummy variable indicating if early or no amd at baseline
data_fit_conti_3 <- copy(data_fit)
data_fit_conti_3[, amd_bl := LT_conti_worst_eye]
data_fit_conti_3[, amd_bl := fct_drop(amd_bl)]
data_fit_conti_3 <- subset_simplify_factor(data_fit_conti_3,
                                     score_bl_levels = c("no_amd","early_amd"),
                                     score_fu = "PT_conti_worst_eye",
                                     score_fu_levels = c("no_amd", "early_amd", "late_amd"))

# Case 4: Progression from early AMD to late AMD
data_fit_conti_4 <- subset_simplify_factor(data_fit,
                                     score_bl_levels = "early_amd",
                                     score_fu = "PT_conti_worst_eye",
                                     score_fu_levels = c("early_amd", "late_amd"))
#######
# FF4 #
#######

# Case 1: Incidence early AMD
data_ff4_conti_1 <- subset_simplify_factor(data_ff4,
                                       score_bl_levels = "no_amd",
                                       score_fu = "U3T_conti_worst_eye",
                                       score_fu_levels = c("no_amd", "early_amd"))

# Case 2: Incidence late AMD among no AMD at BL (definition 1)
data_ff4_conti_2<- subset_simplify_factor(data_ff4,
                                       score_bl_levels = "no_amd",
                                       score_fu = "U3T_conti_worst_eye",
                                       score_fu_levels = c("no_amd", "late_amd"))

# Case 3: Incidence late AMD among no AMD at BL or early AMD at BL (definition 2)
## create dummy variable indicating if early or no amd at baseline
data_ff4_conti_3 <- copy(data_ff4)
data_ff4_conti_3[, amd_bl := LT_conti_worst_eye]
data_ff4_conti_3[, amd_bl := fct_drop(amd_bl)]
data_ff4_conti_3 <- subset_simplify_factor(data_ff4_conti_3,
                                     score_bl_levels = c("no_amd", "early_amd"),
                                     score_fu = "U3T_conti_worst_eye",
                                     score_fu_levels = c("no_amd", "early_amd", "late_amd"))

# Case 4: Progression from early AMD to late AMD
data_ff4_conti_4 <- subset_simplify_factor(data_ff4,
                                     score_bl_levels = "early_amd",
                                     score_fu = "U3T_conti_worst_eye",
                                     score_fu_levels = c("early_amd", "late_amd"))
#-------------------------------------------------------------------------------
#                                 GLM model
#-------------------------------------------------------------------------------
#######
# FIT #
#######

## Case 1
model_glm_fit_conti_1 <- glm(ifelse(PT_conti_worst_eye == "early_amd", 1, 0) ~ ltalteru + lcsex  + ltcigreg_sf + ll_hdla + time_bl_fu,
                   data = data_fit_conti_1,
                   family = binomial(link="logit"))

model_glm_fit_conti_1_gls <- glm(ifelse(PT_conti_worst_eye == "early_amd", 1, 0)  ~ ltalteru + lcsex + ltcigreg_sf + ll_hdla + time_bl_fu+ GrsWeightedByBeta,
                       data = data_fit_conti_1,
                       family = binomial(link="logit"))


## Case 2
model_glm_fit_conti_2 <- glm(ifelse(PT_conti_worst_eye == "late_amd", 1, 0) ~ ltalteru + lcsex + ltcigreg_sf + ll_hdla + time_bl_fu,
                   data = data_fit_conti_2,
                   family = binomial(link="logit"))

model_glm_fit_conti_2_gls <- glm(ifelse(PT_conti_worst_eye == "late_amd", 1, 0) ~ ltalteru + lcsex + ltcigreg_sf + ll_hdla + time_bl_fu+ GrsWeightedByBeta,
                           data = data_fit_conti_2,
                           family = binomial(link="logit"))

## Case 3
model_glm_fit_conti_3 <- glm(ifelse(PT_conti_worst_eye == "early_amd", 1, 0) ~ ltalteru + lcsex + ltcigreg_sf + ll_hdla + time_bl_fu + amd_bl,
                  data = data_fit_conti_3,
                  family = binomial(link="logit"))

model_glm_fit_conti_3_gls <- glm(ifelse(PT_conti_worst_eye == "early_amd", 1, 0) ~ ltalteru + lcsex + ltcigreg_sf + ll_hdla + time_bl_fu +  amd_bl + GrsWeightedByBeta,
                           data = data_fit_conti_3,
                           family = binomial(link="logit"))

## Case 4
model_glm_fit_conti_4 <- glm(ifelse(PT_conti_worst_eye == "late_amd", 1, 0) ~ ltalteru + lcsex + ltcigreg_sf + ll_hdla + time_bl_fu,
                  data = data_fit_conti_4,
                  family = binomial(link="logit"))

model_glm_fit_conti_4_gls <- glm(ifelse(PT_conti_worst_eye == "late_amd", 1, 0)  ~ ltalteru + lcsex + ltcigreg_sf + ll_hdla + time_bl_fu +GrsWeightedByBeta,
                           data = data_fit_conti_4,
                           family = binomial(link="logit"))
#######
# FF4 #
#######

## Case 1
model_glm_ff4_conti_1 <- glm(ifelse(U3T_conti_worst_eye == "early_amd", 1 , 0)  ~ ltalteru + lcsex + ltcigreg_sf + ll_hdla + time_bl_fu,
                       data = data_ff4_conti_1,
                       family = binomial(link="logit"))

model_glm_ff4_conti_1_gls <- glm(ifelse(U3T_conti_worst_eye == "early_amd", 1, 0)  ~ ltalteru + lcsex  + ltcigreg_sf + ll_hdla + time_bl_fu +GrsWeightedByBeta,
                       data = data_ff4_conti_1,
                       family = binomial(link="logit"))

## Case 2
model_glm_ff4_conti_2 <- glm(ifelse(U3T_conti_worst_eye == "late_amd", 1, 0) ~ ltalteru + lcsex + ltcigreg_sf + ll_hdla + time_bl_fu,
                       data = data_ff4_conti_2,
                       family = binomial(link="logit"))

model_glm_ff4_2_conti_gls <- glm(ifelse(U3T_conti_worst_eye == "late_amd", 1, 0) ~ ltalteru + lcsex + ltcigreg_sf + ll_hdla + time_bl_fu + GrsWeightedByBeta,
                       data = data_ff4_conti_2,
                       family = binomial(link="logit"))

## Case 3
model_glm_ff4_conti_3 <- glm(ifelse(U3T_conti_worst_eye == "early_amd", 1, 0) ~ ltalteru + lcsex  + ltcigreg_sf + ll_hdla + time_bl_fu  + amd_bl,
                       data = data_ff4_conti_3,
                       family = binomial(link="logit"))

model_glm_ff4_conti_3_gls <- glm(ifelse(U3T_conti_worst_eye == "early_amd", 1, 0) ~ ltalteru + lcsex  + ltcigreg_sf + ll_hdla + time_bl_fu + amd_bl + GrsWeightedByBeta,
                       data = data_ff4_conti_3,
                       family = binomial(link="logit"))

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------