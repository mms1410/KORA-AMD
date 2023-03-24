#-------------------------------------------------------------------------------
root_folder <- rstudioapi::getSourceEditorContext()$path
root_folder <- dirname(dirname(root_folder))
data_folder <- file.path(root_folder, "data")
r_folder <- file.path(root_folder, "R")
assets_folder <- file.path(root_folder, "assets")
path_data <- file.path(data_folder, "20230222_KORA_S4_FF4_FIT_StaBLab_with_riskfactors.sav")
path_dictionary <- file.path(data_folder, "vars_to_select")
source(file.path(r_folder, "preprocess.R"))
#-------------------------------------------------------------------------------
# FIT: 506 & FF4: 350
# we know that if ferris score is na then also conti score and vice versa.

data_fit <- data[!is.na(LT_ferris_worst_eye) & !is.na(PT_ferris_worst_eye), .(person_id, lcsex, ltalteru, LT_ferris_worst_eye, LT_conti_worst_eye, PT_ferris_worst_eye, PT_conti_worst_eye, U3T_conti_worst_eye, ll_hdln, ltrauchp)]
data_ff4 <- data[!is.na(LT_ferris_worst_eye) & !is.na(U3T_ferris_worst_eye), .(person_id, lcsex, ltalteru, LT_ferris_worst_eye, LT_conti_worst_eye, PT_ferris_worst_eye, PT_conti_worst_eye, U3T_conti_worst_eye, ll_hdln, ltrauchp)] 


duplicate_id <- data_fit$person_id[data_fit$person_id %in% data_ff4$person_id]
data_ff4 <- data_ff4[!person_id %in% duplicate_id]

nrow(data_ff4)

get_summary_amd(data_fit, cols.summary = c("LT_conti_worst_eye"))

data_fit_conti_1 <- data_fit[data_fit[["LT_conti_worst_eye"]] == "no_amd" &
                             data_fit[["PT_conti_worst_eye"]] == "early_amd"]
data_fit_conti_2 <- data_fit[data_fit[["LT_conti_worst_eye"]] == "no_amd" &
           data_fit[["PT_conti_worst_eye"]] == "late_amd"]
data_fit_conti_3 <- data_fit[(data_fit[["LT_conti_worst_eye"]] == "no_amd" |
                                data_fit[["PT_conti_worst_eye"]] == "early_amd") &
                               (data_fit[["PT_conti_worst_eye"]] == "late_amd")]
data_fit_conti_4 <- data_fit[data_fit[["LT_conti_worst_eye"]] == "no_amd" &
                               data_fit[["PT_conti_worst_eye"]] == "late_amd"]

nrow(data_fit_conti_1) + nrow(data_fit_conti_2) + nrow(data_fit_conti_3) + nrow(data_fit_conti_4)


data_ff4_ferris_1 <- data_ff4_ferris_groups[[1]]
data_ff4_ferris_2 <- data_ff4_ferris_groups[[2]]
data_ff4_ferris_3 <- data_ff4_ferris_groups[[3]]
data_ff4_ferris_4 <- data_ff4_ferris_groups[[4]]

#-------------------------------------------------------------------------------
library(lme4)
model.ff4.ferris <- glmer(formula = PT_amd_status ~ LTFerris_LI_2_sf + LTFerris_RE_2_sf +
                            ltrauchp + ltalteru + lcsex + (1 | person_id),
                          data = fit.ferris,
                          family = brms::multinomial())
#-------------------------------------------------------------------------------