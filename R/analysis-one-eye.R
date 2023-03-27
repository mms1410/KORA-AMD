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
# case 1
## at risk OK 484
data_fit[LT_conti_worst_eye == "no_amd"]
## events OK 33
data_fit[LT_conti_worst_eye == "no_amd" & PT_conti_worst_eye == "early_amd"]

# case 2
## at riks OK 484
data_fit[LT_conti_worst_eye == "no_amd"]
## events OK 3
data_fit[LT_conti_worst_eye == "no_amd"  & PT_conti_worst_eye == "late_amd"]

# case 3
## at riks OK 506
data_fit[(LT_conti_worst_eye == "no_amd" | LT_conti_worst_eye == "early_amd")]
## events OK 8
data_fit[(LT_conti_worst_eye == "no_amd" | LT_conti_worst_eye == "early_amd") & PT_conti_worst_eye == "late_amd"]

# case 4
## at riskt 22
data_fit[(LT_conti_worst_eye == "no_amd" | LT_conti_worst_eye == "early_amd")]
## events OK 5
data_fit[LT_conti_worst_eye == "early_amd" & PT_conti_worst_eye == "late_amd"]

#-------------------------------------------------------------------------------
library(lme4)
model.ff4.ferris <- glmer(formula = PT_amd_status ~ LTFerris_LI_2_sf + LTFerris_RE_2_sf +
                            ltrauchp + ltalteru + lcsex + (1 | person_id),
                          data = fit.ferris,
                          family = brms::multinomial())
#-------------------------------------------------------------------------------