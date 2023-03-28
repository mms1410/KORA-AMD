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

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
library(lme4)
model.ff4.ferris <- glmer(formula = PT_amd_status ~ LTFerris_LI_2_sf + LTFerris_RE_2_sf +
                            ltrauchp + ltalteru + lcsex + (1 | person_id),
                          data = fit.ferris,
                          family = brms::multinomial())
#-------------------------------------------------------------------------------