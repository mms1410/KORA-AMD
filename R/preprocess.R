rm(list = ls())
library(haven)
library(data.table)
library(checkmate)
library(forcats)
#-------------------------------------------------------------------------------
root_folder <- rstudioapi::getSourceEditorContext()$path
root_folder <- dirname(dirname(root_folder))
data_folder <- file.path(root_folder, "data")
r_folder <- file.path(root_folder, "R")
assets_folder <- file.path(root_folder, "assets")
path_data <- file.path(data_folder, "20230222_KORA_S4_FF4_FIT_StaBLab_with_riskfactors.sav")
path_dictionary <- file.path(data_folder, "vars_to_select")
#------------------------------------------------------------------------------
assertDirectory(data_folder)
if (!dir.exists(assets_folder)) {
  dir.create(assets_folder)
}
assertFileExists(path_data)
assertFileExists(path_dictionary)
assertFileExists(file.path(r_folder, "functions.R"))
#-------------------------------------------------------------------------------
data <- haven::read_sav(file = path_data)
data <- as.data.table(data)
data_dictionary <- fread(path_dictionary)
source(file.path(r_folder, "functions.R"))
#-------------------------------------------------------------------------------
age_groups_fit <- list(
  "(34,45)" = seq(from = 34, to = 45),
  "(45,50)" = seq(from = 45, to = 50),
  "(50,55)" = seq(from = 50, to = 55))
age_groups_ff4 <- list(
  "(53,60)" = seq(from = 53, to = 60),
  "(61,65)" = seq(from = 61, to = 65),
  "(66,75)" = seq(from = 66, to = 75)
)
data <- subset_data(dtbl = data,
                        data.dictionary = data_dictionary,
                        age.groups.fit = age_groups_fit,
                        age.groups.ff4 = age_groups_ff4)
#-------------------------------------------------------------------------------
def_amd_continental <- c("no_amd" = "0", "early_amd" = "1", "early_amd" = "2",
                         "early_amd" = "3", "late_amd" = "4")
def_amd_ferris <- c("no_amd" = "0", "no_amd" = "1", "early_amd"  = "2",
                    "late_amd" = "3", "late_amd" = "4")

to_group <- list(
  "LTFerris_RE_2_sf" = def_amd_ferris,
  "LTFerris_LI_2_sf" = def_amd_ferris,
  "LTConti_RE_2_sf" = def_amd_continental,
  "LTConti_LI_2_sf" = def_amd_continental,
  "U3TFerris_RE_2_sf" = def_amd_ferris,
  "U3TFerris_LI_2_sf" = def_amd_ferris,
  "U3TConti_RE_2_sf" = def_amd_continental,
  "U3TConti_LI_2_sf" = def_amd_continental,
  "PTFerris_RE_2_sf" = def_amd_ferris,
  "PTFerris_LI_2_sf" = def_amd_ferris,
  "PTConti_RE_2_sf" = def_amd_continental,
  "PTConti_LI_2_sf" = def_amd_continental
)

data <- set_groups(data, to_group)
data$lcsex <- fct_recode(data[["lcsex"]], !!!c("M" = "1", "F" = "2"))
data$ltrauchp <- fct_recode(data[["ltrauchp"]], !!!c("non_smoker" = "0",
                                                     "non_smoker" = "1",
                                                     "former_smoker" = "2",
                                                     "former_smoker" = "3",
                                                     "active_smoker" = "4"))
data$person_id <- as.factor(seq(from = 1, to = nrow(data)))

## create extra column for worst eye
data$LT_ferris_worst_eye <- pmax(data$LTFerris_RE_2_sf, data$LTConti_LI_2_sf, na.rm = TRUE)
data$LT_conti_worst_eye <- pmax(data$LTConti_RE_2_sf, data$LTConti_LI_2_sf, na.rm = TRUE)
data$PT_conti_worst_eye <- pmax(data$PTConti_LI_2_sf, data$PTConti_RE_2_sf, na.rm = TRUE)
data$PT_ferris_worst_eye <- pmax(data$PTFerris_LI_2_sf, data$PTFerris_RE_2_sf, na.rm = TRUE)
data$U3T_conti_worst_eye <- pmax(data$U3TConti_LI_2_sf, data$U3TConti_RE_2_sf, na.rm = TRUE)
data$U3T_ferris_worst_eye <- pmax(data$U3TFerris_LI_2_sf, data$U3TFerris_RE_2_sf, na.rm = TRUE)
#-------------------------------------------------------------------------------
# FIT: 506 & FF4: 350
# we know that if ferris score is na then also conti score and vice versa.
data_fit <- data[!is.na(LT_ferris_worst_eye) & !is.na(PT_ferris_worst_eye)]
data_ff4 <- data[!is.na(LT_ferris_worst_eye) & !is.na(U3T_ferris_worst_eye)] 
data_fit[, grep("^u3t|ff4", colnames(data_fit), ignore.case = TRUE) := NULL]
data_fit$time_bl_fu <- data_fit$PTALTERU - data_fit$ltalteru
data_ff4[, grep("^PT|fit", colnames(data_ff4), ignore.case = TRUE) := NULL]
data_ff4$time_bl_fu <- data_ff4$u3talteru - data_ff4$ltalteru

duplicate_id <- data_fit$person_id[data_fit$person_id %in% data_ff4$person_id]
data_ff4 <- data_ff4[!person_id %in% duplicate_id]
#-------------------------------------------------------------------------------
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
get_summary_amd_factor(data_fit, cols_summary = c("LT_conti_worst_eye", "LTConti_RE_2_sf", "LTConti_LI_2_sf",
                                           "PT_conti_worst_eye", "PTConti_RE_2_sf", "PTConti_LI_2_sf"),
                       log_filename = file.path(assets_folder, "fit_amd_summary.csv"))

get_summary_amd_factor(data_ff4, cols_summary = c("LT_conti_worst_eye", "LTConti_RE_2_sf", "LTConti_LI_2_sf",
                                           "U3T_conti_worst_eye", "U3TConti_RE_2_sf", "U3TConti_LI_2_sf"),
                       log_filename = file.path(assets_folder, "ff4_amd_summary.csv"))