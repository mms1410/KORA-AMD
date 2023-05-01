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
path_grs <- file.path(data_folder, "KORA_AMD_Score_update.txt")
path_dictionary <- file.path(data_folder, "vars_to_select")
#------------------------------------------------------------------------------
assertDirectory(data_folder)
if (!dir.exists(assets_folder)) {
  dir.create(assets_folder)
}
assertFileExists(path_data)
assertFileExists(path_grs)
assertFileExists(path_dictionary)
assertFileExists(file.path(r_folder, "functions.R"))
#-------------------------------------------------------------------------------
data <- haven::read_sav(file = path_data)
grs <- fread(path_grs)

data <- as.data.table(data)
data_dictionary <- fread(path_dictionary)
source(file.path(r_folder, "functions.R"))
#-------------------------------------------------------------------------------
age_groups_fit <- list(
  "(34,45)" = seq(from = 34, to = 45),
  "(45,50)" = seq(from = 46, to = 50),
  "(50,55)" = seq(from = 51, to = 55))
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
#data <- data[grs, on = "zz_nr"] reduces data set
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

## note: warning might be triggered if some levels are no contained in group.
## here original level of 3 was not present in older age group.
## ordered==True needed to detect worst eye later.
data <- set_groups(data, to_group, ordered = TRUE)
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
## remove factor ordering
col_names <- names(sapply(data, is.factor))[sapply(data, is.factor)]
data[, (col_names) := lapply(.SD, function(x){factor(x, ordered = FALSE)}), .SDcols = col_names]
rm(list = c("col_names"))
data$ltalteru <- as.numeric(data$ltalteru)
data$ll_hdla <- as.numeric(data$ll_hdla)
data$PTALTERU <- as.numeric(data$PTALTERU)
data$u3talteru <- as.numeric(data$u3talteru)

data$ltalteru  <- data$ltalteru - mean(data$ltalteru, na.rm = TRUE)
data$ll_hdla <- data$ll_hdla -mean(data$ll_hdla, na.rm = TRUE)
data$PTALTERU <- data$PTALTERU -mean(data$PTALTERU, na.rm = TRUE)
data$u3talteru <- data$u3talteru -mean(data$u3talteru, na.rm = TRUE)
#-------------------------------------------------------------------------------
data$lcsex <- relevel(data$lcsex, ref = "M")
#-------------------------------------------------------------------------------
# FIT: 506 & FF4: 350
# we know that if ferris score is na then also conti score and vice versa.
data_fit <- data[!is.na(LT_ferris_worst_eye) & !is.na(PT_ferris_worst_eye)]
data_ff4 <- data[!is.na(LT_ferris_worst_eye) & !is.na(U3T_ferris_worst_eye)] 
data_fit[, grep("^u3t|ff4", colnames(data_fit), ignore.case = TRUE) := NULL]
data_ff4[, grep("^PT|fit", colnames(data_ff4), ignore.case = TRUE) := NULL]

data_ff4$time_bl_fu <- data_ff4$u3talteru - data_ff4$ltalteru
data_fit$time_bl_fu <- data_fit$PTALTERU - data_fit$ltalteru

duplicate_id <- data_fit$person_id[data_fit$person_id %in% data_ff4$person_id]
data_ff4 <- data_ff4[!person_id %in% duplicate_id]
#-------------------------------------------------------------------------------