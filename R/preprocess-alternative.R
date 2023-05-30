#-------------------------------------------------------------------------------
library(haven)
library(data.table)
library(checkmate)
library(forcats)
library(this.path)  # path via rstudioapi not available if called via terminal
#-------------------------------------------------------------------------------
##
## ltcigreg_sf
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
#data <- data[grs, on = "zz_nr"] #reduces data set
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

cbind(data$U3TConti_LI_2_sf, data$U3TConti_RE_2_sf)

my_pmax <- function(x,y) {
  x <- as.numeric(as.character(x))
  y <- as.numeric(as.character(y))
  pmax(x,y)
}
fct_collapse(data$LTConti_LI_2_sf, )

u3t_conti_worst <- pmax(data$U3TConti_LI_2_sf, data$U3TConti_RE_2_sf)
my_pmax(data$U3TConti_LI_2_sf, data$U3TConti_RE_2_sf) %>% table()
pt_conti_worst <- pmax(data$PTConti_LI_2_sf, data$PTConti_RE_2_sf)
lt_conti_worst <- pmax(data$LTConti_LI_2_sf, data$LTConti_RE_2_sf)


