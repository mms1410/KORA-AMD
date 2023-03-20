rm(list = ls())
library(haven)
library(data.table)
library(checkmate)
library(forcats)
#-------------------------------------------------------------------------------
root.folder <- rstudioapi::getSourceEditorContext()$path
root.folder <- dirname(dirname(root.folder))
data.folder <- file.path(root.folder, "data")
r.folder <- file.path(root.folder, "R")
assets.folder <- file.path(root.folder, "assets")
path.data <- file.path(data.folder, "20230222_KORA_S4_FF4_FIT_StaBLab_with_riskfactors.sav")
path.dictionary <- file.path(data.folder, "vars_to_select")
#------------------------------------------------------------------------------
assertDirectory(data.folder)
if (!dir.exists(assets.folder)) {
  dir.create(assets.folder)
}
assertFileExists(path.data)
assertFileExists(path.dictionary)
assertFileExists(file.path(r.folder, "functions.R"))
#------------------------------------------------------------------------------
data <- haven::read_sav(file = path.data)
data <- as.data.table(data)
data.dictionary <- fread(path.dictionary)
source(file.path(r.folder, "functions.R"))
#------------------------------------------------------------------------------
age.groups.fit <- list(
  "(34,45)" = seq(from = 34, to = 45),
  "(45,50)" = seq(from = 45, to = 50),
  "(50,55)" = seq(from = 50, to = 55))
age.groups.ff4 <- list(
  "(53,60)" = seq(from = 53, to = 60),
  "(61,65)" = seq(from = 61, to = 65),
  "(66,75)" = seq(from = 66, to = 75)
)
data <- preprocess.data(dtbl = data,
                        data.dictionary = data.dictionary,
                        age.groups.fit = age.groups.fit,
                        age.groups.ff4 = age.groups.ff4)
#------------------------------------------------------------------------------
def.amd.continental <- c("no_amd" = "0", "early_amd" = "1", "early_amd" = "2",
                         "early_amd" = "3", "late_amd" = "4")
def.amd.ferris <- c("no_amd" = "0", "no_amd" = "1", "early_amd"  = "2",
                    "late_amd" = "3", "late_amd" = "4")

to.group <- list(
  "LTFerris_RE_2_sf" = def.amd.ferris,
  "LTFerris_LI_2_sf" = def.amd.ferris,
  "LTConti_RE_2_sf" = def.amd.continental,
  "LTConti_LI_2_sf" = def.amd.continental,
  "U3TFerris_RE_2_sf" = def.amd.ferris,
  "U3TFerris_LI_2_sf" = def.amd.ferris,
  "U3TConti_RE_2_sf" = def.amd.continental,
  "U3TConti_LI_2_sf" = def.amd.continental,
  "PTFerris_RE_2_sf" = def.amd.ferris,
  "PTFerris_LI_2_sf" = def.amd.ferris,
  "PTConti_RE_2_sf" = def.amd.continental,
  "PTConti_LI_2_sf" = def.amd.continental
)
data <- set.groups(data, to.group)
#------------------------------------------------------------------------------
get.data.summary.factors(data)
