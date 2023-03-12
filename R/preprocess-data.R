library(haven)
library(data.table)
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
root.folder <- rstudioapi::getSourceEditorContext()$path
root.folder <- dirname(dirname(root.folder))
data.folder <- file.path(root.folder, "data")
#------------------------------------------------------------------------------
data <- haven::read_sav(file.path(data.folder, "20230217_KORA_S4_FF4_FIT_StatLab.sav"))
data <- as.data.table(data)