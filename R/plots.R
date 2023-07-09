library(ggplot2)
library(haven)
library(data.table)
library(checkmate)
library(forcats)
library(this.path)  # path via rstudioapi not available if called via terminal
#===============================================================================
root_folder <- this.path::this.dir()
root_folder <- dirname(root_folder)
data_folder <- file.path(root_folder, "data")
r_folder <- file.path(root_folder, "R")
source(file.path(r_folder, "functions.R"))
theme_set(theme_linedraw(base_size = 18))
options(ggplot2.discrete.colour= c("red", "blue"))
#===============================================================================


