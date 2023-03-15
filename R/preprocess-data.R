library(data.table)
library(mgcv)
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
root.folder <- rstudioapi::getSourceEditorContext()$path
root.folder <- dirname(dirname(root.folder))
data.folder <- file.path(root.folder, "data")
r.folder <- file.path(root.folder, "R")
source(file.path(root.folder, "ersteVersuche.R"))
#-------------------------------------------------------------------------------

data.ff4 <- as.data.table(s4Ff4Sample)
data.fit <- as.data.table(s4FitSample)
to.factor.ff4 <- c("Person_id", "Sex",
                   "Results_Right_Eye", "Results_Left_Eye", "Ferris_Right_Eye",
                   "Ferris_Left_Eye", "3_Continent_Score_Right_Eye",
                   "3_Continent_Score_Left_Eye", "Study")
data.ff4[, (to.factor.ff4) := lapply(.SD, as.factor), .SDcols = to.factor.ff4]

to.factor.fit <- c("Person_id", "Sex", "Results_Right_Eye",
                   "Results_Left_Eye", "Ferris_Right_Eye", "Ferris_Left_Eye",
                   "3_Continent_Score_Right_Eye", "3_Continent_Score_Left_Eye",
                   "Study")
data.fit[, (to.factor.fit) := lapply(.SD, as.factor), .SDcols = to.factor.fit]
#-------------------------------------------------------------------------------
colnames.new.fit <- colnames(data.fit)
colnames.new.fit[colnames.new.fit == "3_Continent_Score_Right_Eye"] <- "continent_score_r"
colnames.new.fit[colnames.new.fit == "3_Continent_Score_Left_Eye"] <- "continent_score_l"
colnames.new.fit[colnames.new.fit == "Ferris_Right_Eye"] <- "ferris_score_r"
colnames.new.fit[colnames.new.fit == "Ferris_Left_Eye"] <- "ferris_score_l"

colnames(data.fit) <- colnames.new.fit
## same column names
colnames(data.ff4) <- colnames.new.fit
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
to.delete <- ls()
to.delete <- to.delete[!to.delete %in% c("data.ff4",
                                         "data.ff4.1", "data.ff4.2", "data.ff4.3",
                                         "data.fit",
                                         "data.fit.1", "data.fit.2", "data.fit.3")]
rm(list = to.delete)
#-------------------------------------------------------------------------------

