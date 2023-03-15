library(data.table)
library(mgcv)
#-------------------------------------------------------------------------------
fac.to.num <- function(x) as.numeric(as.character(x))
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
## partition according to AMD definition and AMD score (ferris vs. conti)
# definition1: no AMD at baseline in at least one eye
# definition2: no AMD or Early AMD in at least one eye

## get data at t = 0
data.fit.start <- data.fit[, .SD[which.min(Age)], by = Person_id]
data.ff4.start <- data.ff4[, .SD[which.min(Age)], by = Person_id]

person_id.fit.def1.ferris <- data.fit.start[fac.to.num(ferris_score_r) == 0 | fac.to.num(ferris_score_l == 0), Person_id]
person_id.fit.def2.ferris <- data.fit.start[(fac.to.num(ferris_score_r) %in% c(0, 1)) | (fac.to.num(ferris_score_l == 0) %in% c(0, 1)), Person_id]
#-------------------------------------------------------------------------------
## partition according to age group

#-------------------------------------------------------------------------------
to.delete <- ls()
to.delete <- to.delete[!to.delete %in% c("data.ff4",
                                         "data.ff4.1", "data.ff4.2", "data.ff4.3",
                                         "data.fit",
                                         "data.fit.1", "data.fit.2", "data.fit.3")]
rm(list = to.delete)
#-------------------------------------------------------------------------------

