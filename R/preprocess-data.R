library(data.table)
library(ggplot2)
library(ggsankey)
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

data.fit.ferris.1 <- data.fit[Person_id %in% person_id.fit.def1.ferris]
data.fit.ferris.2 <- data.fit[Person_id %in% person_id.fit.def2.ferris]
#-------------------------------------------------------------------------------
## partition according to age group
fit.age.1 <- seq(from=34, to=45)
fit.age.2 <- seq(from=46, to=50)
fit.age.3 <- seq(from=51, to=55)

ff4.age.1 <- seq(from=53, to=60)
ff4.age.2 <- seq(from=61, to=65)
ff4.age.3 <- seq(from=66, to=75)

data.fit.ferris.1.age1 <- data.fit.ferris.1[Age %in% fit.age.1]
data.fit.ferris.1.age2 <- data.fit.ferris.1[Age %in% fit.age.2]
data.fit.ferris.1.age3 <- data.fit.ferris.1[Age %in% fit.age.3]

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------

