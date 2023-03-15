library(data.table)
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
root.folder <- rstudioapi::getSourceEditorContext()$path
root.folder <- dirname(dirname(root.folder))
data.folder <- file.path(root.folder, "data")
r.folder <- file.path(root.folder, "R")
source(file.path(root.folder, "ersteVersuche.R"))
#-------------------------------------------------------------------------------
data.ff4 <- as.data.table(s4Ff4Sample)
data.ff4 <- data.ff4[, .(Person_id, FF4_Erh_Nr, FF4_u3csex, FF4_u3talteru,
                         FF4_Result_OD_2, FF4_Result_OS_2,
                         FF4_Ferris2013_OD_2, FF4_Ferris2013_OS_2,
                         FF4_Three_Conti_OD_2, FF4_Three_Conti_OS_2)]
colnames(data.ff4)

data.fit <- as.data.table(s4FitSample)
data.fit <- data.fit[, .(Person_id, FIT_erh_nr, FIT_pcsex, FIT_ptalteru,
             FIT_Result_OD_2, FIT_Result_OS_2,
             FIT_Ferris2013_OD_2, FIT_Ferris2013_OS_2,
             FIT_Three_Conti_OD_2, FIT_Three_Conti_OS_2)]
colnames(data.fit)
#-------------------------------------------------------------------------------
## clean up
to.delete <- ls()
to.delete <- to.delete[!(to.delete %in% c("data", "data.fit", "data.ff4"))]
rm(list = to.delete)
#-------------------------------------------------------------------------------
fac.to.num <- function(x) as.numeric(as.character(x))
to.factor.ff4 <- c("Person_id", "FF4_u3csex", "FF4_Erh_Nr",
                   "FF4_Result_OD_2", "FF4_Result_OS_2",
                   "FF4_Ferris2013_OD_2", "FF4_Ferris2013_OS_2",
                   "FF4_Three_Conti_OD_2", "FF4_Three_Conti_OS_2")
data.ff4[, (to.factor.ff4) := lapply(.SD, as.factor), .SDcols = to.factor.ff4]

to.factor.fit <- c("Person_id", "FIT_pcsex", "FIT_erh_nr",
                   "FIT_Result_OD_2", "FIT_Result_OS_2",
                   "FIT_Ferris2013_OD_2", "FIT_Ferris2013_OS_2",
                   "FIT_Three_Conti_OD_2", "FIT_Three_Conti_OS_2"
                   )
data.fit[, (to.factor.fit) := lapply(.SD, as.factor), .SDcols = to.factor.fit]
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
colnames.new.fit <- colnames(data.fit)
colnames.new.fit[colnames.new.fit == "FIT_Three_Conti_OD_2"] <- "continent_score_r"
colnames.new.fit[colnames.new.fit == "FIT_Three_Conti_OS_2"] <- "continent_score_l"
colnames.new.fit[colnames.new.fit == "FIT_Ferris2013_OD_2"] <- "ferris_score_r"
colnames.new.fit[colnames.new.fit == "FIT_Ferris2013_OS_2"] <- "ferris_score_l"
colnames.new.fit[colnames.new.fit == "FIT_ptalteru"] <- "age"
colnames.new.fit[colnames.new.fit == "FIT_pcsex"] <- "sex"
colnames(data.fit) <- colnames.new.fit
colnames(data.fit)


colnames.new.ff4 <- colnames(data.ff4)
colnames.new.ff4[colnames.new.ff4 == "FF4_Three_Conti_OD_2"] <- "continent_score_r"
colnames.new.ff4[colnames.new.ff4 == "FF4_Three_Conti_OS_2"] <- "continent_score_l"
colnames.new.ff4[colnames.new.ff4 == "FF4_Ferris2013_OD_2"] <- "ferris_score_r"
colnames.new.ff4[colnames.new.ff4 == "FF4_Ferris2013_OS_2"] <- "ferris_score_l"
colnames.new.ff4[colnames.new.ff4 == "FF4_u3talteru"] <- "age"
colnames.new.ff4[colnames.new.ff4 == "FF4_u3csex"] <- "sex"
colnames(data.ff4) <- colnames.new.ff4
colnames(data.ff4)
#-------------------------------------------------------------------------------
levels(data.ff4$sex) <- c("M", "W")
levels(data.fit$sex) <- c("M", "W")
#-------------------------------------------------------------------------------
summary(data.fit$age)
summary(data.ff4$age)
data.fit$age <- data.fit$age - 18
data.ff4$age <- data.ff4$age - 14
summary(data.fit$age)
summary(data.ff4$age)
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
## partition according to AMD definition and AMD score (ferris vs. conti)
# definition1: no AMD at baseline in at least one eye
# definition2: no AMD or Early AMD in at least one eye

## get data at t = 0 (needed for AMD definition)
data.fit.start <- data.fit[, .SD[which.min(age)], by = Person_id] # nrow 451
data.ff4.start <- data.ff4[, .SD[which.min(age)], by = Person_id] # nrow 261

table(data.fit.start$age)
table(data.ff4.start$age)
#-------------------------------------------------------------------------------
## get person id for AMD definitions
## fit data
person_id.fit.def1.ferris <- data.fit.start[fac.to.num(ferris_score_r) == 0 | fac.to.num(ferris_score_l == 0), Person_id]
person_id.fit.def2.ferris <- data.fit.start[(fac.to.num(ferris_score_r) %in% c(0, 1)) | (fac.to.num(ferris_score_l == 0) %in% c(0, 1)), Person_id]

person_id.fit.def1.cont <- data.fit.start[fac.to.num(continent_score_r) == 0 | fac.to.num(continent_score_l == 0), Person_id]
person_id.fit.def2.cont <- data.fit.start[(fac.to.num(continent_score_r) %in% c(0, 1)) | (fac.to.num(continent_score_l == 0) %in% c(0, 1)), Person_id]
## ff4 data
person_id.ff4.def1.ferris <- data.ff4.start[(fac.to.num(ferris_score_r) == 0) | (fac.to.num(ferris_score_r == 0)), Person_id]
person_id.ff4.def2.ferris <- data.ff4.start[(fac.to.num(ferris_score_r) %in% c(0, 1)) | (fac.to.num(ferris_score_l) %in% c(0, 1)), Person_id]

person_id.ff4.def1.cont <- data.ff4.start[(fac.to.num(continent_score_r) == 0) | (fac.to.num(continent_score_l == 0)), Person_id]
person_id.ff4.def2.cont <- data.ff4.start[(fac.to.num(continent_score_r) %in% c(0, 1)) | (fac.to.num(continent_score_l) %in% c(0, 1)), Person_id]
#-------------------------------------------------------------------------------
## FIT
length(person_id.fit.def1.ferris)
length(person_id.fit.def2.ferris)
length(unique(person_id.fit.def1.ferris))
length(unique(person_id.fit.def2.ferris))


length(person_id.fit.def1.cont)
length(person_id.fit.def2.cont)
length(unique(person_id.fit.def1.cont))
length(unique(person_id.fit.def2.cont))

## FF4
length(person_id.ff4.def1.ferris)
length(person_id.ff4.def2.ferris)
length(unique(person_id.ff4.def1.ferris))
length(unique(person_id.ff4.def2.ferris))


length(person_id.ff4.def1.cont)
length(person_id.ff4.def2.cont)
length(unique(person_id.ff4.def1.cont))
length(unique(person_id.ff4.def2.cont))

#-------------------------------------------------------------------------------
#------------ partition data according to definition of AMD score---------------
#-------------------------------------------------------------------------------
## FIT
data.fit.ferris.def1 <- data.fit[Person_id %in% person_id.fit.def1.ferris]
data.fit.ferris.def2 <- data.fit[Person_id %in% person_id.fit.def2.ferris]

data.fit.cont.def1 <- data.fit[Person_id %in% person_id.fit.def1.cont]
data.fit.cont.def2 <- data.fit[Person_id %in% person_id.fit.def2.cont]

## FF4
data.ff4.ferris.def1 <- data.ff4[Person_id %in% person_id.ff4.def1.ferris]
data.ff4.ferris.def2 <- data.ff4[Person_id %in% person_id.ff4.def2.ferris]

data.ff4.cont.def1 <- data.ff4[Person_id %in% person_id.ff4.def1.cont]
data.ff4.cont.def2 <- data.ff4[Person_id %in% person_id.ff4.def2.cont]
#-------------------------------------------------------------------------------
#
#-------------------------------------------------------------------------------
#---------------------------------- SET AGE BINS -------------------------------
#-------------------------------------------------------------------------------
fit.age.1 <- seq(from=34, to=45)
fit.age.2 <- seq(from=46, to=50)
fit.age.3 <- seq(from=51, to=55)

ff4.age.1 <- seq(from=53, to=60)
ff4.age.2 <- seq(from=61, to=65)
ff4.age.3 <- seq(from=66, to=75)
#-------------------------------------------------------------------------------
#-------------------------- PARTITON AGE & DEF (FERRIS) ------------------------
#-------------------------------------------------------------------------------
#
#
#----------------------------------- FIT ---------------------------------------
## age group 1
data.fit.ferris.def1.age1 <- data.fit.ferris.def1[age %in% fit.age.1]
data.fit.ferris.def1.age1$age_group <- as.factor("(34,45)")
data.fit.ferris.def2.age1 <- data.fit.ferris.def2[age %in% fit.age.1]
data.fit.ferris.def2.age1$age_group <- as.factor("(34,45)")

## age group 2
data.fit.ferris.def1.age2 <- data.fit.ferris.def1[age %in% fit.age.2]
data.fit.ferris.def1.age2$age_group <- as.factor("(45,50)")
data.fit.ferris.def2.age2 <- data.fit.ferris.def2[age %in% fit.age.2]
data.fit.ferris.def2.age2$age_group <- as.factor("45,50")

## age group 3
data.fit.ferris.def1.age3 <- data.fit.ferris.def1[age %in% fit.age.3]
data.fit.ferris.def1.age3$age_group <- as.factor("(50,55)")
data.fit.ferris.def2.age3 <- data.fit.ferris.def2[age %in% fit.age.3]
data.fit.ferris.def2.age3$age_group <- as.factor("(50,55)")

#----------------------------------- FF4 ---------------------------------------
## age group 1
data.ff4.ferris.def1.age1 <- data.ff4.ferris.def1[age %in% ff4.age.1]
data.ff4.ferris.def1.age1$age_group <- as.factor("(53,60)")
data.ff4.ferris.def2.age1 <- data.ff4.ferris.def2[age %in% ff4.age.1]
data.ff4.ferris.def2.age1$age_group <- as.factor("(53,60)")

## age group 2
data.ff4.ferris.def1.age2 <- data.ff4.ferris.def1[age %in% ff4.age.2]
data.ff4.ferris.def1.age2$age_group <- as.factor("(60,65)")
data.ff4.ferris.def2.age2 <- data.ff4.ferris.def2[age %in% ff4.age.2]
data.ff4.ferris.def2.age2$age_group <- as.factor("(60,65)")


## age group 3
data.ff4.ferris.def1.age3 <- data.ff4.ferris.def1[age %in% ff4.age.3]
data.ff4.ferris.def1.age3$age_group <- as.factor("(65,75)")
data.ff4.ferris.def2.age3 <- data.ff4.ferris.def2[age %in% ff4.age.3]
data.ff4.ferris.def2.age3$age_group <- as.factor("(65, 75)")
#-------------------------------------------------------------------------------
#-------------------------- PARTITON AGE & DEF  CONTINENTAL --------------------
#-------------------------------------------------------------------------------
#
#
#----------------------------------- FIT ---------------------------------------
## age group 1
data.fit.cont.def1.age1 <- data.fit.cont.def1[age %in% fit.age.1]
data.fit.cont.def1.age1$age_group <- as.factor("(34,45)")
data.fit.cont.def2.age1 <- data.fit.cont.def2[age %in% fit.age.1]
data.fit.cont.def2.age1$age_group <- as.factor("(34,45)")

## age group 2
data.fit.cont.def1.age2 <- data.fit.cont.def1[age %in% fit.age.2]
data.fit.cont.def1.age2$age_group <- as.factor("(45,50)")
data.fit.cont.def2.age2 <- data.fit.cont.def2[age %in% fit.age.2]
data.fit.cont.def2.age2$age_group <- as.factor("45,50")

## age group 3
data.fit.cont.def1.age3 <- data.fit.cont.def1[age %in% fit.age.3]
data.fit.cont.def1.age3$age_group <- as.factor("(50,55)")
data.fit.cont.def2.age3 <- data.fit.cont.def2[age %in% fit.age.3]
data.fit.cont.def2.age3$age_group <- as.factor("(50,55)")

#----------------------------------- FFF4 --------------------------------------
## age group 1
data.ff4.cont.def1.age1 <- data.ff4.cont.def1[age %in% ff4.age.1]
data.ff4.cont.def1.age1$age_group <- as.factor("(53,60)")
data.ff4.cont.def2.age1 <- data.ff4.cont.def2[age %in% ff4.age.1]
data.ff4.cont.def2.age1$age_group <- as.factor("(53,60)")

## age group 2
data.ff4.cont.def1.age2 <- data.ff4.cont.def1[age %in% ff4.age.2]
data.ff4.cont.def1.age2$age_group <- as.factor("(60,65)")
data.ff4.cont.def2.age2 <- data.ff4.cont.def2[age %in% ff4.age.2]
data.ff4.cont.def2.age2$age_group <- as.factor("(60,65)")


## age group 3
data.ff4.cont.def1.age3 <- data.ff4.cont.def1[age %in% ff4.age.3]
data.ff4.cont.def1.age3$age_group <- as.factor("(65,75)")
data.ff4.cont.def2.age3 <- data.ff4.cont.def2[age %in% ff4.age.3]
data.ff4.cont.def2.age3$age_group <- as.factor("(65, 75)")
#-------------------------------------------------------------------------------
#--------------------------------- CHECKS --------------------------------------
#-------------------------------------------------------------------------------
# FERRIS
ncol(data.fit.ferris.def1.age1)
ncol(data.fit.ferris.def1.age2)
ncol(data.fit.ferris.def1.age3)

ncol(data.fit.ferris.def2.age1)
ncol(data.fit.ferris.def2.age2)
ncol(data.fit.ferris.def2.age3) 

ncol(data.ff4.ferris.def1.age1)
ncol(data.ff4.ferris.def1.age2)
ncol(data.ff4.ferris.def1.age3)

ncol(data.ff4.ferris.def2.age1)
ncol(data.ff4.ferris.def2.age2)
ncol(data.ff4.ferris.def2.age3)

# CONTINENTAL
ncol(data.fit.cont.def1.age1)
ncol(data.fit.cont.def1.age2)
ncol(data.fit.cont.def1.age3)

ncol(data.fit.cont.def2.age1)
ncol(data.fit.cont.def2.age2)
ncol(data.fit.cont.def2.age3)

ncol(data.ff4.cont.def1.age1)
ncol(data.ff4.cont.def1.age2)
ncol(data.ff4.cont.def1.age3)

ncol(data.ff4.cont.def2.age1)
ncol(data.ff4.cont.def2.age2)
ncol(data.ff4.cont.def2.age3)
#-------------------------------------------------------------------------------
data.fit.end <- data.fit[, .SD[which.max(age)], by = Person_id] # nrow 451
data.ff4.end <- data.ff4[, .SD[which.max(age)], by = Person_id] # nrow 261
#-------------------------------------------------------------------------------
#--------------------------------- END CONDITION -------------------------------
#-------------------------------------------------------------------------------
# FERRIS
person_id.fit.ferris.end0 <-  data.fit.end[fac.to.num(ferris_score_r) == 0 | fac.to.num(ferris_score_l == 0), Person_id]
person_id.fit.ferris.end1 <-  data.fit.end[fac.to.num(ferris_score_r) == 1 | fac.to.num(ferris_score_l == 1), Person_id]
person_id.fit.ferris.end01 <- data.fit.end[(fac.to.num(ferris_score_r) %in% c(0,1)) | (fac.to.num(ferris_score_l) %in% c(0,1)), Person_id]
person_id.fit.ferris.end2 <- data.fit.end[fac.to.num(ferris_score_r) == 2 | fac.to.num(ferris_score_l == 2), Person_id]
person_id.fit.ferris.end3 <- data.fit.end[fac.to.num(ferris_score_r) == 3 | fac.to.num(ferris_score_l == 3), Person_id]

person_id.ff4.ferris.end0 <-  data.ff4.end[fac.to.num(ferris_score_r) == 0 | fac.to.num(ferris_score_l == 0), Person_id]
person_id.ff4.ferris.end1 <-  data.ff4.end[fac.to.num(ferris_score_r) == 1 | fac.to.num(ferris_score_l == 1), Person_id]
person_id.ff4.ferris.end01 <- data.ff4.end[(fac.to.num(ferris_score_r) %in% c(0,1)) | (fac.to.num(ferris_score_l) %in% c(0,1)), Person_id]
person_id.ff4.ferris.end2 <- data.ff4.end[fac.to.num(ferris_score_r) == 2 | fac.to.num(ferris_score_l == 2), Person_id]
person_id.ff4.ferris.end3 <- data.ff4.end[fac.to.num(ferris_score_r) == 3 | fac.to.num(ferris_score_l == 3), Person_id]
#-------------------------------------------------------------------------------
# Continental
person_id.fit.cont.end0 <-  data.fit.end[fac.to.num(continent_score_r) == 0 | fac.to.num(continent_score_l == 0), Person_id]
person_id.fit.cont.end1 <-  data.fit.end[fac.to.num(continent_score_r) == 1 | fac.to.num(continent_score_l == 1), Person_id]
person_id.fit.cont.end01 <- data.fit.end[(fac.to.num(continent_score_r) %in% c(0,1)) | (fac.to.num(continent_score_l) %in% c(0,1)), Person_id]
person_id.fit.cont.end2 <- data.fit.end[fac.to.num(continent_score_r) == 2 | fac.to.num(continent_score_l == 2), Person_id]
person_id.fit.cont.end3 <- data.fit.end[fac.to.num(continent_score_r) == 3 | fac.to.num(continent_score_l == 3), Person_id]

person_id.ff4.cont.end0 <-  data.ff4.end[fac.to.num(continent_score_r) == 0 | fac.to.num(continent_score_l == 0), Person_id]
person_id.ff4.cont.end1 <-  data.ff4.end[fac.to.num(continent_score_r) == 1 | fac.to.num(continent_score_l == 1), Person_id]
person_id.ff4.cont.end01 <- data.ff4.end[(fac.to.num(continent_score_r) %in% c(0,1)) | (fac.to.num(continent_score_l) %in% c(0,1)), Person_id]
person_id.ff4.cont.end2 <- data.ff4.end[fac.to.num(continent_score_r) == 2 | fac.to.num(continent_score_l == 2), Person_id]
person_id.ff4.cont.end3 <- data.ff4.end[fac.to.num(continent_score_r) == 3 | fac.to.num(continent_score_l == 3), Person_id]
