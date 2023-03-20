library(haven)
library(dplyr)
library(stringr)
library(tidyr)
data <- read_sav(file.path(root.folder,"20230217_KORA_S4_FF4_FIT_StatLab.sav"))
columnNames <- colnames(data)
data$Person_id <- seq(1, nrow(data), 1)
data <- data[, c("Person_id", columnNames)]
checkIfGradable <- function(vec) {
  if (length(which(vec == "")) == 2) {
    "No Picture"
  } else if (length(which(vec == "NA")) == 0) {
    # wenn man nur ein auge braucht dann length(which(vec == "NA")) < 2
    "Gradable"
  } else {
    "Not Gradable"
  }
}
s4Grading <- apply(data[, c("S4_Ferris2013_OD_2", "S4_Ferris2013_OS_2")],
                   1, checkIfGradable)
ff4Grading <- apply(data[, c("FF4_Ferris2013_OD_2", "FF4_Ferris2013_OS_2")],
                    1, checkIfGradable)
fitGrading <- apply(data[, c("FIT_Ferris2013_OD_2", "FIT_Ferris2013_OS_2")],
                    1, checkIfGradable)
gradingVec <- paste(s4Grading, ff4Grading, fitGrading, sep = ", ")

## Indizes für Stichprobe
#
# Personen die für s4 und ff4 auswertbare bilder haben
s4Ff4Index <- which(gradingVec == c("Gradable, Gradable, No Picture"))
#
# personen die für s4 und fit auswertbare bilder haben aber hier kommen noch die
# hinzu die bei s4, ff4, fit anwesend waren und auswertbare bilder haben
s4FitPart1 <- which(gradingVec == c("Gradable, No Picture, Gradable"))
#
# anwesend bei s4, ff4, fit mit auswertbaren bildern, diese gehören zu s4-fit sample
s4FitPart2 <- which(gradingVec == c("Gradable, Gradable, Gradable"))
s4FitIndex <- c(s4FitPart1, s4FitPart2)

s4Ff4Sample <- data[s4Ff4Index, names(which(sapply(colnames(data),
                                                   function(x) !grepl("FIT", x) & !grepl("max", x) & !grepl("AMD", x)) == TRUE))]
s4FitSample <- data[s4FitIndex, names(which(sapply(colnames(data),
                                                   function(x) !grepl("FF4", x) & !grepl("max", x) & !grepl("AMD", x)) == TRUE))]
# 
# s4Sample1 <- s4Ff4Sample[, c(1, which(grepl("S4", colnames(s4Ff4Sample)) == TRUE))][, -2]
# ff4Sample <- s4Ff4Sample[, c(1, which(grepl("FF4",colnames(s4Ff4Sample)) == TRUE))][, -2]
# s4Sample2 <- s4FitSample[, c(1, which(grepl("S4",colnames(s4FitSample)) == TRUE))][, -2]
# fitSample <- s4FitSample[, c(1, which(grepl("FIT",colnames(s4FitSample)) == TRUE))][, -2]
# 
# subsampleList1 <- list(s4Sample1, ff4Sample)
# subsampleList1 <- lapply(subsampleList1, setNames, c("Person_id", "Sex", "Age", "S4_FF4_Age_Diff" ,"Results_Right_Eye", "Results_Left_Eye",
#                                                    "Ferris_Right_Eye", "Ferris_Left_Eye", "3_Continent_Score_Right_Eye",
#                                                    "3_Continent_Score_Left_Eye"))
# subsampleList2 <- list(s4Sample2, fitSample)
# subsampleList2 <- lapply(subsampleList2, setNames, c("Person_id", "Sex", "Age", "S4_FF4_Age_Diff" ,"Results_Right_Eye", "Results_Left_Eye",
#                                                      "Ferris_Right_Eye", "Ferris_Left_Eye", "3_Continent_Score_Right_Eye",
#                                                      "3_Continent_Score_Left_Eye"))
# s4Sample1 <- subsampleList1[[1]]
# ff4Sample <- subsampleList1[[2]]
# s4Sample2 <- subsampleList2[[1]]
# fitSample <- subsampleList2[[2]]
# 
# s4Sample1$Study <- rep("S4", nrow(s4Sample1)) 
# ff4Sample$Study <- rep("FF4", nrow(ff4Sample))
# s4Sample2$Study <- rep("S4", nrow(s4Sample2))
# fitSample$Study <- rep("FIT", nrow(fitSample))
# 
# s4Ff4Sample <- rbind(s4Sample1, ff4Sample)
# s4Ff4Sample <- s4Ff4Sample[order(s4Ff4Sample$Person_id), ]
# s4FitSample <- rbind(s4Sample2, fitSample)
# s4FitSample <- s4FitSample[order(s4FitSample$Person_id), ]
# 
# 
# 
