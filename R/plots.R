library(ggplot2)
library(patchwork)
library(magrittr)
#-------------------------------------------------------------------------------
theme_set(theme_bw())
#-------------------------------------------------------------------------------
root.folder <- rstudioapi::getSourceEditorContext()$path
root.folder <- dirname(dirname(root.folder))
data.folder <- file.path(root.folder, "data")
r.folder <- file.path(root.folder, "R")
assets.folder <- file.path(root.folder, "assets")
assets.folder.graphics <- file.path(assets.folder,"graphics")
source(file.path(r.folder, "preprocess-data.R"))
root.folder <- rstudioapi::getSourceEditorContext()$path
root.folder <- dirname(dirname(root.folder))
data.folder <- file.path(root.folder, "data")
r.folder <- file.path(root.folder, "R")
assets.folder <- file.path(root.folder, "assets")
assets.folder.graphics <- file.path(assets.folder,"graphics")
#-------------------------------------------------------------------------------
if (!dir.exists(assets.folder)) {
  dir.create(assets.folder)
  dir.create(assets.folder.graphics)
  
} else {
  if (!dir.exists(assets.folder.graphics)) {
    dir.create(assets.folder.graphics)
  }
}
save.plot <- function(plot.name) {
  ggsave(paste0(assets.folder.graphics, .Platform$file.sep, plot.name), bg = "white", dpi = 400, scale = 2.5)
}
#-------------------------------------------------------------------------------
# ferris score
#-------------------------------------------------------------------------------
# FIT
## a
plot.fit.ferris.def1.age_group.sex <-rbindlist(list(
  data.fit.ferris.def1.age1,
  data.fit.ferris.def1.age2,
  data.fit.ferris.def1.age3
)) %>% 
  ggplot() +
  stat_count(aes(x =sex, fill = age_group), position = "dodge") +
  ggtitle("FIT-Patienten pro Altersgruppe", subtitle = "Definition1 (ferris)") +
  ylab("") +
  xlab("") +
  labs(fill ="Altersgruppen")
plot.fit.ferris.def1.age_group.sex
save.plot("FIT-ferris-Def1-Atersgruppen.Geschlecht.png")

plot.fit.ferris.def2.age_group.sex <- rbindlist(list(
  data.fit.ferris.def2.age1,
  data.fit.ferris.def2.age2,
  data.fit.ferris.def2.age3
)) %>% 
  ggplot() +
  stat_count(aes(x =sex, fill = age_group), position = "dodge") +
  ggtitle("FIT-Patienten pro Altersgruppe", subtitle = "Definition2 (ferris)") +
  ylab("") +
  xlab("") +
  labs(fill ="Altersgruppen")
plot.fit.ferris.def2.age_group.sex
save.plot("FIT-ferris-Def2-Atersgruppen-Geschlecht.png")

plot.fit.ferris.def1.age_group.sex | plot.fit.ferris.def2.age_group.sex
save.plot("FIT-ferris-Altersgruppen.Geschlecht.png")


## b
plot.fit.ferris.def1.age_group <-rbindlist(list(
  data.fit.ferris.def1.age1,
  data.fit.ferris.def1.age2,
  data.fit.ferris.def1.age3
)) %>% 
  ggplot() +
  stat_count(aes(x ="", fill = age_group), position = "dodge") +
  ggtitle("FIT-Patienten pro Altersgruppe", subtitle = "Definition1 (ferris)") +
  ylab("") +
  xlab("") +
  labs(fill ="Altersgruppen")
plot.fit.ferris.def1.age_group
save.plot("FIT-ferris-Def1-Atersgruppen.png")

plot.fit.ferris.def2.age_group <-rbindlist(list(
  data.fit.ferris.def2.age1,
  data.fit.ferris.def2.age2,
  data.fit.ferris.def2.age3
)) %>% 
  ggplot() +
  stat_count(aes(x ="", fill = age_group), position = "dodge") +
  ggtitle("FIT-Patienten pro Altersgruppe", subtitle = "Definition2 (ferris)") +
  ylab("") +
  xlab("") +
  labs(fill ="Altersgruppen")
plot.fit.ferris.def2.age_group
save.plot("FIT-ferris-Def2-Atersgruppen.png")

plot.fit.ferris.def1.age_group | plot.fit.ferris.def2.age_group
save.plot("FIT-ferris-Altersgruppen.png")

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# FF4
## a
plot.ff4.ferris.def1.age_group <- rbindlist(list(
  data.ff4.ferris.def1.age1,
  data.ff4.ferris.def1.age2,
  data.ff4.ferris.def1.age3
)) %>% 
  ggplot()+
  stat_count(aes(x ="", fill = age_group), position = "dodge") +
  ggtitle("FF4-Patienten pro Altersgruppe", subtitle = "Definition1 (ferris)") +
  ylab("") +
  xlab("") +
labs(fill ="Altersgruppen")
plot.ff4.ferris.def1.age_group
save.plot("FF4-ferris-Def1-Atersgruppen.png")

plot.ff4.ferris.def2.age_group <- rbindlist(list(
  data.ff4.ferris.def2.age1,
  data.ff4.ferris.def2.age2,
  data.ff4.ferris.def2.age3
)) %>% 
  ggplot() +
  stat_count(aes(x ="", fill = age_group), position = "dodge") +
  ggtitle("FF4-Patienten pro Altersgruppe", subtitle = "Definition2 (ferris)") +
  ylab("") +
  xlab("") +
  labs(fill ="Altersgruppen")
plot.ff4.ferris.def2.age_group
save.plot("FF4-ferris-Def2-Atersgruppen.png")

plot.ff4.ferris.def1.age_group | plot.ff4.ferris.def2.age_group 
save.plot("FF4-ferris-Altersgruppen.png")


## b
plot.ff4.ferris.def1.age_group.sex <- rbindlist(list(
  data.ff4.ferris.def1.age1,
  data.ff4.ferris.def1.age2,
  data.ff4.ferris.def1.age3
)) %>% 
  ggplot() +
  stat_count(aes(x =sex, fill = age_group), position = "dodge") +
  ggtitle("FF4-Patienten pro Altersgruppe", subtitle = "Definition1 (ferris)") +
  ylab("") +
  xlab("") +
  labs(fill ="Altersgruppen")
plot.ff4.ferris.def1.age_group.sex
save.plot("FF4-ferris-Def1-Altersgruppen-Geschlecht.png")

plot.ff4.ferris.def2.age_group.sex <- rbindlist(list(
  data.ff4.ferris.def2.age1,
  data.ff4.ferris.def2.age2,
  data.ff4.ferris.def2.age3
)) %>% 
  ggplot() +
  stat_count(aes(x =sex, fill = age_group), position = "dodge") +
  ggtitle("FF4-Patienten pro Altersgruppe", subtitle = "Definition1 (ferris)") +
  ylab("") +
  xlab("") +
  labs(fill ="Altersgruppen")
plot.ff4.ferris.def2.age_group.sex
save.plot("FF4-ferris-Def2-Altersgruppen-Geschlecht.png")

plot.ff4.ferris.def1.age_group.sex | plot.ff4.ferris.def2.age_group.sex
save.plot("FF4-ferris-Altersgruppen-Geschlecht.png")
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# continental score
#-------------------------------------------------------------------------------
# FIT
#-------------------------------------------------------------------------------
# FF4