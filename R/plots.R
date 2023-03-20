library(ggplot2)
library(patchwork)
library(magrittr)
#-------------------------------------------------------------------------------
theme_set(theme_bw(base_size = 20))
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

plot.fit.ferris.sex <- plot.fit.ferris.def1.age_group.sex | plot.fit.ferris.def2.age_group.sex
plot.fit.ferris.sex
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

plot.fit.ferris <- plot.fit.ferris.def1.age_group | plot.fit.ferris.def2.age_group
plot.fit.ferris
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

plot.ff4.ferris <- plot.ff4.ferris.def1.age_group | plot.ff4.ferris.def2.age_group 
plot.ff4.ferris
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
  ggtitle("FF4-Patienten pro Altersgruppe", subtitle = "Definition2 (ferris)") +
  ylab("") +
  xlab("") +
  labs(fill ="Altersgruppen")
plot.ff4.ferris.def2.age_group.sex
save.plot("FF4-ferris-Def2-Altersgruppen-Geschlecht.png")

plot.ff4.ferris.sex <- plot.ff4.ferris.def1.age_group.sex | plot.ff4.ferris.def2.age_group.sex
plot.ff4.ferris.sex
save.plot("FF4-ferris-Altersgruppen-Geschlecht.png")
#-------------------------------------------------------------------------------

plot.fit.ferris.merge <- plot.fit.ferris / plot.fit.ferris.sex
plot.fit.ferris.merge
save.plot("FIT-ferris-Altersgruppen-merge.png")

plot.ff4.merge <- plot.ff4.ferris / plot.ff4.ferris.sex
plot.ff4.merge
save.plot("FF4-ferris-Altersgruppen-merge.png")


#-------------------------------------------------------------------------------
# scratch_sven needs to be loaded
plot.ff4.r <- rbindlist(list(
  data.ff4
)) %>% 
  ggplot() +
  stat_count(aes(x = sex, fill = ferris_score_r), position = "dodge") +
  ylab("") +
  xlab("") +
  ggtitle("FF4 ferris-score (R)") + 
  labs(fill ="ferris (R)")
plot.ff4.r
save.plot("ff4-ferris-R.png")

plot.ff4.l <- rbindlist(list(
  data.fit
)) %>% 
  ggplot() +
  stat_count(aes(x = sex, fill = ferris_score_l), position = "dodge") +
  ylab("") +
  xlab("") +
  ggtitle("FF4 ferris-score (L)") + 
  labs(fill ="ferris (L)")
plot.ff4.l
save.plot("ff4-ferris-L.png")

plot.ff4.ferris.rl <- plot.ff4.r | plot.ff4.l
plot.ff4.ferris.rl
save.plot("ff4-ferris-rl.png")

plot.fit.r <- rbindlist(list(
  data.fit
)) %>% 
  ggplot() +
  stat_count(aes(x = sex, fill = ferris_score_r), position = "dodge") +
  ylab("") +
  xlab("") +
  ggtitle("FIT ferris-score (R)") + 
  labs(fill ="ferris (R)")
plot.fit.r
save.plot("fit-ferris-R.png")
  
plot.fit.l <- rbindlist(list(
  data.fit
)) %>% 
  ggplot() +
  stat_count(aes(x = sex, fill = ferris_score_l), position = "dodge") +
  ylab("") +
  xlab("") +
  ggtitle("FIT ferris-score (L)") + 
  labs(fill ="ferris (L)")
plot.fit.l
save.plot("fit-ferris-L.png")

plot.fit.rl <- plot.fit.r | plot.fit.l
plot.fit.rl
save.plot("fit-ferris-rl.png")

plot.ferris.rl.merge <- plot.fit.rl / plot.ff4.ferris.rl
plot.ferris.rl.merge
save.plot("ferris-rl-merge.png")
#-------------------------------------------------------------------------------
plot.ff4.cont.r <- rbindlist(list(
  data.ff4
)) %>% 
  ggplot() +
  stat_count(aes(x = sex, fill = continent_score_r), position = "dodge") +
  ylab("") +
  xlab("") +
  ggtitle("FF4 continent-score (R)") + 
  labs(fill ="continental (R)")
plot.ff4.cont.r
save.plot("ff4-cont-R.png")

plot.ff4.cont.l <- rbindlist(list(
  data.fit
)) %>% 
  ggplot() +
  stat_count(aes(x = sex, fill = continent_score_l), position = "dodge") +
  ylab("") +
  xlab("") +
  ggtitle("FF4 continent-score (L)") + 
  labs(fill ="continental (L)")
plot.ff4.cont.l
save.plot("ff4-continental-L.png")

plot.ff4.cont.rl <- plot.ff4.cont.r | plot.ff4.cont.l
plot.ff4.cont.rl
save.plot("ff4-ferris-rl.png")
#
#

plot.fit.cont.r <- rbindlist(list(
  data.ff4
)) %>% 
  ggplot() +
  stat_count(aes(x = sex, fill = continent_score_r), position = "dodge") +
  ylab("") +
  xlab("") +
  ggtitle("FIT continent-score (R)") + 
  labs(fill ="continental (R)")
plot.fit.cont.r
save.plot("ff4-cont-R.png")

plot.fit.cont.l <- rbindlist(list(
  data.fit
)) %>% 
  ggplot() +
  stat_count(aes(x = sex, fill = continent_score_l), position = "dodge") +
  ylab("") +
  xlab("") +
  ggtitle("FIT continent-score (L)") + 
  labs(fill ="continental (L)")
plot.fit.cont.l
save.plot("fit-continental-L.png")

plot.fit.cont.rl <- plot.fit.cont.r | plot.fit.cont.l
plot.fit.cont.rl
save.plot("fit-ferris-rl.png")

plot.cont.rl.merge <- plot.fit.cont.rl / plot.ff4.cont.rl
plot.cont.rl.merge
save.plot("cont-rl-merge.png")



