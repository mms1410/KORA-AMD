library(ggplot2)
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

rbindlist(list(
  data.fit.ferris.def1.age1,
  data.fit.ferris.def1.age2,
  data.fit.ferris.def1.age3
  
)) %>% 
  ggplot() +
  stat_count(aes(x ="", fill = age_group), position = "dodge") +
  ggtitle("FIT-Patienten pro Altersgruppe", subtitle = "Definition1") +
  ylab("") +
  xlab("")
save.plot("FIT-Def1-Atersgruppen.png")

rbindlist(list(
  data.fit.ferris.def2.age1,
  data.fit.ferris.def2.age2,
  data.fit.ferris.def2.age3
  
)) %>% 
  ggplot() +
  stat_count(aes(x ="", fill = age_group), position = "dodge") +
  ggtitle("FIT-Patienten pro Altersgruppe", subtitle = "Definition2") +
  ylab("") +
  xlab("")
save.plot("FIT-Def2-Atersgruppen.png")

 