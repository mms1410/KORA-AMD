library(ggplot2)
#-------------------------------------------------------------------------------
theme_set(theme_linedraw())
#-------------------------------------------------------------------------------
plot_odds(models = list(model_glm_ff4_1, model_glm_fit_1),
          group_names = c("KORA-FF4", "KORA-FIT"),
          names_covariates = c("Age BL \n [per 10 years]",
                               "Sex \n [fem. vs. male]",
                               "Time BL-FU \n [per year]",
                               "Smoking \n [curr. vs. never]",
                               "Smoking \n [former vs. never]",
                               "HDL \n [per 10 mg/dl]"),
          plot_title = "Early AMD")

plot_odds(models = list(model_glm_ff4_2, model_glm_fit_2))