library(knitr)
library(xtable)
library(kableExtra)
#-------------------------------------------------------------------------------
#                                 data description
#-------------------------------------------------------------------------------
#######
# FIT #
#######

# CONTINENTAL
get_amd_table(data_fit, amd_col = "LT_conti_worst_eye")
get_amd_table(data_fit, amd_col = "PT_conti_worst_eye")

# FERRIS
get_amd_table(data_fit, amd_col = "LT_ferris_worst_eye")
get_amd_table(data_fit, amd_col = "PT_ferris_worst_eye")


#######
# FF4 #
#######

# CONTINENTAL
get_amd_table(data_ff4, amd_col = "LT_conti_worst_eye")
get_amd_table(data_ff4, amd_col = "U3T_conti_worst_eye")

# FERRIS
get_amd_table(data_ff4, amd_col = "LT_ferris_worst_eye")
get_amd_table(data_ff4, amd_col = "U3T_ferris_worst_eye")


#-------------------------------------------------------------------------------
data.frame(cbind(
  get_amd_table(data_fit, amd_col = "LT_conti_worst_eye"),
  get_amd_table(data_fit, amd_col = "PT_conti_worst_eye"),
  get_amd_table(data_ff4, amd_col = "LT_conti_worst_eye"),
  get_amd_table(data_ff4, amd_col = "U3T_conti_worst_eye")
))

get_table_amd_count(data_fit_conti_long, data_ff4_conti_long)

get_table_amd_count(data_fit_conti_long,
                    data_ff4_conti_long,
                    bl_amd_col_ff4 = "amd_status_bl",
                    bl_amd_col_fit = "amd_status_bl",
                    fu_amd_col_ff4 = "amd_status_fu",
                    fu_amd_col_fit = "amd_status_fu",
                    row_names = c("Keine AMD", "Frühe AMD", "Späte AMD")
)
#-------------------------------------------------------------------------------
library(kableExtra)
grouped_table <- grouped_table_models(model_gee = model_gee_fit_conti_1,
                     model_glm =  model_glm_fit_conti_1,
                     table_caption = "Inzidenz frühe AMD (FIT)",
                     group_captions = c("Log. Regression", "GEE Modell"),
                     selection = c("ltalteru", "lcsexF", "time_bl_fu", "ltcigreg_sfcurr.", "ltcigreg_sfformer", "ll_hdla"),
                     row_names = c("Alter", "Männlich vs. Weiblich", "Zeit BL-FU", "Nichtraucher vs. Raucher",
                                   "Nichtraucher vs. Ex-Raucher", "HDL"))

plot_gee_vs_glm(grouped_table, gg_title = "", gg_xlab = "OR", gg_ylab = "", gg_caption = ""){
  #'
  #'
  #'
  #'
  
  get_str_conf_lower <- function(str_conf) {
    #'
    #'
    #'
    result <- regmatches(x = str_conf,
               gregexpr(text = str_conf, pattern = "(?<=\\[)[\\d]+.[\\d]+",
                        perl = TRUE)) %>%
             unlist()
    result <- as.numeric(result)
    return(result)
  }
  get_str_conf_upper <- function(str_conf) {
    #'
    #'
    #'
    result <- regmatches(x = str_conf,
                         gregexpr(text = str_conf,
                                  pattern = "[\\d]+.[\\d]+(?=\\])",
                                  perl = TRUE)) %>%
      unlist()
    result <- as.numeric(result)
    return(result)
  }
  
  names_params <- rownames(grouped_table)
  ci_lower_glm <- get_str_conf_lower(grouped_table$`95% KI`)
  ci_upper_glm <- get_str_conf_upper(grouped_table$`95% KI`)
  ci_lower_gee <- get_str_conf_lower(grouped_table$`95% KI (Robust)`)
  ci_upper_gee <- get_str_conf_upper(grouped_table$`95% KI (Robust)`)
  estimate_glm <- as.numeric(grouped_table[,1])
  estimate_gee <- as.numeric(grouped_table[,3])
  
  
  dtbl <- rbind(
    data.table(cbind(Variable = names_params,
                     Parameter= estimate_glm,
                     ci_lower = ci_lower_glm, ci_upper = ci_upper_glm,
                     Modell = rep(x = "GLM", times = length(names_params)))),
    data.table(cbind(Variable = names_params,
                     Parameter = estimate_gee,
                     ci_lower = ci_lower_gee, ci_upper = ci_upper_gee,
                     Modell = rep(x = "GEE", times = length(names_params))))
  )
  dtbl[,  `:=`(Parameter = as.numeric(Parameter), ci_lower = as.numeric(ci_lower), ci_upper = as.numeric(ci_upper))]
  
  ggplot(data = dtbl, mapping = aes(y = forcats::fct_inorder(f = rev(x = Variable)), col = Modell)) +
    geom_errorbarh(aes(xmin = ci_lower,
                       xmax = ci_upper,
                       y = forcats::fct_inorder(f = rev(x = Variable))),
                   linewidth = 1.01,
                   position = position_dodge2(0.6)) +
    geom_point(aes(x = Parameter,
                   y = forcats::fct_inorder(f = rev(x = Variable))),
               position = position_dodge(0.9)) +
    geom_vline(xintercept = 1,
               linetype = "dashed",
               linewidth = 1) +
    ggtitle(gg_title) +
    labs(x = gg_xlab,
         y = gg_ylab,
         caption = gg_caption) +
    scale_x_continuous(trans = "log2") 
}

gtbl <- grouped_table_models(model_gee = model_gee_ff4_conti_1,
                             model_glm =  model_glm_ff4_conti_1,
                             table_caption = "Inzidenz frühe AMD (FF4)",
                             group_captions = c("Log. Regression", "GEE Modell"),
                             selection = c("ltalteru", "lcsexF", "time_bl_fu", "ltcigreg_sfcurr.", "ltcigreg_sfformer", "ll_hdla"),
                             row_names = c("Alter", "Männlich vs. Weiblich", "Zeit BL-FU", "Nichtraucher vs. Raucher",
                                           "Nichtraucher vs. Ex-Raucher", "HDL"))
grouped_table_to_knitr(gtbl, table_title = "Risikofaktoren Inzidenz frühe AMD (FF4) I")
save_tbl(gtbl, table_name = "Models-FF-1",table_title = "Modellvergleich Inzidenz fühe AMD (FF4) I")

