library(data.table)
library(checkmate)
library(haven)
library(ggplot2)
library(scales)
theme_set(theme_linedraw(base_size = 12))
options(ggplot2.discrete.colour= c("red", "blue"))
#-------------------------------------------------------------------------------
#                              Data Wrangling
#-------------------------------------------------------------------------------
get_summary <- function(dtbl, bl_col,
                        fu_col,
                        amd_name = "AMD Status",
                        bl_name = "Baseline",
                        fu_name = "Follow-Up",
                        replace_no_amd = "Keine AMD",
                        replace_early_amd = "Fruehe AMD",
                        replace_late_amd = "Spaete AMD",
                        digits = 3) {
  #'
  #'
  #'
  assert(bl_col %in% colnames(dtbl))
  assert(fu_col %in% colnames(dtbl))
  
  amd_levels <-  dtbl[[fu_col]] %>%  levels()
  
  fu <- setorder(dtbl[, .(count = .N), by = fu_col])[, count]
  bl <- setorder(dtbl[, .(count = .N), by = bl_col])[, count]
  bl <- c(bl, rep(0, length(fu) - length(bl)))
  
  names(bl) <- amd_levels
  names(fu) <- amd_levels
  
  out <- data.table(cbind(bl, fu), keep.rownames = TRUE)
  fu_percent <- round(out$fu / sum(out$fu), digits)
  out[, rn := gsub(rn, pattern = "no_amd", replacement = replace_no_amd)]
  out[, rn := gsub(rn, pattern = "early_amd", replacement = replace_early_amd)]
  out[, rn := gsub(rn, pattern = "late_amd", replacement = replace_late_amd)]
  out$fu_percentage <- fu_percent
  colnames(out) <- c(amd_name, bl_name, fu_name, paste0(fu_name, "(%)"))
  return(out)
}
rename_ferris_scores <- function(dtbl,
                                 col_names = c("LTFerris_LI_2_sf",
                                               "LTFerris_RE_2_sf",
                                               "LT_ferris_worst_eye",
                                               "U3TFerris_LI_2_sf",
                                               "U3TFerris_RE_2_sf",
                                               "U3T_ferris_worst_eye",
                                               "PTFerris_LI_2_sf",
                                               "PTFerris_RE_2_sf",
                                               "PT_ferris_worst_eye")) {
  assertDataTable(dtbl)
  assertCharacter(col_names)
  assert(all(col_names %in% colnames(dtbl)))
  
  
  no_amd <- c("0", "1")
  early_amd <- c("2", "3")
  late_amd <- c("4")
  rename_ferris <- function(column) {
    no_amd <- no_amd[no_amd %in% column]
    early_amd <- early_amd[early_amd %in% column]
    late_amd <- late_amd[late_amd %in% column]
    fct_collapse(column, 
                 no_amd = no_amd,
                 early_amd = early_amd,
                 late_amd = late_amd
    )
  }
  dtbl[, c(col_names) := lapply(.SD, rename_ferris), .SDcols = col_names]
  
  return(dtbl)
}

rename_conti_scores <- function(dtbl,
                                col_names = c("LTConti_LI_2_sf",
                                              "LTConti_RE_2_sf",
                                              "LT_conti_worst_eye",
                                              "U3TConti_LI_2_sf",
                                              "U3TConti_RE_2_sf",
                                              "U3T_conti_worst_eye",
                                              "PTConti_LI_2_sf",
                                              "PTConti_RE_2_sf",
                                              "PT_conti_worst_eye")) {
  assertDataTable(dtbl)
  assertCharacter(col_names)
  assert(all(col_names %in% colnames(dtbl)))
  
  no_amd <- "0"
  early_amd <- c("1", "2", "3")
  late_amd <- "4"
  rename_conti <- function(column) {
    fct_collapse(column, 
                 no_amd = no_amd[no_amd %in% column],
                 early_amd = early_amd[early_amd %in% column],
                 late_amd = late_amd[late_amd %in% column]
    ) 
  }
  dtbl[, c(col_names) := lapply(.SD, rename_conti), .SDcols = col_names]
  
  return(dtbl)
}

factor_pmax <- function(x,y) {
  #'
  #'
  #'
  assertFactor(x)
  assertFactor(y)
  
  x <- as.numeric(as.character(x))
  y <- as.numeric(as.character(y))
  result <- pmax(x,y, na.rm = TRUE)
  as.factor(result)
}
collapse_conti_score <- function(dtbl,
                                 col_names = c("LTConti_LI_2_sf",
                                               "LTConti_RE_2_sf",
                                               "U3TConti_LI_2_sf",
                                               "U3TConti_RE_2_sf",
                                               "PTConti_LI_2_sf",
                                               "PTConti_RE_2_sf")) {
  #'
  #'
  #'
  #'
  #'
  assertDataTable(dtbl)
  assertCharacter(col_names)
  assert(all(col_names %in% colnames(dtbl)))
  
  no_amd <- c("0")
  early_amd <- c("1", "2", "3")
  late_amd <- c("4")
  conti_collapse <- function(column) {
    # not all levels may be observed.
    no_amd <- no_amd[no_amd %in% column]
    early_amd <- early_amd[early_amd %in% column]
    late_amd <- late_amd[late_amd %in% column]
    fct_collapse(column, "0" = no_amd, "1" = early_amd, "4" = late_amd)
  }
  dtbl[, c(col_names) := lapply(.SD, conti_collapse), .SDcols = col_names]
  
  return(dtbl)
}
collapse_ferris_score <- function(dtbl,
                                  col_names = c("LTFerris_LI_2_sf",
                                                "LTFerris_RE_2_sf",
                                                "U3TFerris_LI_2_sf",
                                                "U3TFerris_RE_2_sf",
                                                "PTFerris_LI_2_sf",
                                                "PTFerris_RE_2_sf")) {
  #'
  #'
  #'
  assertDataTable(dtbl)
  assertCharacter(col_names)
  assert(all(col_names %in% colnames(dtbl)))
  
  no_amd <- c("0", "1")
  early_amd <- c("2", "3")
  late_amd <- c("4")
  ferris_collapse <- function(column) {
    # not all levels may be observed.
    no_amd <- no_amd[no_amd %in% column]
    early_amd <- early_amd[early_amd %in% column]
    late_amd <- late_amd[late_amd %in% column]
    fct_collapse(column, "0" = no_amd, "1" = early_amd, "4" = late_amd)
  }
  dtbl[, c(col_names) := lapply(.SD, ferris_collapse), .SDcols = col_names]
  
  return(dtbl)
}
set_age_groups <- function(dtbl, named.list.ff4, named.list.fit, col.age = "ltalteru") {
  #'
  #' Set age groups for FIT and FF4 patients.
  #' 
  #' Based on two named lists for FIT and FF4 this functions creates age bins
  #' for each group in a new column called 'age_group_ff4' and 'age_group_fit'.
  #' Is is assumed that 'named.list.fit' and 'named.list.ff4' are lists with
  #' list entry's name indicating the name of the corresponding factor variable.
  #' Each list item contains a sequence of integer numbers associated with this
  #' specific age group.
  #'
  #'
  #' @param dtbl (data.table):
  #' @param col.age (chr):
  #' @param named.list.ff4 (chr):
  #' @param named.list.fit (chr):
  #'
  #' @return data.table:
  assertDataTable(dtbl)
  assert(col.age %in% colnames(dtbl))
  assertList(named.list.ff4)
  assertList(named.list.fit)
  
  for (groupname in names(named.list.ff4)) {
    dtbl[dtbl[[col.age]] %in% named.list.ff4[[groupname]], age_group_ff4 := as.factor(groupname)]
  }
  for (groupname in names(named.list.fit)) {
    dtbl[dtbl[[col.age]] %in% named.list.fit[[groupname]], age_group_fit := as.factor(groupname)]
  }
  
  return(dtbl)
}

set_groups <- function(dtbl, to.group, ordered = TRUE) {
  #'
  #' Set groups of analysis.
  #' 
  #' note:
  #' Ferris:
  #'     0: no AMD no apparent change
  #'     1: no AMD normal aging change
  #'     2: early AMD
  #'     3: Intermediate AMD
  #'     4: Late AMD
  #'     NA
  #' Continental:
  #'     0: no AMD
  #'     1: mild early
  #'     2: moderate early
  #'     3: severe early
  #'     4: late AMD
  #'     NA
  #'
  #' @param dtbl (data.tabe):
  #' @param to.group (chr):
  #' @param ordered (logical):
  #'
  #' @return data.table:
  assert(all(names(to.group) %in% colnames(dtbl)))
  
  for (groupname in names(to.group)) {
    entry <- to.group[[groupname]]
    col <- dtbl[[groupname]]
    col.levels <-  levels(col)
    assert(all(col.levels %in% entry))
    col <- fct_recode(col, !!!entry)
    col <- factor(col, ordered = ordered)
    dtbl[[groupname]] <- col
  }
  
  return(dtbl)
}
subset_simplify_factor <- function(dtbl, score_bl = "LT_conti_worst_eye", score_bl_levels, score_fu, score_fu_levels) {
  #'
  #' Create subset of datatable based on target columns for BL and FU (score_<..>)and its corresponding values (score_<..>_levels).
  #'
  #' @param dtbl (data.table): data set.
  #' @param score_bl (chr): Column name with considered score.
  #' @param score_bl_levels (chr): Indicating selected amd levels. If named factor levels can be reordered.
  #' @param score_fu (chr): Column name with considered score.
  #' @param score_fu_levels (chr): Indicating selected amd levels. If named factor levels can be reordered.
  #'
  #' @return data.table: subset data table.
  assertDataTable(dtbl)
  assert(all(score_bl %in% colnames(dtbl)))
  assert(all(score_fu %in% colnames(dtbl)))
  assertFactor(dtbl[[score_bl]])
  assertFactor(dtbl[[score_fu]])
  assert(all(score_bl_levels %in% levels(dtbl[[score_bl]])))
  assert(all(score_fu_levels %in% levels(dtbl[[score_fu]])))
  
  
  ## recode (if applicable)
  if (test_named(score_bl_levels)) {
    dtbl[[score_bl]] <- fct_recode(dtbl[[score_bl]], !!!score_bl_levels)
    score_bl_levels <- unique(names(score_bl_levels))
  }
  if (test_named(score_fu_levels)) {
    dtbl[[score_fu]] <- fct_recode(dtbl[[score_fu]], !!!score_fu_levels)
    score_fu_levels <- unique(names(score_fu_levels))
  }
  ## subset
  dtbl <- dtbl[dtbl[[score_bl]] %in% score_bl_levels]
  dtbl <- dtbl[dtbl[[score_fu]] %in% score_fu_levels]
  ## simplify factor levels
  dtbl[[score_bl]] <- fct_drop(dtbl[[score_bl]])
  dtbl[[score_fu]] <- fct_drop(dtbl[[score_fu]])
  
  return(dtbl)
}


clear_factors <- function(dtbl) {
  #'
  #' Drop unused factor columns.
  #'
  #' @param dtbl (data.table): data
  #' 
  #' @return data.table: data
  assertDataTable(dtbl)
  factor_columns <- names(sapply(dtbl, is.factor))[sapply(dtbl, is.factor) == TRUE]
  dtbl[, (factor_columns) := lapply(.SD, fct_drop), .SDcols = factor_columns]
  return(dtbl)
}
subset_data <- function(dtbl, data.dictionary, age.groups.fit, age.groups.ff4) {
  #'
  #' Preprocess dtbl.
  #'
  #'
  #' This function assumes user has already defined a data table containing
  #' column names of variables to be selected ('variable_name') and  a column
  #' indicating the corresponding column type 'variable_type'.
  #'
  #'
  #' @param dtbl (data.table): data table object to be preprocessed
  #' @param data.dictionary (data.table): data dictionary
  #' @param age.group.fit (list):
  #' @param age.group.ff4 (list):
  #'
  #' @return data.table: preprocessed data.table object
  assertDataTable(dtbl)
  assertDataTable(data.dictionary, any.missing = FALSE)
  assert(all(c("variable_name", "variable_type") %in% colnames(data.dictionary)))
  assert(all(data.dictionary[, variable_name] %in% colnames(dtbl)))
  assert(length(data.dictionary[, variable_name]) == length(unique(data.dictionary[, variable_name])))
  assert(all(data.dictionary[, variable_type] %in% c("factor", "numeric")))
  
  select <- data.dictionary[, variable_name]
  variables.factor <- data.dictionary[variable_type == "factor", variable_name]
  variables.numeric <- data.dictionary[variable_type == "numeric", variable_name]
  
  dtbl[, (variables.factor) := lapply(.SD, as.factor), .SDcols = variables.factor]
  dtbl[, (variables.numeric) := lapply(.SD, as.numeric), .SDcols = variables.numeric]
  dtbl <- zap_formats(dtbl[, ..select])
  
  dtbl <- set_age_groups(dtbl,
                         named.list.ff4 = age.groups.ff4,
                         named.list.fit = age.groups.fit
                         )
  
  return(dtbl)
}


wide_to_long <- function(dtbl, study, score) {
  #'
  #' Transform dtbl from wide format into long format.
  #'
  #' @dtbl (data.table): data
  #' @study (chr): fit or ff4
  #' @score (chr): ferris or continental
  #'
  #' @return data.table
  assertDataTable(dtbl)
  assertChoice(study, c("fit", "ff4"))
  assertChoice(score, c("ferris", "continental"))
  
  # AMD at follow up
  fit_eyes_ferris_fu <- c("PTFerris_RE_2_sf", "PTFerris_LI_2_sf")
  fit_eyes_conti_fu <- c("PTConti_RE_2_sf", "PTConti_LI_2_sf")
  ff4_eyes_ferris_fu <- c("U3TFerris_RE_2_sf", "U3TFerris_LI_2_sf")
  ff4_eyes_conti_fu <- c("U3TConti_RE_2_sf", "U3TConti_LI_2_sf")
  
  # AMD at baseline (same name for ff4 and fit)
  fit_eyes_ferris_bl <- c("LTFerris_RE_2_sf", "LTFerris_LI_2_sf")
  ff4_eyes_ferris_bl <- fit_eyes_ferris_bl
  fit_eyes_conti_bl <- c("LTConti_RE_2_sf", "LTConti_LI_2_sf")
  ff4_eyes_conti_bl <- fit_eyes_conti_bl
  
  variable_name_bl <- "eye_score_name_bl"
  variable_name_fu <- "eye_score_name_fu"
  value_name_bl <- "amd_status_bl"
  value_name_fu <- "amd_status_fu"
  
  if (study == "fit") {
    if (score == "ferris") {
      measure_vars_fu <- fit_eyes_ferris_fu
      measure_vars_bl <- fit_eyes_ferris_bl
    } else {
      measure_vars_fu <-  fit_eyes_conti_fu
      measure_vars_bl <- fit_eyes_conti_bl
    }
  } else {
    if (score == "ferris") {
      measure_vars_fu <- ff4_eyes_ferris_fu
      measure_vars_bl <- ff4_eyes_ferris_bl
    } else {
      measure_vars_fu <- ff4_eyes_conti_fu
      measure_vars_bl <- ff4_eyes_conti_bl
    }
  }
  assert(all(measure_vars_fu %in% colnames(dtbl)))
  assert(all(measure_vars_bl %in% colnames(dtbl)))
  
  ## melt baseline
  id_vars <- colnames(dtbl)[!(colnames(dtbl) %in% measure_vars_bl)]
  dtbl <- melt(data = dtbl,
               id.vars = id_vars,
               measure.vars = measure_vars_bl,
               value.name = value_name_bl,
               variable.name = variable_name_bl,
               value.factor = TRUE)
  ## melt follow up
  id_vars <- colnames(dtbl)[!(colnames(dtbl) %in% measure_vars_fu)]
  dtbl <- melt(data = dtbl,
               id.vars = id_vars,
               measure.vars = measure_vars_fu,
               value.name = value_name_fu,
               variable.name = variable_name_fu,
               value.factor = TRUE)
  ## remove worst eye results
  to_remove <- colnames(dtbl)[grep(pattern = "worst", x = colnames(dtbl))]
  dtbl[, (to_remove) := NULL]

  ## 2 consecutive melts created all 4 combinations for R/L eye at BL/FU
  ## retain only RR LL combination
  ## remove score not used in melt
  if (score == "continental") {
    to_remove <- colnames(dtbl)[grep(pattern = "ferris", x = colnames(dtbl), ignore.case = T)]
    dtbl[[variable_name_bl]] <- dtbl[[variable_name_bl]] %>% fct_collapse(R = "LTConti_RE_2_sf", L = "LTConti_LI_2_sf")
    if (study == "fit") {
      dtbl[[variable_name_fu]] <- dtbl[[variable_name_fu]] %>% fct_collapse(R = "PTConti_RE_2_sf", L = "PTConti_LI_2_sf")
    } else {
      dtbl[[variable_name_fu]] <- dtbl[[variable_name_fu]] %>% fct_collapse(R = "U3TConti_RE_2_sf", L = "U3TConti_LI_2_sf")
    }
  } else {
    to_remove <- colnames(dtbl)[grep(pattern = "conti", x = colnames(dtbl), ignore.case = T)]
    dtbl[[variable_name_bl]] <- dtbl[[variable_name_bl]] %>% fct_collapse(R = "LTFerris_RE_2_sf", L = "LTFerris_LI_2_sf")
    if (study == "fit") {
      dtbl[[variable_name_fu]] <- dtbl[[variable_name_fu]] %>% fct_collapse(R = "PTFerris_RE_2_sf", L = "PTFerris_LI_2_sf")
    } else {
      dtbl[[variable_name_fu]] <- dtbl[[variable_name_fu]] %>% fct_collapse(R = "U3TFerris_RE_2_sf", L = "U3TFerris_LI_2_sf") 
    }
  }
  dtbl <- dtbl[dtbl[[variable_name_fu]] == dtbl[[variable_name_bl]]]
  to_remove <- c(to_remove, variable_name_fu)
  dtbl[, (to_remove) := NULL]
  return(dtbl)
}
#-------------------------------------------------------------------------------
#                              
#-------------------------------------------------------------------------------
confint.gee <- function(model_gee, level = 0.05, robust = TRUE, digits = 2,
                        selection = c("ltalteru", "lcsexF", "time_bl_fu", "ltcigreg_sfcurr.", "ltcigreg_sfformer", "ll_hdla")) {
  #'
  #' Compute confidence intervals for a GEE Model.
  #' 
  #' @model_gee (gee): Gee model.
  #' @level (numeric): Significance level, by default 0.95.
  #' @robust (logical): If robust SE should be used or not.
  #'
  assert("gee" %in% class(model_gee))
  assertNumeric(level, lower = 0, upper = 1)
  assertLogical(robust)
  assert(all(selection %in% names(coef(model_gee))))

  
  ## compute confint: param +/- c * se
  critical_value <- qnorm(1 - level/2)
  
  if (robust) {
    se <- sqrt(diag(model_gee$robust.variance))
  } else {
    se <- sqrt(diag(model_gee$naive.variance))
  }
  se <- se[selection]
  coefs <- model_gee$coefficients
  coefs <- coefs[selection]
  
  confs <- coefs + t(c(-1,1) %o% se * critical_value)
  
  colnames(confs) <- c(paste0(level / 2 * 100, " %"), paste0((1 - level / 2) * 100, " %"))
  
  coefs <- round(coefs, digits)
  confs <- round(confs, digits)
  
  return(confs)
}
#-------------------------------------------------------------------------------
#                              Graphics
#-------------------------------------------------------------------------------

plot_gee_vs_glm <- function(grouped_table, gg_title = "", gg_xlab = "OR", gg_ylab = "", gg_caption = "", var_ticks = NA){
  #'
  #'
  #'
  #'
  
  get_str_conf_lower <- function(str_conf) {
    #'
    #'
    #'
    result <- regmatches(x = str_conf,
                         gregexpr(text = str_conf, pattern = "(?<=\\[)[\\d]+[.]{0,1}[\\d]+",
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
                                  pattern = "[\\d]*.[\\d]*(?=\\])",
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
  
  assert(
  estimate_glm %>% length() == estimate_gee %>% length() &
  ci_lower_glm %>% length() == ci_lower_gee %>%  length() &
  names_params %>% length() == estimate_gee %>% length() &
  names_params %>% length() == ci_lower_gee %>%  length() &
  names_params %>% length() == ci_upper_gee %>%  length()
  )
  
  
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
  
  tcks <- c(2 ** -4, 2 ** -3, 2 ** -2, 2 ** -1, 2 ** 0, 2 ** 1, 2 ** 2, 2 ** 3, 2 ** 4)
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
    scale_x_continuous(trans = "log2",
                       labels = scales::label_number(accuracy = 0.01),
                       breaks = tcks,
                       limits = c(min(tcks) -0.01, max(tcks) + 0.01))
}
#-------------------------------------------------------------------------------
#                              Tables
#-------------------------------------------------------------------------------
summary_table_gee <- function(model_gee, exponentiate = TRUE, age_col = "ltalteru", age_multiplier = 10, digits = 2,
                              table_names = c("Parameter", "95% KI (Naive)", "95% KI (Robust)"),
                              selection) {
  #'
  #'
  #'
  assert("gee" %in% class(model_gee))
  assertLogical(exponentiate)
  assertCharacter(age_col)
  assert(age_col %in% names(model_gee$coefficients))
  assertNumeric(age_multiplier)
  assertNumeric(digits)
  assertCharacter(table_names)
  assert(length(table_names) == 3)
  
  confs_n <- confint(model_gee, selection = selection)
  confs_r <- confint(model_gee, robust = FALSE, selection = selection)
  coefs <- model_gee$coefficients[selection]
  
  ## multiply age
  coefs[age_col] <- coefs[age_col] * age_multiplier
  confs_n[age_col, ] <- confs_n[age_col, ] * age_multiplier
  confs_r[age_col, ] <- confs_r[age_col, ] * age_multiplier
  
  if(exponentiate) {
    coefs <- exp(coefs)
    confs_n <- exp(confs_n)
    confs_r <- exp(confs_r)
  }
  
  ## create table
  coefs <- round(coefs, digits)
  confs_n <- round(confs_n, digits)
  confs_r <- round(confs_r, digits)
  
  confs_n <- paste0("[", format(confs_n[, 1], nsmall = digits), ", ", format(confs_n[,2], nsmall = digits), "]")
  confs_r <- paste0("[", format(confs_r[, 1], nsmall = digits), ", ", format(confs_r[, 2], nsmall = digits), "]")
  tbl <- data.frame(cbind(coefs, confs_n, confs_r))
  colnames(tbl) <- table_names
  
  return(tbl)
}

summary_table_glm <- function(model_glm, exponentiate = TRUE, age_col = "ltalteru", age_multiplier = 10, digits = 2,
                              selection,
                              table_names = c("Parameter", "95% KI")) {
  #'
  #'
  #'
  #'
  assert("glm" %in% class(model_glm))
  assertLogical(exponentiate)
  assertCharacter(age_col)
  assert(age_col %in% names(model_glm$coefficients))
  assertNumeric(age_multiplier)
  assertNumeric(digits)
  assertCharacter(table_names)
  assert(length(table_names) == 2)
  
  coefs <- model_glm$coefficients[selection]
  ## supress "Profiling ...." message
  suppressMessages(
    confs <- confint(model_glm)[selection, ]
    )
  ## multiply age
  confs[age_col, ] <- confs[age_col, ]  * age_multiplier
  coefs[age_col] <- coefs[age_col] * age_multiplier
  if(exponentiate){
    confs <- exp(confs)
    coefs <- exp(coefs)
  }
  coefs <- round(coefs, digits)
  confs <- round(confs, digits)
  confs <- paste0("[", format(confs[, 1], nsmall = digits), ", ", format(confs[, 2], nsmall = digits), "]")
  
  tbl <- data.frame(cbind(coefs, confs))
  colnames(tbl) <- table_names
  
  return(tbl)
}

check_model_call <- function(model, formula_check = "ltalteru + lcsex + time_bl_fu + ltcigreg_sf + ll_hdla + time_bl_fu") {
  call_formula <- as.character(model$call)[2]
  assert(
    grepl(pattern = formula_check,
          x = call_formula, fixed = TRUE)
  )
}

get_amd_table <- function(dtbl, amd_col, no_amd_name = "no_amd", early_amd_name = "early_amd", late_amd_name = "late_amd") {
  #'
  #'
  #'
  assertDataTable(dtbl)
  assert(amd_col %in% colnames(dtbl))
  
  cols_dtbl <- colnames(dtbl)
  values <- dtbl[[amd_col]]

  
  no_amd <- sum(values == no_amd_name, na.rm = TRUE)
  early_amd <- sum(values == early_amd_name, na.rm = TRUE)
  late_amd <- sum(values == late_amd_name, na.rm = TRUE)
  
  result <- c(no_amd, early_amd, late_amd)
  names(result) <- c(no_amd_name, early_amd_name, late_amd_name)
  return(result)
}

get_table_amd_count <- function(data_fit, data_ff4,
                                bl_amd_col_ff4 = "LT_conti_worst_eye",
                                bl_amd_col_fit = "LT_conti_worst_eye",
                                fu_amd_col_ff4 = "U3T_conti_worst_eye",
                                fu_amd_col_fit = "PT_conti_worst_eye",
                                row_names = c("Keine AMD", "Frühe AMD", "Späte AMD")) {
  #'
  #'
  #'
  #'
  dframe <- data.frame(cbind(
    get_amd_table(data_fit, amd_col = bl_amd_col_fit),
    get_amd_table(data_fit, amd_col = fu_amd_col_fit),
    get_amd_table(data_ff4, amd_col = bl_amd_col_ff4),
    get_amd_table(data_ff4, amd_col = fu_amd_col_ff4)
  ))
  colnames(dframe) <- c("BL", "FU", "BL", "FU")
  rownames(dframe) <- row_names
  return(dframe)
}
grouped_table_models <- function(model_gee, model_glm, table_caption, group_captions, row_names = "",
                                 selection = c("ltalteru", "lcsexF", "time_bl_fu", "ltcigreg_sfcurr.", "ltcigreg_sfformer", "ll_hdla")) {
  #'
  #'
  #'
  #'
  assert("glm" %in% class(model_glm))
  assert("gee" %in% class(model_gee))
  assertString(table_caption)
  assert(length(group_captions) == 2)
  
  assert(all(selection %in% names(coef(model_gee))))
  assert(all(selection %in% names(coef(model_glm))))
  
  table_gee <- summary_table_gee(model_gee, selection = selection)
  table_glm <- summary_table_glm(model_glm, selection = selection)
  
  dframe <- cbind(table_glm, table_gee)
  
  group_caption_sections <- c(1, ncol(table_glm), ncol(table_gee))
  names(group_caption_sections) <- c(" ", group_captions)
  
  if (!all(row_names == "")) {
    assert(length(row_names) == nrow(dframe))
    rownames(dframe) <- row_names
  }
  
  return(dframe)
}


gg_save <- function(plot_name) {
  #'
  #'
  #'
  assert(dir.exists(assets_folder))
  ggsave(filename = file.path(assets_folder, paste0(plot_name, ".pdf")),
         device = "pdf")
}
library(kableExtra)
Sys.setenv(OPENSSL_CONF="/dev/null")
grouped_table_to_knitr <- function(grouped_datatable, n_glm = 2, n_gee = 3,
                                   group_captions = c("GLM Modell", "GEE Modell"),
                                   table_caption = "", table_title = "") {
  #'
  #'
  
  
  group_caption_sections <- c(1, n_glm, n_gee)
  names(group_caption_sections) <- c(" ", group_captions)
  
  header <- c(1,(n_glm + n_gee))
  names(header) <- c(" ", table_title)
  
  kout <- kable(grouped_datatable, longtable = T, booktabs = T, caption = table_caption) %>% 
    add_header_above(group_caption_sections) 
  
  if (!table_title == "") {
    kout <- kout %>% add_header_above(header)
  }
  kout
  
}
library(magick)
save_tbl <- function(tble, table_name, table_title = "") {
  #'
  #'
  #'
  assert(dir.exists(assets_folder))
  tble <- grouped_table_to_knitr(grouped_datatable = tble, table_title = table_title)
  tble <- tble %>% kable_styling(latex_options = c("striped"), font_size = 12)
  kableExtra::save_kable(tble, file.path(assets_folder, paste0(table_name, ".pdf")))
}
#-------------------------------------------------------------------------------