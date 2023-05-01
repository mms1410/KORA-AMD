library(data.table)
library(checkmate)
library(haven)
#-------------------------------------------------------------------------------
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

get_summary_amd_factor <- function(dtbl, cols_summary = "") {
  #'
  #'
  #'
  #'
  #' @param dtbl (data.table):
  #' @param cols_summary (chr):
  #' @param verbose (logi):
  #' @param append (logi)
  #'
  assertDataTable(dtbl)
  assertCharacter(cols_summary)
  for (col in cols_summary){
    assert(is.factor(dtbl[[col]]))
  }
  if (length(cols_summary) == 1 && cols_summary == "") {
    cols_summary = c(
      colnames(dtbl)[grepl(pattern = "Ferris", colnames(dtbl))],
      colnames(dtbl)[grepl(pattern = "Conti", colnames(dtbl))]
    )
  }
  assert(all(cols_summary %in% colnames(dtbl)))
  
  smry <- data.table()
  for (col in cols_summary) {
    vals <- dtbl[[col]]
    smry <- rbindlist(list(
      smry,
      as.data.table(t(c("variable" = col, table(vals),
                        "n" = sum(!is.na(vals)),
                        "NA" = sum(is.na(vals)),
                        "total" = length(vals))))
    ))
  }
  return(smry)
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
  fit_eyes_ferris_bl <- c("LTFerris_RE_2_sf", "LTFerris_LI_sf")
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
      measure_vars_bl <- ff4_eye_ferris_bl
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

get_incidence_tbl <- function(dtbl, amd_bl_col, amd_fu_col, split_col, digits = 2,
                              col_names = c("At_Risk", "Events", "Incidence(%)"), case_4 = "Progression") {
  #'
  #' Create incidence table by calling 'get_incidence' for each level in 'split_col'. 
  #'
  #' @param dtbl (data.table):
  #' @param amd_bl_col (chr):
  #' @param amd_fu_col (chr):
  #' @param split_col (chr):
  #' @param digits (int):
  #' @param col_names (chr):
  #' @param case_4 (chr):
  #'
  #' @return data.table: incidence numbers in data.table
  assertDataTable(dtbl)
  assertString(split_col)
  assertString(amd_bl_col)
  assertString(amd_fu_col)
  assertInt(digits)
  assert(amd_bl_col %in% colnames(dtbl))
  assert(amd_fu_col %in% colnames(dtbl))
  assert(split_col %in% colnames(dtbl))
  assert(is.factor(dtbl[[split_col]]))
  assert(setequal(levels(dtbl[[amd_bl_col]]), c("no_amd", "early_amd", "late_amd")))
  assert(setequal(levels(dtbl[[amd_fu_col]]), c("no_amd", "early_amd", "late_amd")))
  assertCharacter(col_names, len = 3, any.missing = FALSE)
  assertString(case_4)
  
  case1 <- data.table()
  case2 <- data.table()
  case3 <- data.table()
  case4 <- data.table()
  for (level in levels(dtbl[[split_col]])){
    tmp <- get_incidence(dtbl = dtbl[dtbl[[split_col]] == level],
                  amd_bl_col = amd_bl_col,
                  amd_fu_col = amd_fu_col,
                  col_names = col_names,
                  case_4 = case_4)
    case1 <- rbindlist(list(case1,tmp$case1[, group := as.factor(level)]))
    case2 <- rbindlist(list(case2,tmp$case2[, group := as.factor(level)]))
    case3 <- rbindlist(list(case3,tmp$case3[, group := as.factor(level)]))
    case4 <- rbindlist(list(case4,tmp$case4[, group := as.factor(level)]))
    
  }
  tmp <- get_incidence(dtbl,amd_bl_col, amd_fu_col, col_names = col_names, case_4 = case_4)
  case1 <- rbindlist(list(tmp[["case1"]][, group := as.factor("all")], case1))
  case2 <- rbindlist(list(tmp[["case2"]][, group := as.factor("all")], case2))
  case3 <- rbindlist(list(tmp[["case3"]][, group := as.factor("all")], case3))
  case4 <- rbindlist(list(tmp[["case4"]][, group := as.factor("all")], case4))
  
  return(list(
    "case1" = case1,
    "case2" = case2,
    "case3" = case3,
    "case4" = case4
  ))
  
}

get_incidence <- function(dtbl, amd_bl_col, amd_fu_col, digits = 2, col_names = c("At_Risk", "Events", "Incidence(%)"), case_4 = "Progression") {
  #'
  #' Create incidence table.
  #'
  #' @param dtbl (data.table):
  #' @param amd_bl_col (chr):
  #' @param amd_fu_col (chr):
  #' @param digits (int):
  #' @param col_names (chr):
  #' @param case_4 (chr):
  #'
  #' @return data.table: incidence numbers in data.table
  assertDataTable(dtbl)
  assertString(amd_bl_col)
  assertString(amd_fu_col)
  assertInt(digits)
  assert(amd_bl_col %in% colnames(dtbl))
  assert(amd_fu_col %in% colnames(dtbl))
  assert(setequal(levels(dtbl[[amd_bl_col]]), c("no_amd", "early_amd", "late_amd")))
  assert(setequal(levels(dtbl[[amd_fu_col]]), c("no_amd", "early_amd", "late_amd")))
  assertCharacter(col_names, len = 3, any.missing = FALSE)
  assertString(case_4)
  
  amd_bl <- dtbl[[amd_bl_col]]
  amd_fu <-dtbl[[amd_fu_col]]
  
  
  # Case 1
  at_risk <- nrow(dtbl[amd_bl == "no_amd"])
  events <- nrow(dtbl[(amd_bl == "no_amd" & amd_fu == "early_amd")])
  case1 <- as.data.table(t(c(
    setNames(at_risk, col_names[1]),
    setNames(events, col_names[2]),
    setNames(round(events / at_risk * 100, digits), col_names[3])
  )))
  # Case 2
  at_risk <- nrow(dtbl[amd_bl == "no_amd"])
  events <- nrow(dtbl[amd_bl == "no_amd" & amd_fu == "late_amd"])
  case2 <- as.data.table(t(c(
    setNames(at_risk, col_names[1]),
    setNames(events, col_names[2]),
    setNames(round(events / at_risk * 100, digits), col_names[3])
  )))
  # Case 3
  at_risk <- nrow(dtbl[amd_bl == "no_amd" | amd_bl == "early_amd"])
  events <- nrow(dtbl[(amd_bl == "no_amd" | amd_bl == "early_amd") & amd_fu == "late_amd"])
  case3 <- as.data.table(t(c(
    setNames(at_risk, col_names[1]),
    setNames(events, col_names[2]),
    setNames(round(events / at_risk * 100, digits), col_names[3])
  )))
  # Case 4
  at_risk <- nrow(dtbl[amd_bl == "early_amd"])
  events <- nrow(dtbl[amd_bl == "early_amd" & amd_fu == "late_amd"])
  case4 <- as.data.table(t(c(
    setNames(at_risk, col_names[1]),
    setNames(events, col_names[2]),
    setNames(round(events / at_risk * 100, digits), case_4)
  )))
  
  return(list("case1" = case1,
              "case2" = case2,
              "case3" = case3,
              "case4" = case4))
  
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

center_variables <- function(dtbl, variables){
  #'
  #' Center variables in given datatable.
  #'
  #' @param dtbl (data.table): Input data table.
  #' @param variables (chr): Variable names to be centered.
  #'
  #' @return data.table: Datatable with centered variables.
  assertDataTable(dtbl)
  assertCharacter(variables)
  assert(all(variables %in% colnames(dtbl)))
  for (variable in variables) {
    assertNumeric(dtbl[[variable]])
  }
  
  
}

tidy_gee <- function(gee_model, exponentiate = FALSE, row_names = NA, col_name = NA, col_names = NA, digits = 4) {
  #'
  #' Create tidy coefficient output for fitted gee model.
  #'
  #' @param gee_model (gee, glm): fitted gee model.
  #' @param exponentiate (logical): if TRUE coefficients will be taken to power of e.
  #' @param row_name (chr):
  #' @param col_name (chr):
  #' @param col_names (chr):
  #' @param digits (chr):
  #'
  #'
  #' @return data.table: table of coefficients.
  assertClass(gee_model, c("gee", "glm"))
  assertLogical(exponentiate)
  assertCharacter(row_names)
  assertString(col_name, na.ok = TRUE)
  assertCharacter(col_names)
  assertInt(digits)
  
  smry <- summary(gee_model)
  dtbl <- data.table(smry$coefficients, keep.rownames = TRUE)
  if (all(!is.na(col_names))) {
    assert(length(col_names) == length(colnames(dtbl)))
    colnames(dtbl) <- col_names
  }
  if (all(!is.na(row_names))) {
    assert(col_name %in% colnames(dtbl))
    assertCharacter(dtbl[[col_name]])
    assert(length(row_names) == length(dtbl[[col_name]]))
    dtbl[[col_name]] <- row_names
  }
  cols_numeric <- names(sapply(dtbl, is.numeric))[sapply(dtbl, is.numeric)]
  if (exponentiate) {
    dtbl[, (cols_numeric) := lapply(.SD, exp), .SDcols = cols_numeric]
  }
  dtbl[, (cols_numeric) := lapply(.SD, round, digits), .SDcols = cols_numeric]
  return(dtbl)
}

fit_gee_models <- function(dtbl, regressors, regressand, id_col) {
  #'
  #' Fit GEE model.
  #'
  #' @param dtbl (data.table):
  #' @param regressors (chr):
  #' @param regressand (chr):
  #' @param id_col (chr):
  #'
  #' @return
  assertDataTable(dtbl)
  assertCharacter(regressors)
  assert(all(regressors %in% colnames(dtbl)))
  assertString(regressand)
  assert(regressand %in% colnames(dtbl))
  assertString(id_col)
  assert(id_col %in% colnames(dtbl))
  # TODO: assert formula elements in colnames
  
  dtbl <- data_fit_1
  regressors <- c("ltalteru", "lcsex", "ltrauchp", "ll_hdla", "time_bl_fu")
  regressand <- "amd_status_fu"
  id_col <- "person_id"
  gee_formula <- as.formula(
    paste(regressand, paste0(regressors, collapse = " + "), sep = " ~ ")
  )
  gee(formula = amd_status_fu ~ ltalteru + lcsex + time_bl_fu + ltrauchp + ll_hdla,
      id = person_id,
      data = dtbl)
  
  model1 <- do.call("gee", list(formula = gee_formula, data = dtbl, id = "person_id"))
  
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

get_table_glm <- function(fitted_model, col_names = c("Parameter", "Prob/OR", "95%-CI"), expo = FALSE, digits = 4, intercept = FALSE){
  #'
  #'
  #' @param fitted_model (glm):
  #' @param col_names (chr):
  #' @param expo (logical):
  #' @param digits (int):
  #' @param intercept (logical):
  #'
  #' @return data.table:
  assert("glm" %in% class(fitted_model))
  assertCharacter(col_names)
  if (!all(is.na(col_names))) assert(length(col_names) == 3)
  assertLogical(expo)
  assertInt(digits)
  assertLogical(intercept)
  
  smry <- summary(fitted_model)
  if (expo) {
    cis <- round(exp(confint(fitted_model)),digits)
    coefs <- round(exp(coef(fitted_model)), digits)
  } else {
    cis <- round(confint(fitted_model), digits)
    coefs <- round(coef(fitted_model), digits)
  }
  cis <- paste0("[",cis[, 1], ",",cis[, 2], "]")
  
  dtbl <- data.table(cbind(names(coefs), coefs, cis))
  if (!all(is.na(col_names))) {
    colnames(dtbl) <- col_names
  }
  if (!intercept) {
    dtbl <- dtbl[-1,]
  }
  return(dtbl)
}
