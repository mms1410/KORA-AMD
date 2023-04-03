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
  #' @returns data.table: preprocessed data.table object
  
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
  #'
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

split_smoker <- function(dtbl) {
  #'
  #' Split smoking column with levels non_smoker, former_smoker and active_smoker
  #' into two columns: non_smoker vs. active smoker and non_smoker vs. former_smoker
  #'
  
  # TODO
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
  
  fit_eyes_ferris <- c("PTFerris_RE_2_sf", "PTFerris_LI_2_sf")
  fit_eyes_conti <- c("PTConti_RE_2_sf", "PTConti_LI_2_sf")
  ff4_eyes_ferris <- c("U3TFerris_RE_2_sf", "U3TFerris_LI_2_sf")
  ff4_eyes_conti <- c("U3TConti_RE_2_sf", "U3TConti_LI_2_sf")
  
  if (study == "fit") {
    value_name <- "PT_amd_status"
    variable_name <- "PT_eye_score_name"
    if (score == "ferris") {
      measure_vars <- fit_eyes_ferris
      assert(all(measure_vars %in% colnames(dtbl)))
    } else {
      measure_vars <-  fit_eyes_conti
      assert(all(measure_vars %in% colnames(dtbl)))
    }
  } else {
    value_name <- "FF4_amd_status"
    variable_name <- "FF4_amd_eye_sore_name"
    if (score == "ferris") {
      measure_vars <- ff4_eyes_ferris
      assert(all(measure_vars %in% colnames(dtbl)))
    } else {
      measure_vars <- ff4_eyes_conti
      assert(all(measure_vars %in% colnames(dtbl)))
    }
  }
  ## if not measure var, then id var
  id_vars <- colnames(dtbl)[!(colnames(dtbl) %in% measure_vars)]
  
  melt(data = dtbl,
      id.vars = id_vars,
      measure.vars = measure_vars,
      value.name = value_name,
      variable.name = variable_name,
      value.factor = TRUE)
}

get_incidence_tbl <- function(dtbl, amd_bl_col, amd_fu_col, split_col, digits = 2) {
  #'
  #'
  #'
  #'
  #'
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
  
  case1 <- data.table()
  case2 <- data.table()
  case3 <- data.table()
  case4 <- data.table()
  for (level in levels(dtbl[[split_col]])){
    tmp <- get_incidence(dtbl = dtbl[dtbl[[split_col]] == level],
                  amd_bl_col = amd_bl_col,
                  amd_fu_col = amd_fu_col)
    case1 <- rbindlist(list(case1,tmp$case1[, group := as.factor(level)]))
    case2 <- rbindlist(list(case2,tmp$case2[, group := as.factor(level)]))
    case3 <- rbindlist(list(case3,tmp$case3[, group := as.factor(level)]))
    case4 <- rbindlist(list(case4,tmp$case4[, group := as.factor(level)]))
    
  }
  tmp <- get_incidence(dtbl,amd_bl_col, amd_fu_col)
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

get_incidence <- function(dtbl, amd_bl_col, amd_fu_col, digits = 2) {
  #'
  #'
  #'
  #'
  #'
  #'
  assertDataTable(dtbl)
  assertString(amd_bl_col)
  assertString(amd_fu_col)
  assertInt(digits)
  assert(amd_bl_col %in% colnames(dtbl))
  assert(amd_fu_col %in% colnames(dtbl))
  assert(setequal(levels(dtbl[[amd_bl_col]]), c("no_amd", "early_amd", "late_amd")))
  assert(setequal(levels(dtbl[[amd_fu_col]]), c("no_amd", "early_amd", "late_amd")))
  
  
  amd_bl <- dtbl[[amd_bl_col]]
  amd_fu <-dtbl[[amd_fu_col]]
  
  # Case 1
  at_risk <- nrow(dtbl[amd_bl == "no_amd"])
  events <- nrow(dtbl[(amd_bl == "no_amd" & amd_fu == "early_amd")])
  case1 <- as.data.table(t(c(
    "At_Risk" = at_risk,
    "Events" = events,
    "Incidence(%)" = round(events / at_risk * 100, digits)
  )))
  # Case 2
  at_risk <- nrow(dtbl[amd_bl == "no_amd"])
  events <- nrow(dtbl[amd_bl == "no_amd" & amd_fu == "late_amd"])
  case2 <- as.data.table(t(c(
    "At_Risk" = at_risk,
    "Events" = events,
    "Incidence(%)" = round(events / at_risk * 100, digits)
  )))
  # Case 3
  at_risk <- nrow(dtbl[amd_bl == "no_amd" | amd_bl == "early_amd"])
  events <- nrow(dtbl[(amd_bl == "no_amd" | amd_bl == "early_amd") & amd_fu == "late_amd"])
  case3 <- as.data.table(t(c(
    "At_Risk" = at_risk,
    "Events" = events,
    "Incidence(%)" = round(events / at_risk * 100, digits)
  )))
  # Case 4
  at_risk <- nrow(dtbl[amd_bl == "early_amd"])
  events <- nrow(dtbl[amd_bl == "early_amd" & amd_fu == "late_amd"])
  case4 <- as.data.table(t(c(
    "At_Risk" = at_risk,
    "Events" = events,
    "Progression(%)" = round(events / at_risk * 100, digits)
  )))
  
  return(list("case1" = case1,
              "case2" = case2,
              "case3" = case3,
              "case4" = case4))
  
}

subset_simplify_factor <- function(dtbl, score_bl = "LT_conti_worst_eye", score_bl_levels, score_fu, score_fu_levels) {
  #'
  #'
  #' @param dtbl (data.table): data set.
  #' @param score_bl (chr): Column name with considered score.
  #' @param score_bl_levels (chr): Indicating selected amd levels. If named factor levels can be reordered.
  #' @param score_fu (chr): Column name with considered score.
  #' @param score_fu_levels (chr): Indicating selected amd levels. If named factor levels can be reordered.
  #'
  #' @return data.table: subsetted data table.
  #'
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
