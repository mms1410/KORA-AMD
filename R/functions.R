library(data.table)
library(checkmate)
library(haven)
#-------------------------------------------------------------------------------
preprocess.data <- function(dtbl, data.dictionary, age.groups.fit, age.groups.ff4) {
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
  
  dtbl <- set.age.groups(dtbl, named.list.ff4 = age.groups.ff4, age.groups.fit)
  
  return(dtbl)
}

set.age.groups <- function(dtbl, named.list.ff4, named.list.fit, col.age = "ltalteru") {
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
  #' @param col.age (str):
  #' @param named.list.ff4 (str):
  #' @param named.list.fit (str):
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
  
  
get.data.summary <- function(dtbl, verbose=TRUE, log) {
  #'
  #'
  #'
  #'
  #'
  #'
  #'
  assertDataTable(dtbl)
  assertLogical(verbose)
  assertLogical(log)
  
  
}
