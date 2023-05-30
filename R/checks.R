# CHECKS
source("analysis-one-eye-conti.R")
#===============================================================================
#######
# FIT #
#######

## Case 1
# 33/484
data_fit_1[, .(LT_conti_worst_eye, PT_conti_worst_eye)] %>% 
  summary()
# 15/268
data_fit_1[age_group_fit == "(34,45)", .(LT_conti_worst_eye, PT_conti_worst_eye)] %>% 
  summary()
# 8/136
data_fit_1[age_group_fit == "(45,50)", .(LT_conti_worst_eye, PT_conti_worst_eye)] %>% 
  summary()
# 10/80
data_fit_1[age_group_fit == "(50,55)", .(LT_conti_worst_eye, PT_conti_worst_eye)] %>% 
  summary()

## Case 2
# 3/484
data_fit_2[, .(LT_conti_worst_eye, PT_conti_worst_eye)] %>% 
  summary()
# 0/268
data_fit_2[age_group_fit == "(34,45)", .(LT_conti_worst_eye, PT_conti_worst_eye)] %>% 
  summary()
# 1/136
data_fit_2[age_group_fit == "(45,50)", .(LT_conti_worst_eye, PT_conti_worst_eye)] %>% 
  summary()
# 2/80
data_fit_2[age_group_fit == "(50,55)", .(LT_conti_worst_eye, PT_conti_worst_eye)] %>% 
  summary()

## Case 3
# 8/506
data_fit_3[, .(LT_conti_worst_eye, PT_conti_worst_eye)] %>% 
  summary()
# 1/275
data_fit_3[age_group_fit == "(34,45)", .(LT_conti_worst_eye, PT_conti_worst_eye)] %>% 
  summary()
# 2/144
data_fit_3[age_group_fit == "(45,50)", .(LT_conti_worst_eye, PT_conti_worst_eye)] %>% 
  summary()
# 5/87
data_fit_3[age_group_fit == "(50,55)", .(LT_conti_worst_eye, PT_conti_worst_eye)] %>% 
  summary()

## Case 4
# 5/22
data_fit_4[, .(LT_conti_worst_eye, PT_conti_worst_eye)] %>% 
  summary()
# 1/7
data_fit_4[age_group_fit == "(34,45)", .(LT_conti_worst_eye, PT_conti_worst_eye)] %>% 
  summary()
# 1/8
data_fit_4[age_group_fit == "(45,50)", .(LT_conti_worst_eye, PT_conti_worst_eye)] %>% 
  summary()
# 3/7
data_fit_4[age_group_fit == "(50,55)", .(LT_conti_worst_eye, PT_conti_worst_eye)] %>% 
  summary()

#######
# FF4 #
#######

# Case 1
# 46/332
data_ff4_1[ , .(LT_conti_worst_eye, U3T_conti_worst_eye)] %>% 
  summary()
# 14/149
data_ff4_1[age_group_ff4 == "(53,60)", .(LT_conti_worst_eye, U3T_conti_worst_eye)] %>% 
  summary()
# 19/113
data_ff4_1[age_group_ff4 == "(61,65)", .(LT_conti_worst_eye, U3T_conti_worst_eye)] %>% 
  summary()
# 13/70
data_ff4_1[age_group_ff4 == "(66,75)", .(LT_conti_worst_eye, U3T_conti_worst_eye)] %>% 
  summary()

# Case 2
# 7/332
data_ff4_2[ , .(LT_conti_worst_eye, U3T_conti_worst_eye)] %>% 
  summary()
# 4/149
data_ff4_2[age_group_ff4 == "(53,60)", .(LT_conti_worst_eye, U3T_conti_worst_eye)] %>% 
  summary()
# 1/113
data_ff4_2[age_group_ff4 == "(61,65)", .(LT_conti_worst_eye, U3T_conti_worst_eye)] %>% 
  summary()
# 2/70
data_ff4_2[age_group_ff4 == "(66,75)", .(LT_conti_worst_eye, U3T_conti_worst_eye)] %>% 
  summary()

# Case 3
# 14/348
data_ff4_3[ , .(LT_conti_worst_eye, U3T_conti_worst_eye)] %>% 
  summary()
# 4/151
data_ff4_3[age_group_ff4 == "(53,60)", .(LT_conti_worst_eye, U3T_conti_worst_eye)] %>% 
  summary()
# 3/120
data_ff4_3[age_group_ff4 == "(61,65)", .(LT_conti_worst_eye, U3T_conti_worst_eye)] %>% 
  summary()
# 7/77
data_ff4_3[age_group_ff4 == "(66,75)", .(LT_conti_worst_eye, U3T_conti_worst_eye)] %>% 
  summary()

# Case 4
# 7/16
data_ff4_4[ , .(LT_conti_worst_eye, U3T_conti_worst_eye)] %>% 
  summary()
# 0/2
data_ff4_4[age_group_ff4 == "(53,60)", .(LT_conti_worst_eye, U3T_conti_worst_eye)] %>% 
  summary()
# 2/7
data_ff4_4[age_group_ff4 == "(61,65)", .(LT_conti_worst_eye, U3T_conti_worst_eye)] %>% 
  summary()
# 5/7
data_ff4_4[age_group_ff4 == "(66,75)", .(LT_conti_worst_eye, U3T_conti_worst_eye)] %>% 
  summary()

