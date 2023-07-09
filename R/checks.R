# CHECKS
library(this.path)  # path via rstudioapi not available if called via terminal
source("analysis-one-eye-conti.R")
source("analysis-two-eyes-conti.R")
#============================= DATA SUBSETS  GLM ===============================
#######
# FIT #
#######

## Case 1
# 33/484
data_fit_conti_1[, .(LT_conti_worst_eye, PT_conti_worst_eye)] %>% 
  summary()
# 15/268
data_fit_conti_1[age_group_fit == "(34,45)", .(LT_conti_worst_eye, PT_conti_worst_eye)] %>% 
  summary()
# 8/136
data_fit_conti_1[age_group_fit == "(45,50)", .(LT_conti_worst_eye, PT_conti_worst_eye)] %>% 
  summary()
# 10/80
data_fit_conti_1[age_group_fit == "(50,55)", .(LT_conti_worst_eye, PT_conti_worst_eye)] %>% 
  summary()

## Case 2
# 3/484
data_fit_conti_2[, .(LT_conti_worst_eye, PT_conti_worst_eye)] %>% 
  summary()
# 0/268
data_fit_conti_2[age_group_fit == "(34,45)", .(LT_conti_worst_eye, PT_conti_worst_eye)] %>% 
  summary()
# 1/136
data_fit_conti_2[age_group_fit == "(45,50)", .(LT_conti_worst_eye, PT_conti_worst_eye)] %>% 
  summary()
# 2/80
data_fit_conti_2[age_group_fit == "(50,55)", .(LT_conti_worst_eye, PT_conti_worst_eye)] %>% 
  summary()

## Case 3
# 8/506
data_fit_conti_3[, .(LT_conti_worst_eye, PT_conti_worst_eye)] %>% 
  summary()
# 1/275
data_fit_conti_3[age_group_fit == "(34,45)", .(LT_conti_worst_eye, PT_conti_worst_eye)] %>% 
  summary()
# 2/144
data_fit_conti_3[age_group_fit == "(45,50)", .(LT_conti_worst_eye, PT_conti_worst_eye)] %>% 
  summary()
# 5/87
data_fit_conti_3[age_group_fit == "(50,55)", .(LT_conti_worst_eye, PT_conti_worst_eye)] %>% 
  summary()

## Case 4
# 5/22
data_fit_conti_4[, .(LT_conti_worst_eye, PT_conti_worst_eye)] %>% 
  summary()
# 1/7
data_fit_conti_4[age_group_fit == "(34,45)", .(LT_conti_worst_eye, PT_conti_worst_eye)] %>% 
  summary()
# 1/8
data_fit_conti_4[age_group_fit == "(45,50)", .(LT_conti_worst_eye, PT_conti_worst_eye)] %>% 
  summary()
# 3/7
data_fit_conti_4[age_group_fit == "(50,55)", .(LT_conti_worst_eye, PT_conti_worst_eye)] %>% 
  summary()

#######
# FF4 #
#######

# Case 1
# 46/332
data_ff4_conti_1[ , .(LT_conti_worst_eye, U3T_conti_worst_eye)] %>% 
  summary()
# 14/149
data_ff4_conti_1[age_group_ff4 == "(53,60)", .(LT_conti_worst_eye, U3T_conti_worst_eye)] %>% 
  summary()
# 19/113
data_ff4_conti_1[age_group_ff4 == "(61,65)", .(LT_conti_worst_eye, U3T_conti_worst_eye)] %>% 
  summary()
# 13/70
data_ff4_conti_1[age_group_ff4 == "(66,75)", .(LT_conti_worst_eye, U3T_conti_worst_eye)] %>% 
  summary()

# Case 2
# 7/332
data_ff4_conti_2[ , .(LT_conti_worst_eye, U3T_conti_worst_eye)] %>% 
  summary()
# 4/149
data_ff4_conti_2[age_group_ff4 == "(53,60)", .(LT_conti_worst_eye, U3T_conti_worst_eye)] %>% 
  summary()
# 1/113
data_ff4_conti_2[age_group_ff4 == "(61,65)", .(LT_conti_worst_eye, U3T_conti_worst_eye)] %>% 
  summary()
# 2/70
data_ff4_conti_2[age_group_ff4 == "(66,75)", .(LT_conti_worst_eye, U3T_conti_worst_eye)] %>% 
  summary()

# Case 3
# 14/348
data_ff4_conti_3[ , .(LT_conti_worst_eye, U3T_conti_worst_eye)] %>% 
  summary()
# 4/151
data_ff4_conti_3[age_group_ff4 == "(53,60)", .(LT_conti_worst_eye, U3T_conti_worst_eye)] %>% 
  summary()
# 3/120
data_ff4_conti_3[age_group_ff4 == "(61,65)", .(LT_conti_worst_eye, U3T_conti_worst_eye)] %>% 
  summary()
# 7/77
data_ff4_conti_3[age_group_ff4 == "(66,75)", .(LT_conti_worst_eye, U3T_conti_worst_eye)] %>% 
  summary()

# Case 4
# 7/16
data_ff4_conti_4[ , .(LT_conti_worst_eye, U3T_conti_worst_eye)] %>% 
  summary()
# 0/2
data_ff4_conti_4[age_group_ff4 == "(53,60)", .(LT_conti_worst_eye, U3T_conti_worst_eye)] %>% 
  summary()
# 2/7
data_ff4_conti_4[age_group_ff4 == "(61,65)", .(LT_conti_worst_eye, U3T_conti_worst_eye)] %>% 
  summary()
# 5/7
data_ff4_conti_4[age_group_ff4 == "(66,75)", .(LT_conti_worst_eye, U3T_conti_worst_eye)] %>% 
  summary()
#============================= DATA SUBSETS  GEE ===============================
#######
# FIT #
#######

data_fit_long[, .(amd_status_bl, amd_status_fu)] %>%  summary()
# Case  1
data_fit_long_1[, .(amd_status_bl, amd_status_fu)] %>%  summary()
# Case 2
data_fit_long_2[, .(amd_status_bl, amd_status_fu)] %>%  summary()
# Case 3
data_fit_long_3[, .(amd_status_bl, amd_status_fu)] %>%  summary()
# Case 4
data_fit_long_4[, .(amd_status_bl, amd_status_fu)] %>%  summary()


#######
# FF4 #
#######
data_ff4_long[, .(amd_status_bl, amd_status_fu)] %>%  summary()
# Case  1
data_ff4_long_1[, .(amd_status_bl, amd_status_fu)] %>%  summary()
# Case 2
data_ff4_long_2[, .(amd_status_bl, amd_status_fu)] %>%  summary()
# Case 3
data_ff4_long_3[, .(amd_status_bl, amd_status_fu)] %>%  summary()
# Case 4
data_ff4_long_4[, .(amd_status_bl, amd_status_fu)] %>%  summary()

#===============================================================================
get_amd_table_conti(data_fit_conti_1,
                    data_fit_conti_2,
                    data_fit_conti_3,
                    data_fit_conti_4)


get_amd_table_long(data_fit_conti_long_1,
                   data_fit_conti_long_2,
                   data_fit_conti_long_3,
                   data_fit_conti_long_4)


#=============================== GLM Models ====================================
#######
# FIT #
#######

# Case 1
round(exp(coef(model_glm_fit_1)), 1)
round(exp(coef(model_glm_fit_1_gls)), 1)

# Case 2
round(exp(coef(model_glm_fit_2)), 1)
round(exp(coef(model_glm_fit_2_gls)), 1)

# Case 3
round(exp(coef(model_glm_fit_3)), 1)
round(exp(coef(model_glm_fit_3_gls)), 1)

# Case 4
round(exp(coef(model_glm_fit_4)), 1)
round(exp(coef(model_glm_fit_4_gls)), 1)



#=============================== GEE Models ====================================
check_model_existence(model_name = c("model_gee_ff4_conti_1", "model_gee_ff4_conti_2", "model_gee_ff4_conti_3", "model_gee_ff4_conti_4"))
check_model_existence(model_name = c("model_gee_fit_conti_1", "model_gee_fit_conti_2", "model_gee_fit_conti_3", "model_gee_fit_conti_4"))
