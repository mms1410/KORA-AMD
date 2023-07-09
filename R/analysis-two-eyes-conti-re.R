#-------------------------------------------------------------------------------
#                                 Random Effects Model
#-------------------------------------------------------------------------------
#######
# FIT #
#######

# checkConv warning if included time_bl_fu
## Case 1
model_glmer_fit_1 <- glmer(formula = amd_status_fu ~ ltalteru + lcsex + ltcigreg_sf + ll_hdla  + (1|person_id),
                           family = binomial(link = "logit"),
                           data = data_fit_long_1)
## Case 2
model_glmer_fit_2 <- glmer(formula = amd_status_fu ~ ltalteru + lcsex + ltcigreg_sf + ll_hdla  + (1|person_id),
                           family = binomial(link = "logit"),
                           data = data_fit_long_2)
## Case 3
model_glmer_fit_3 <- glmer(formula = amd_status_fu ~ ltalteru + lcsex + ltcigreg_sf + ll_hdla  + (1|person_id),
                           family = binomial(link = "logit"),
                           data = data_fit_long_3)
## Case 4
model_glmer_fit_4 <- glmer(formula = amd_status_fu ~ ltalteru + lcsex + ltcigreg_sf + ll_hdla  + (1|person_id),
                           family = binomial(link = "logit"),
                           data = data_fit_long_4)
#######
# FF4 #
#######

## Case 1
model_glmer_ff4_1 <- glmer(formula = amd_status_fu ~ ltalteru + lcsex + ltcigreg_sf + ll_hdla  + (1|person_id),
                           family = binomial(link = "logit"),
                           data = data_ff4_long_1)
## Case2
model_glmer_ff4_2 <- glmer(formula = amd_status_fu ~ ltalteru + lcsex + ltcigreg_sf + ll_hdla  + (1|person_id),
                           family = binomial(link = "logit"),
                           data = data_ff4_long_2)
## Case3
model_glmer_ff4_3 <- glmer(formula = amd_status_fu ~ ltalteru + lcsex + ltcigreg_sf + ll_hdla  + (1|person_id),
                           family = binomial(link = "logit"),
                           data = data_ff4_long_3)
## Case4
model_glmer_ff4_4 <- glmer(formula = amd_status_fu ~ ltalteru + lcsex + ltcigreg_sf + ll_hdla  + (1|person_id),
                           family = binomial(link = "logit"),
                           data = data_ff4_long_4)