# new variable for shapes
task_df2 <- task_df2 %>%
  dplyr::mutate(
    model_consistent = ifelse(inference_accepted, 1, -1),
    adj_confidence = model_consistent * confidence
  )

# redo analysis
MT_MP_conf_new <- task_df2 %>%
  dplyr::filter(RT > 100) %>%
  dplyr::select(adj_confidence,inference_type,subj_id) %>%
  dplyr::group_by(subj_id) %>%
  dplyr::summarise(
    MT_MP_conf_new = mean(adj_confidence[inference_type == 'MT']) - mean(adj_confidence[inference_type == 'MP']))

corr_data_new <- occlusion_RT_absent %>%
  inner_join(MT_MP_conf_new, by = "subj_id") %>%
  select(-subj_id) 
corr_data_new <- as.data.frame(corr_data_new)

cor.test(corr_data_new$occlusion_RT_absent, corr_data_new$MT_MP_conf_new, type = "pearson", na.rm=TRUE)

MT_DA_conf_new <- task_df2 %>%
  dplyr::filter(RT > 100) %>%
  dplyr::select(adj_confidence,inference_type,subj_id) %>%
  dplyr::group_by(subj_id) %>%
  dplyr::summarise(
    MT_DA_conf = mean(adj_confidence[inference_type == 'MT']) - mean(adj_confidence[inference_type == 'DA']))

corr_data2_new <- occlusion_RT_absent %>%
  inner_join(MT_DA_conf_new, by = "subj_id")
corr_data2 <- as.data.frame(corr_data2_new)

cor.test(corr_data2_new$occlusion_RT_absent, corr_data2_new$MT_DA_conf, type = "pearson", na.rm=TRUE)

# extract relevant measures from shapes task
inference_confidence_new <- task_df2 %>%
  dplyr::filter(RT > 100) %>%
  dplyr::select(adj_confidence,inference_type,subj_id) %>%
  dplyr::group_by(subj_id) %>%
  dplyr::summarise(
    MT = mean(adj_confidence[inference_type == 'MT'], na.rm=TRUE) - mean(adj_confidence[inference_type == 'AC'], na.rm=TRUE),
    MP = mean(adj_confidence[inference_type == 'MP'], na.rm=TRUE) - mean(adj_confidence[inference_type == 'AC'], na.rm=TRUE),
    DA = mean(adj_confidence[inference_type == 'DA'], na.rm=TRUE) - mean(adj_confidence[inference_type == 'AC'], na.rm=TRUE)
  )

# bind together
analysis_df_new <- occlusion_df %>%
  inner_join(inference_confidence_new, by = "subj_id")

# regression of occlusion effect on RT in absence as a function of confidence in inference types 
absent_df_new <- analysis_df_new %>% 
  dplyr::filter(present == 0) 

summary(RT_absent_model_new <- lm(occlusion_RT_z ~ MT + MP + DA, data = absent_df_new, na.action=na.omit))

# regression of occlusion effect on RT in presence as a function of confidence in inference types 
present_df_new <- analysis_df_new %>% 
  dplyr::filter(present == 1) 

summary(RT_present_model_new <- lm(occlusion_RT_z ~ MT + MP + DA, data = present_df_new, na.action=na.omit))

# confidence in absent
summary(conf_absent_model_new <- lm(occlusion_confidence_z ~ MT + MP + DA, data = absent_df_new, na.action=na.omit))

# confidence in present
summary(conf_present_model_new <- lm(occlusion_confidence_z ~ MT + MP + DA, data = present_df_new, na.action=na.omit))

# false alarm rate
summary(accuracy_absent_model_new <- lm(occlusion_accuracy_z ~ MT + MP + DA, data = absent_df_new, na.action=na.omit))

# miss rate
summary(accuracy_present_model_new <- lm(occlusion_accuracy_z ~ MT + MP + DA, data = present_df_new, na.action=na.omit))

# non-parametric permutation test for present vs absent model for RT 
get_rsquared_diff_RT <- function(df) {
  
  present_df <- df %>% filter(present==1);
  absent_df <- df %>% filter(present==0);
  
  present_model <- lm(occlusion_RT_z~MP+MT+DA, data=present_df) %>%
    summary()
  
  absent_model <- lm(occlusion_RT_z~MP+MT+DA, data=absent_df) %>%
    summary()
  
  return(absent_model$r.squared-present_model$r.squared)
}

shuffle_present <- function(df) {
  new_df <- df %>%
    dplyr::group_by(subj_id) %>%
    dplyr::mutate(flip = rbinom(1,1,0.5),
                  old_present = present,
                  present = ifelse(flip, as.integer(!present),present))
  return(new_df)
}

true_difference_RT <- get_rsquared_diff_RT(analysis_df_new);

N = 5000; #number of permutations
null_dist_RT = c();

for (i in 1:N) {
  shuffled_df <- shuffle_present(analysis_df_new);
  shuffled_diff_RT <- get_rsquared_diff_RT(shuffled_df);
  null_dist_RT = c(null_dist_RT, shuffled_diff_RT);
}

p_value_RT <- mean(abs(true_difference_RT)<=abs(null_dist_RT))