# regression
# extract relevant measures from letter detection task 
occlusion_RT <- task_df %>%
  dplyr::filter(RT > 100 & RT < 7000 & correct == TRUE) %>%
  dplyr::group_by(subj_id, present) %>%
  dplyr::summarise(
    occlusion_RT = median(RT[hide_proportion == 0.35], na.rm = TRUE) - median(RT[hide_proportion == 0.10], na.rm = TRUE)
  ) %>%
  dplyr::ungroup() %>%  #this is acc important bc mean and sd need to be calc over entire dataset 
  dplyr::mutate(
    occlusion_RT_z = (occlusion_RT - mean(occlusion_RT, na.rm = TRUE)) / sd(occlusion_RT, na.rm = TRUE)
  )

occlusion_confidence <- task_df %>%
  dplyr::filter(correct == TRUE) %>%
  dplyr::group_by(subj_id, present) %>%
  dplyr::summarise(
    occlusion_confidence = mean(confidence[hide_proportion == 0.35], na.rm = TRUE) - mean(confidence[hide_proportion == 0.10], na.rm = TRUE)
  ) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(
    occlusion_confidence_z = (occlusion_confidence - mean(occlusion_confidence, na.rm = TRUE)) / sd(occlusion_confidence, na.rm = TRUE)
  )

occlusion_accuracy <- task_df %>%
  dplyr::group_by(subj_id, present, hide_proportion) %>%
  dplyr::summarise(
    accuracy = sum(!correct)/n(),
    .groups = 'drop') %>%
  dplyr::group_by(subj_id, present) %>%
  dplyr::summarise(
    occlusion_accuracy = accuracy[hide_proportion == 0.35] - accuracy[hide_proportion == 0.10]
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    occlusion_accuracy_z = (occlusion_accuracy - mean(occlusion_accuracy, na.rm = TRUE)) / sd(occlusion_accuracy, na.rm = TRUE)
  )

occlusion_df <- occlusion_RT %>%
  left_join(occlusion_confidence, by = c("subj_id", "present")) %>%
  left_join(occlusion_accuracy, by = c("subj_id", "present"))

# extract relevant measures from shapes task
inference_confidence <- task_df2 %>%
  dplyr::filter(RT > 100 & inference_accepted == TRUE) %>%
  dplyr::select(confidence,inference_type,subj_id) %>%
  dplyr::group_by(subj_id) %>%
  dplyr::summarise(
    MT = mean(confidence[inference_type == 'MT']) - mean(confidence[inference_type == 'AC']),
    MP = mean(confidence[inference_type == 'MP']) - mean(confidence[inference_type == 'AC']),
    DA = mean(confidence[inference_type == 'DA']) - mean(confidence[inference_type == 'AC'])
  )

# bind together
analysis_df <- occlusion_df %>%
  inner_join(inference_confidence, by = "subj_id")

# regression of occlusion effect on RT in absence as a function of confidence in inference types 
absent_df <- analysis_df %>% 
  dplyr::filter(present == 0) 

summary(RT_absent_model <- lm(occlusion_RT_z ~ MT + MP + DA, data = absent_df, na.action=na.omit))

# regression of occlusion effect on RT in presence as a function of confidence in inference types 
present_df <- analysis_df %>% 
  dplyr::filter(present == 1) 

summary(RT_present_model <- lm(occlusion_RT_z ~ MT + MP + DA, data = present_df, na.action=na.omit))

# confidence in absent
summary(conf_absent_model <- lm(occlusion_confidence_z ~ MT + MP + DA, data = absent_df, na.action=na.omit))

# confidence in present
summary(conf_present_model <- lm(occlusion_confidence_z ~ MT + MP + DA, data = present_df, na.action=na.omit))

# error rate in absence
summary(accuracy_absent_model <- lm(occlusion_accuracy_z ~ MT + MP + DA, data = absent_df, na.action=na.omit))

# error rate in presence
summary(accuracy_present_model <- lm(occlusion_accuracy_z ~ MT + MP + DA, data = present_df, na.action=na.omit))

# permutation tests comparing goodness of fit of present vs absent models for each DV 

# non-parametric permutation test for present vs absent model for RT 
get_rsquared_diff_RT <- function(df) {
  
  present_df <- df %>% filter(present==1);
  absent_df <- df %>% filter(present==0);
  
  present_model <- lm(occlusion_RT~MT+MP+DA, data=present_df) %>%
    summary()
  
  absent_model <- lm(occlusion_RT~MT+MP+DA, data=absent_df) %>%
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

true_difference_RT <- get_rsquared_diff_RT(analysis_df);

N = 10000; #number of permutations
null_dist_RT = c();

for (i in 1:N) {
  shuffled_df <- shuffle_present(analysis_df);
  shuffled_diff_RT <- get_rsquared_diff_RT(shuffled_df);
  null_dist_RT = c(null_dist_RT, shuffled_diff_RT);
}

(p_value_RT <- mean(abs(true_difference_RT)<=abs(null_dist_RT)))

# permutation test confidence
get_rsquared_diff_conf <- function(df) {
  
  present_df <- df %>% filter(present==1);
  absent_df <- df %>% filter(present==0);
  
  present_model <- lm(occlusion_confidence~MT+MP+DA, data=present_df) %>% summary()
  absent_model <- lm(occlusion_confidence~MT+MP+DA, data=absent_df) %>% summary()
  
  return(absent_model$r.squared-present_model$r.squared)
}

true_difference_conf <- get_rsquared_diff_conf(analysis_df);

N = 10000; #number of permutations
null_dist_conf = c();

for (i in 1:N) {
  shuffled_df <- shuffle_present(analysis_df);
  shuffled_diff_conf <- get_rsquared_diff_conf(shuffled_df);
  null_dist_conf = c(null_dist_conf, shuffled_diff_conf);
}

(p_value_conf <- mean(abs(true_difference_conf)<=abs(null_dist_conf)))

# permutation test error rate
get_rsquared_diff_accuracy <- function(df) {
  
  present_df <- df %>% filter(present==1);
  absent_df <- df %>% filter(present==0);
  
  present_model <- lm(occlusion_accuracy~MT+MP+DA, data=present_df) %>% summary()
  absent_model <- lm(occlusion_accuracy~MT+MP+DA, data=absent_df) %>% summary()
  
  return(absent_model$r.squared-present_model$r.squared)
}

true_difference_accuracy <- get_rsquared_diff_accuracy(analysis_df);

N = 10000; 
null_dist_accuracy = c();

for (i in 1:N) {
  shuffled_df <- shuffle_present(analysis_df);
  shuffled_diff_accuracy <- get_rsquared_diff_accuracy(shuffled_df);
  null_dist_accuracy = c(null_dist_accuracy, shuffled_diff_accuracy);
}

(p_value_accuracy <- mean(abs(true_difference_accuracy)<=abs(null_dist_accuracy)))

