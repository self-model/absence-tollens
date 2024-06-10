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

