# occlusion effect on RT in absence with confidence contrast MP and MT
occlusion_RT_absent <- task_df %>%
  dplyr::filter(RT > 100 & RT < 7000 & correct == TRUE) %>%
  dplyr::group_by(subj_id) %>%
  dplyr::summarise(
    occlusion_RT_absent = (median(RT[present == 0 & hide_proportion == 0.35]) - median(RT[present == 0 & hide_proportion == 0.10])))

MT_MP_conf <- task_df2 %>%
  dplyr::filter(RT > 100 & inference_accepted == TRUE) %>%
  dplyr::select(confidence,inference_type,subj_id) %>%
  dplyr::group_by(subj_id) %>%
  dplyr::summarise(
    MT_MP_conf = mean(confidence[inference_type == 'MT']) - mean(confidence[inference_type == 'MP']))

corr_data <- occlusion_RT_absent %>%
  inner_join(MT_MP_conf, by = "subj_id") %>%
  select(-subj_id) 
corr_data <- as.data.frame(corr_data)

cor.test(corr_data$occlusion_RT_absent, corr_data$MT_MP_conf, type = "pearson", na.rm=TRUE)

# RT difference for positive/negative inferences correlation with RT difference for yes/no responses 
RT_polarity <- task_df2_RT %>%
  dplyr::filter(RT > 100 & RT < 9000 & inference_accepted == TRUE) %>%
  dplyr::select(RT,inference_type,subj_id) %>%
  dplyr::group_by(subj_id) %>%
  dplyr::summarise(
    polarity_RT = median(RT[inference_type %in% c('MT','DA')]) - median(RT[inference_type %in% c('MP','AC')])
  ) 

corr_data_new <- response_RT %>% 
  inner_join(RT_polarity, by = "subj_id") 

corr_data_new <- as.data.frame(corr_data_new) %>%
  dplyr::select(-subj_id)

cor.test(corr_data_new$response_RT, corr_data_new$polarity_RT, type = "pearson")