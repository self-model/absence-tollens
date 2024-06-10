library(Hmisc)

# extract summary statistics letter-detection task
## effects in present - absent
occlusion_RT <- task_df %>%
  dplyr::filter(RT > 100 & RT < 7000 & correct == TRUE) %>%
  dplyr::group_by(subj_id) %>%
  dplyr::summarise(occlusion_RT = 
                     (median(RT[response == TRUE & hide_proportion == 0.1]) - median(RT[response == TRUE & hide_proportion == 0.35])) - 
                     (median(RT[response == FALSE & hide_proportion == 0.1]) - median(RT[response == FALSE & hide_proportion == 0.35]))
  )

occlusion_conf <- task_df %>%
  dplyr::filter(correct == TRUE) %>%
  dplyr::group_by(subj_id) %>%
  dplyr::summarise(
    occlusion_conf = 
      (mean(confidence[response == TRUE & hide_proportion == 0.1]) - mean(confidence[response == TRUE & hide_proportion == 0.35])) - 
      (mean(confidence[response == FALSE & hide_proportion == 0.1]) - mean(confidence[response == FALSE & hide_proportion == 0.35]))
  )

hit_fa_occlusion <- hit_fa_occlusion_response %>%
  select(subj_id,hit_fa_occlusion)

## effects in absent
occlusion_RT_absent <- task_df %>%
  dplyr::filter(RT > 100 & RT < 7000 & correct == TRUE) %>%
  dplyr::group_by(subj_id) %>%
  dplyr::summarise(
    occlusion_RT_absent = (median(RT[response == FALSE & hide_proportion == 0.1]) - median(RT[response == FALSE & hide_proportion == 0.35])))

occlusion_conf_abs <- task_df %>%
  dplyr::filter(correct == TRUE) %>%
  dplyr::group_by(subj_id) %>%
  dplyr::summarise(
    occlusion_conf_absent = (mean(confidence[response == FALSE & hide_proportion == 0.1]) - mean(confidence[response == FALSE & hide_proportion == 0.35])))

fa_occlusion <- hit_fa_occlusion_response %>%
  select(subj_id,fa_rate_occlusion)

# extract summary statistics shapes task 
confidence_comparisons <- task_df2 %>%
  dplyr::filter(RT > 100 & inference_accepted == TRUE) %>%
  dplyr::select(confidence,inference_type,subj_id) %>%
  dplyr::group_by(subj_id) %>%
  dplyr::summarise(
    validity_conf =  mean(confidence[inference_type %in% c('MT', 'MP')]) - mean(confidence[inference_type %in% c('DA', 'AC')]),
    polarity_conf = mean(confidence[inference_type %in% c('MT','DA')]) - mean(confidence[inference_type %in% c('MP','AC')]),
    direction_conf = mean(confidence[inference_type %in% c('MT','AC')]) - mean(confidence[inference_type %in% c('MP','DA')])
  )

conf_id_comp <- task_df2 %>%
  dplyr::filter(RT > 100 & inference_accepted == TRUE) %>%
  dplyr::select(confidence,inference_type,subj_id) %>%
  dplyr::group_by(subj_id) %>%
  dplyr::summarise(
    MT_MP_conf = mean(confidence[inference_type == 'MT']) - mean(confidence[inference_type == 'MP']),
    MT_DA_conf = mean(confidence[inference_type == 'MT']) - mean(confidence[inference_type == 'DA']),
    MT_AC_conf = mean(confidence[inference_type == 'MT']) - mean(confidence[inference_type == 'AC']),
    MP_DA_conf = mean(confidence[inference_type == 'MP']) - mean(confidence[inference_type == 'DA']),
    MP_AC_conf = mean(confidence[inference_type == 'MP']) - mean(confidence[inference_type == 'AC']),
    DA_AC_conf = mean(confidence[inference_type == 'DA']) - mean(confidence[inference_type == 'AC'])
  )

# polarity, validity, direction effect on confidence X occlusion effects in present minus absent
occlusion_combined <- occlusion_RT %>% 
  inner_join(occlusion_conf, by = "subj_id") %>% 
  inner_join(hit_fa_occlusion, by = "subj_id")

corr_data_combined <- occlusion_combined %>% 
  inner_join(confidence_comparisons, by = "subj_id") %>%
  dplyr::select(-subj_id)

corr_data_combined <- as.data.frame(corr_data_combined)

col_labs2 <- c("Validity", "Polarity", "Direction")
row_labs2 <- c("RT", "Confidence", "False alarms")

corr_coeff_comb <- matrix(NA, nrow = 3, ncol = 3, dimnames = list(row_labs2, col_labs2))
corr_p_comb <- matrix(NA, nrow = 3, ncol = 3, dimnames = list(row_labs2, col_labs2))

for (i in 1:3) { 
  for (j in 4:6) { 
    corr_result_comb <- rcorr(corr_data_combined[, i], corr_data_combined[, j], type = "pearson")
    corr_coeff_comb[i, (j - 3)] <- corr_result_comb$r[1, 2]  
    corr_p_comb[i, (j - 3)] <- corr_result_comb$P[1, 2]  
  }
}

print(corr_coeff_comb)
print(corr_p_comb)

# polarity, validity, direction effect on confidence X occlusion effects in absent only

occlusion_absence_combined <- occlusion_RT_absent %>% 
  inner_join(occlusion_conf_abs, by = "subj_id") %>% 
  inner_join(fa_occlusion, by = "subj_id")

corr_data_abs <- occlusion_absence_combined %>% 
  inner_join(confidence_comparisons, by = "subj_id") %>%
  dplyr::select(-subj_id)

corr_data_abs <- as.data.frame(corr_data_abs)

corr_coeff2 <- matrix(NA, nrow = 3, ncol = 3, dimnames = list(row_labs2, col_labs2))
corr_p2 <- matrix(NA, nrow = 3, ncol = 3, dimnames = list(row_labs2, col_labs2))

for (i in 1:3) { 
  for (j in 4:6) { 
    corr_result2 <- rcorr(corr_data_abs[, i], corr_data_abs[, j], type = "pearson")
    corr_coeff2[i, (j - 3)] <- corr_result2$r[1, 2]  
    corr_p2[i, (j - 3)] <- corr_result2$P[1, 2]  
  }
}

print(corr_coeff2)
print(corr_p2)

# inference contrasts on confidence X occlusion effects in present minus absent

corr_data_comb2 <- occlusion_combined %>% 
  inner_join(conf_id_comp, by = "subj_id") %>%
  dplyr::select(-subj_id)
corr_data_comb2 <- as.data.frame(corr_data_comb2)

col_labs3 <- c("MT vs MP", "MT vs DA", "MT vs AC", "MP vs DA", "MP vs AC", "DA vs AC")

corr_coeff_comb2 <- matrix(NA, nrow = 3, ncol = 6, dimnames = list(row_labs2, col_labs3))
corr_p_comb2 <- matrix(NA, nrow = 3, ncol = 6, dimnames = list(row_labs2, col_labs3))

for (i in 1:3) {  
  for (j in 4:9) { 
    corr_result_comb2 <- rcorr(corr_data_comb2[, i], corr_data_comb2[, j], type = "pearson")
    corr_coeff_comb2[i, (j - 3)] <- corr_result_comb2$r[1, 2]  
    corr_p_comb2[i, (j - 3)] <- corr_result_comb2$P[1, 2]  
  }
}

print(corr_coeff_comb2)
print(corr_p_comb2)

# inference contrasts on confidence X occlusion effects in absent only 

corr_data_abs2 <- occlusion_absence_combined %>% 
  inner_join(conf_id_comp, by = "subj_id") %>%
  dplyr::select(-subj_id)
corr_data_abs2 <- as.data.frame(corr_data_abs2)

col_labs3 <- c("MT vs MP", "MT vs DA", "MT vs AC", "MP vs DA", "MP vs AC", "DA vs AC")

corr_coeff3 <- matrix(NA, nrow = 3, ncol = 6, dimnames = list(row_labs2, col_labs3))
corr_p3 <- matrix(NA, nrow = 3, ncol = 6, dimnames = list(row_labs2, col_labs3))

for (i in 1:3) {  
  for (j in 4:9) { 
    corr_result3 <- rcorr(corr_data_abs2[, i], corr_data_abs2[, j], type = "pearson")
    corr_coeff3[i, (j - 3)] <- corr_result3$r[1, 2]  
    corr_p3[i, (j - 3)] <- corr_result3$P[1, 2]  
  }
}

print(corr_coeff3)
print(corr_p3)