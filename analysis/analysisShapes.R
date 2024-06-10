# read-in data shapes task 
df2 <- read.csv('shapesInference/data/jatos_results_data_batch1.csv', sep = ",", header = TRUE)

# tidy
raw_df2 <- df2 %>%
  dplyr::filter(trial_type=='jsShapes') %>%
  dplyr::select(PROLIFIC_PID,shapes,occluder,options,response,RT,confidence) %>%
  dplyr::rename(subj_id=PROLIFIC_PID) %>%
  dplyr::mutate(decision=ifelse(response==0,as.numeric(substr(options,2,2)),as.numeric(substr(options,4,4))),
                RT=as.numeric(RT),
                confidence=as.numeric(confidence))

# add inference type
raw_df2 <- raw_df2 %>% 
  dplyr::mutate(
    inference_type = case_when(
      substr(shapes, 2, 2) == "0" & substr(shapes, 4, 4) == "1" & occluder == 'right' ~ 'MP',
      substr(shapes, 2, 2) == "0" & substr(shapes, 4, 4) == "1" & occluder == 'left' ~ 'AC',
      !(substr(shapes, 2, 2) == "0" & substr(shapes, 4, 4) == "1") & occluder == 'right' ~ 'DA',
      !(substr(shapes, 2, 2) == "0" & substr(shapes, 4, 4) == "1") & occluder == 'left' ~ 'MT'
    ))

# specify "accepted" 
raw_df2 <- raw_df2 %>%
  dplyr::mutate(
    inference_accepted = case_when(
      inference_type == "MP" & decision == 1 ~ TRUE,
      inference_type == "AC" & decision == 0 ~ TRUE,
      inference_type == "DA" & decision %in% c(0,2,3,4) ~ TRUE,
      inference_type == "MT" & decision %in% c(1,2,3,4) ~ TRUE,
      TRUE ~ FALSE
    ))
raw_df2$inference_type <- as.factor(raw_df2$inference_type)

# calculate proportion accepted
proportion_accepted <- raw_df2 %>%
  dplyr::group_by(subj_id, inference_type) %>%
  dplyr::summarise(
    proportion_accepted = sum(inference_accepted)/n()
  )
raw_df2 <- raw_df2 %>%
  left_join(proportion_accepted, by = c("subj_id", "inference_type"))

# low accuracy
low_accuracy_shapes <- raw_df2 %>%
  dplyr::filter(inference_type == "MP" & proportion_accepted < 0.75 ) %>%
  dplyr::pull(subj_id)  

# too fast 
too_fast_shapes <- raw_df2 %>%
  dplyr::group_by(subj_id) %>%
  dplyr::summarise(
    first_quartile_RT = quantile(RT,0.25)) %>%
  dplyr::filter(first_quartile_RT<100) %>%
  dplyr::pull(subj_id)

# df for analysis
to_exclude_shapes <- c(low_accuracy_shapes,too_fast_shapes) %>% unique()
task_df2 <- raw_df2 %>%
  filter(!(subj_id %in% to_exclude_shapes))

# too slow 
too_slow_shapes <- raw_df2 %>%
  dplyr::group_by(subj_id) %>%
  dplyr::summarise(
    third_quantile_RT = quantile(RT,0.75)) %>%
  dplyr::filter(third_quantile_RT>9000) %>%
  dplyr::pull(subj_id)

to_exclude_shapes_RT <- c(low_accuracy_shapes, too_fast_shapes, too_slow_shapes) %>% unique()

# df for RT analysis
task_df2_RT <- raw_df2 %>%
  filter(!(subj_id %in% to_exclude_shapes_RT)) 

# median individual-level response times for each inference type
RT_df <- task_df2_RT %>%
  dplyr::filter(RT > 100 & RT < 9000 & inference_accepted == TRUE) %>%
  dplyr::select(RT,inference_type,subj_id) %>%
  dplyr::mutate(subj_id = as.factor(subj_id)) %>%
  dplyr::group_by(subj_id, inference_type) %>%
  dplyr::summarise(median_RT = median(RT), .groups = 'drop') # do this or no?

# anova 
aov_RT <- aov_ez("subj_id", "median_RT", RT_df, within = "inference_type", na.rm=TRUE)
summary(aov_RT)

# post-hoc contrasts 
summary(contrast(emmeans(aov_RT, "inference_type"), method = "pairwise"))

# mean individual-level confidence for each inference type 
conf_df <- task_df2 %>%
  dplyr::filter(RT > 100 & inference_accepted == TRUE) %>%
  dplyr::select(confidence,inference_type,subj_id) %>%
  dplyr::mutate(subj_id = as.factor(subj_id)) %>%
  dplyr::group_by(subj_id, inference_type) %>%
  dplyr::summarise(confidence = mean(confidence), .groups = 'drop') 

# anova 
aov_conf <- aov_ez("subj_id", "confidence", conf_df, within = "inference_type", na.rm=TRUE)
summary(aov_conf)

# post-hoc comparisons 
summary(contrast(emmeans(aov_conf, "inference_type"), method = "pairwise"))

# individual-level proportions per inference type 
prop_df <- task_df2 %>%
  dplyr::filter(RT > 100) %>%
  dplyr::select(proportion_accepted,inference_type,subj_id) %>%
  dplyr::mutate(subj_id = as.factor(subj_id))  

# anova 
aov_prop <- aov_ez("subj_id", "proportion_accepted", prop_df, within = "inference_type", na.rm=TRUE)
summary

# post-hoc comparisons 
summary(contrast(emmeans(aov_prop, "inference_type"), method = "pairwise"))