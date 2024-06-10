# extract star trials 
star_df <- df2 %>%
  dplyr::filter(trial_type=='jsShapes' & rare_option == 'true') %>%
  dplyr::select(PROLIFIC_PID,shapes,occluder,options,response,RT,confidence) %>%
  dplyr::rename(subj_id=PROLIFIC_PID) %>%
  dplyr::mutate(decision=ifelse(response==0,as.numeric(substr(options,2,2)),as.numeric(substr(options,4,4))),
                RT=as.numeric(RT),
                confidence=as.numeric(confidence)) %>%
  dplyr::mutate(
    MT_accepted = case_when(
      decision == 5 ~ TRUE,
      decision == 0 ~ FALSE)
  )

# merge with occlusion RT effect 
merged_df <- star_df %>%
  inner_join(occlusion_RT_absent, by = "subj_id")

group_A <- merged_df %>%
  filter(MT_accepted == TRUE) %>%
  pull(occlusion_RT_absent)

group_B <- merged_df %>%
  filter(MT_accepted == FALSE) %>%
  pull(occlusion_RT_absent)

# test difference between MT-group and not-MT group in occlusion RT effect 
t.test(group_A, group_B)

# analyse non-star trials 
star_df2 <- df2 %>%
  dplyr::filter(trial_type=='jsShapes' & rare_option == 'false') %>%
  dplyr::select(PROLIFIC_PID,shapes,occluder,options,response,RT,confidence,confidence_RT, rare_option) %>%
  dplyr::rename(subj_id=PROLIFIC_PID) %>%
  dplyr::mutate(decision=ifelse(response==0,as.numeric(substr(options,2,2)),as.numeric(substr(options,4,4))),
                RT=as.numeric(RT),
                confidence=as.numeric(confidence)) %>%
  dplyr::mutate(
    inference_type = case_when(
      substr(shapes, 2, 2) == "0" & substr(shapes, 4, 4) == "1" & occluder == 'right' ~ 'MP',
      substr(shapes, 2, 2) == "0" & substr(shapes, 4, 4) == "1" & occluder == 'left' ~ 'AC',
      !(substr(shapes, 2, 2) == "0" & substr(shapes, 4, 4) == "1") & occluder == 'right' ~ 'DA',
      !(substr(shapes, 2, 2) == "0" & substr(shapes, 4, 4) == "1") & occluder == 'left' ~ 'MT'
    )) %>%
  dplyr::mutate(
    inference_accepted = case_when(
      inference_type == "MP" & decision == 1 ~ TRUE,
      inference_type == "AC" & decision == 0 ~ TRUE,
      inference_type == "DA" & decision %in% c(0,2,3,4) ~ TRUE,
      inference_type == "MT" & decision %in% c(1,2,3,4) ~ TRUE,
      TRUE ~ FALSE
    ))

# proportion accepted 
proportion_accepted <- star_df2 %>%
  dplyr::group_by(subj_id, inference_type) %>%
  dplyr::summarise(
    proportion_accepted = sum(inference_accepted)/n()
  )
star_df2 <- star_df2 %>%
  left_join(proportion_accepted, by = c("subj_id", "inference_type"))

# proportion overall MT 
mean_proportion_accepted <- star_df2 %>%
  dplyr::summarise(
    mean_prop = mean(proportion_accepted[inference_type=='MT'])
  )

# extract proportion MT for hard trials 
star_df2 <- star_df2 %>%
  mutate(
    revealed_shape = ifelse(occluder=='right',as.numeric(substr(shapes,2,2)),as.numeric(substr(shapes,4,4))),
    option1 = as.numeric(substr(options,2,2)),
    option2 = as.numeric(substr(options,4,4)),
    hard_trial = revealed_shape == option1 | revealed_shape == option2) 

proportion_accepted_hard <- star_df2 %>%
  filter(hard_trial)%>%
  dplyr::group_by(subj_id, inference_type) %>%
  dplyr::summarise(
    proportion_accepted = sum(inference_accepted)/n()
  )

mean_prop_hard <- proportion_accepted_hard %>%
  dplyr::summarise(
    mean_prop_hard = mean(proportion_accepted[inference_type=='MT'])
  )

mean_prop_easy <- task_df %>%
  filter(!hard_trial, inference_type == 'MT') %>%
  dplyr::summarise(mean_prop_easy = mean(inference_accepted))