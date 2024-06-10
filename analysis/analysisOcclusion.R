library(tidyverse)
library(ggplot2)

setwd('C:/Users/schip/Desktop/dissertation experiments/shapesInference/experiments/pilots')

# read-in data from letter task 
batch6 <- read.csv('pilotRowOcclusion1b/data/jatos_results_data_batch2.csv', sep = ",", header = TRUE) 
batch7 <- read.csv('pilotRowOcclusion1c/data/jatos_results_data_batch1.csv', sep = ",", header = TRUE)
batch8 <- read.csv('pilotRowOcclusion1d/data/jatos_results_data_batch1.csv', sep = ",", header = TRUE)
batch9 <- read.csv('pilotRowOcclusion2/data/jatos_results_data_batch1.csv', sep = ",", header = TRUE)

# bind data
df2 <- bind_rows(batch6,batch7,batch8,batch9)

# tidy
raw_df2 <- df2 %>%
  dplyr::filter(trial_type=='noisyLetter'& (test_part=='test1' | test_part=='test2')) %>%
  dplyr::select(PROLIFIC_PID, RT, hide_proportion, present, correct, confidence, response, presence_key) %>%
  dplyr::rename(subj_id=PROLIFIC_PID) %>%
  dplyr::mutate(
    RT=as.numeric(RT),
    confidence=as.numeric(confidence),
    present=as.numeric(present),
    response = response == presence_key,
    correct = ifelse(correct == 'true', TRUE, FALSE))

# exclusions
low_accuracy <- raw_df2 %>%
  dplyr::group_by(subj_id) %>%
  dplyr::summarise(
    accuracy = mean(correct)) %>%
  dplyr::filter(accuracy<0.5) %>%
  dplyr::pull(subj_id)

too_slow <- raw_df2 %>%
  dplyr::group_by(subj_id) %>%
  dplyr::summarise(
    third_quartile_RT = quantile(RT,0.75)) %>%
  dplyr::filter(third_quartile_RT>7000) %>%
  dplyr::pull(subj_id)

too_fast <- raw_df2 %>%
  dplyr::group_by(subj_id) %>%
  dplyr::summarise(
    first_quartile_RT = quantile(RT,0.25)) %>%
  dplyr::filter(first_quartile_RT<100) %>%
  dplyr::pull(subj_id)

to_exclude <- c(
  low_accuracy,
  too_slow,
  too_fast
) %>% unique()

task_df2 <- raw_df2 %>%
  filter(!(subj_id %in% to_exclude))

# global present-absent comparisons 
# are they more confident when target = present
task_df2 %>%
  dplyr::group_by(present)%>%
  dplyr::summarise(mean_confidence = mean(confidence))

# are they faster when target = present 
task_df2 %>%
  dplyr::group_by(present) %>%
  dplyr::summarise(median_RT = median(RT))

# accuracy? 
task_df2 %>%
  dplyr::group_by(present) %>%
  dplyr::summarise(
    total_trials = n(),
    correct_trials = sum(correct),
    proportion_correct = correct_trials/total_trials
  ) 
## this would be hit and correct rejection rate 
## but do we want false alarms instead?

# difference in RT for low->high occlusion in present vs absent
task_df2 %>%
  dplyr::group_by(present, hide_proportion) %>%
  dplyr::summarise(median_RT = median(RT)) %>%
  ggplot(aes(x=hide_proportion, y=median_RT, color=present)) +
  geom_point() +
  geom_line(aes(group = present)) + 
  labs(x = "Proportion Hidden", 
       y = "Median RT (ms)", 
       color = "Target presence") +
  scale_color_manual(values = c("0" = "red", "1" = "blue"),
                     labels = c("0" = "Absent", "1" = "Present"))

  ## dplyr::summarise(diff_occlusion = median_RT[hide_proportion == 0.10] - median_RT[hide_proportion == 0.35])
  ## maybe just run a test when you have more data (check occlusion paper for what type)
  
# difference in confidence for low->occlusion in present vs absent
task_df2 %>%
  dplyr::group_by(present, hide_proportion) %>%
  dplyr::summarise(mean_confidence = mean(confidence)) %>%
  ggplot(aes(x=hide_proportion, y=mean_confidence, color=present)) +
  geom_point() +
  geom_line(aes(group = present)) + 
  labs(x = "Proportion Hidden", 
       y = "Mean confidence", 
       color = "Target presence") +
  scale_color_manual(values = c("0" = "red", "1" = "blue"),
                     labels = c("0" = "Absent", "1" = "Present"))
  
# difference in accuracy for low->high occlusion in present vs absent
task_df2 %>%
  dplyr::group_by(present, hide_proportion) %>%
  dplyr::summarise(
    total_trials = n(),
    correct_trials = sum(correct),
    proportion_correct = correct_trials/total_trials
  ) %>%
  ggplot(aes(x=hide_proportion, y=proportion_correct, color=present)) +
  geom_point() +
  geom_line(aes(group = present)) + 
  labs(x = "Proportion Hidden", 
       y = "Accuracy", 
       color = "Target presence") +
  scale_color_manual(values = c("0" = "red", "1" = "blue"),
                     labels = c("0" = "Absent", "1" = "Present"))

  ## again do we want false alarm rates instead? 
