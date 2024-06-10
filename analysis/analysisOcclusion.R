library(tidyverse)
library(ggplot2)

setwd('C:/Users/schip/Desktop/dissertation experiments/shapesInference/experiments/pilots/pilotRowOcclusion/data')

# load df
df5 <- read.csv('jatos_results_data_firstparticipant.csv', sep = ",", header = TRUE) 

task_df2 <- df5 %>%
  dplyr::filter(trial_type=='noisyLetter'& (test_part=='test1' | test_part=='test2')) %>%
  dplyr::select(RT, hide_proportion, present, correct, confidence, confidence_RT) %>%
  dplyr::mutate(
    subj_id = '5e9fd839311b8018f88c99eb',
    RT=as.numeric(RT),
    confidence_RT=as.numeric(confidence_RT),
    confidence=as.numeric(confidence),
    hide_proportion=as.factor(hide_proportion),
    ## occlusion = as.factor(ifelse(hide_proportion == 0.10, 'low', 'high'))
    present=as.factor(present),
    correct = ifelse(correct == 'true', TRUE, FALSE))

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
