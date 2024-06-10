library(tidyverse)
library(dplyr)
library(ggplot2)

setwd('C:/Users/schip/Desktop/dissertation experiments/shapesInference/experiments/pilots')

# read-in data from letter task 
batch6 <- read.csv('pilotRowOcclusion1b/data/jatos_results_data_batch2.csv', sep = ",", header = TRUE) 
batch7 <- read.csv('pilotRowOcclusion1c/data/jatos_results_data_batch1.csv', sep = ",", header = TRUE)
batch8 <- read.csv('pilotRowOcclusion1d/data/jatos_results_data_batch1.csv', sep = ",", header = TRUE)
batch9 <- read.csv('pilotRowOcclusion2/data/jatos_results_data_batch1.csv', sep = ",", header = TRUE)

# bind data
df <- bind_rows(batch6,batch7,batch8,batch9)

# tidy
raw_df <- df %>%
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
low_accuracy <- raw_df %>%
  dplyr::group_by(subj_id) %>%
  dplyr::summarise(
    accuracy = mean(correct)) %>%
  dplyr::filter(accuracy<0.5) %>%
  dplyr::pull(subj_id)

too_slow <- raw_df %>%
  dplyr::group_by(subj_id) %>%
  dplyr::summarise(
    third_quartile_RT = quantile(RT,0.75)) %>%
  dplyr::filter(third_quartile_RT>7000) %>%
  dplyr::pull(subj_id)

too_fast <- raw_df %>%
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

task_df <- raw_df %>%
  filter(!(subj_id %in% to_exclude))

# effect of presence/absence on reaction time
response_RT <- task_df %>%
  dplyr::filter(RT > 100 & RT < 7000 & correct == TRUE) %>%  
  dplyr::group_by(subj_id, present) %>%
  dplyr::summarise(RT = median(RT)) %>%
  tidyr::pivot_wider(names_from = present, values_from = RT) %>%
  dplyr::mutate(response_RT = `1` - `0`)
t.test(response_RT$response_RT)
mean(response_RT$`0`)
mean(response_RT$`1`)

# occlusion effect on RT in presence
RT_occlusion_presence <- task_df %>%
  dplyr::filter(RT > 100 & RT < 7000 & present == 1 & correct == TRUE) %>%
  dplyr::group_by(subj_id, hide_proportion) %>%
  dplyr::summarise(RT = median(RT))%>%
  tidyr::pivot_wider(names_from = hide_proportion, values_from = RT) %>%
  dplyr::mutate(RT_occlusion_presence = `0.35` - `0.1`)
t.test(RT_occlusion_presence$RT_occlusion_presence)
mean(RT_occlusion_presence$`0.1`)
mean(RT_occlusion_presence$`0.35`)

# occlusion effect on RT in absence
RT_occlusion_absence <- task_df %>%
  dplyr::filter(RT > 100 & RT < 7000 & present == 0 & correct == TRUE) %>%
  dplyr::group_by(subj_id, hide_proportion) %>%
  dplyr::summarise(RT = median(RT)) %>%
  tidyr::pivot_wider(names_from = hide_proportion, values_from = RT) %>%
  dplyr::mutate(RT_occlusion_absence = `0.35` - `0.1`)
t.test(RT_occlusion_absence$RT_occlusion_absence)
mean(RT_occlusion_absence$`0.1`)
mean(RT_occlusion_absence$`0.35`)

# occlusion response interaction on reaction time 
RT_occlusion_response <- inner_join(
  RT_occlusion_presence, 
  RT_occlusion_absence,
  by = "subj_id") %>%
  mutate(RT_occlusion_response = RT_occlusion_presence - RT_occlusion_absence)
t.test(RT_occlusion_response$RT_occlusion_response)

# effect of presence/absence on confidence
response_confidence <- task_df %>%
  dplyr::filter(correct == TRUE) %>%
  dplyr::group_by(subj_id, present) %>%
  dplyr::summarise(confidence=mean(confidence)) %>%
  tidyr::pivot_wider(names_from = present, values_from = confidence) %>%
  dplyr::mutate(response_confidence = `1` - `0`)
t.test(response_confidence$response_confidence)
mean(response_confidence$`0`)
mean(response_confidence$`1`)

# occlusion effect on confidence in presence
conf_occlusion_presence <- task_df %>%
  dplyr::filter(present == 1 & correct == TRUE) %>%
  dplyr::group_by(subj_id, hide_proportion) %>%
  dplyr::summarise(confidence = mean(confidence)) %>%
  tidyr::pivot_wider(names_from = hide_proportion, values_from = confidence) %>%
  dplyr::mutate(conf_occlusion_presence = `0.35` - `0.1`)
t.test(conf_occlusion_presence$conf_occlusion_presence)
mean(conf_occlusion_presence$`0.1`)
mean(conf_occlusion_presence$`0.35`)

# occlusion effect on confidence in absence
conf_occlusion_absence <- task_df %>%
  dplyr::filter(present == 0 & correct == TRUE) %>%
  dplyr::group_by(subj_id, hide_proportion) %>%
  dplyr::summarise(confidence = mean(confidence)) %>%
  tidyr::pivot_wider(names_from = hide_proportion, values_from = confidence) %>%
  dplyr::mutate(conf_occlusion_absence = `0.35` - `0.1`)
t.test(conf_occlusion_absence$conf_occlusion_absence)
mean(conf_occlusion_absence$`0.1`)
mean(conf_occlusion_presence$`0.35`)

# occlusion response interaction on confidence 
conf_occlusion_response <- inner_join(
  conf_occlusion_presence,
  conf_occlusion_absence,
  by = "subj_id") %>%
  mutate(conf_occlusion_response = conf_occlusion_presence - conf_occlusion_absence)
t.test(conf_occlusion_response$conf_occlusion_response)

# general accuracy measures 
accuracy_occlusion <- task_df %>%
  dplyr::group_by(subj_id, hide_proportion) %>%
  dplyr::summarise(
    hit_rate = (sum(correct & present)+0.5)/(sum(present)+1),
    fa_rate = (sum(!correct & !present)+0.5)/(sum(!present)+1),
    d = qnorm(hit_rate)-qnorm(fa_rate),
    c = -0.5*(qnorm(hit_rate)+qnorm(fa_rate)))

# d prime 
dprime <- accuracy_occlusion %>%
  dplyr::select(subj_id, hide_proportion,d) %>%
  tidyr::pivot_wider(names_from=hide_proportion, values_from=d) %>%
  dplyr::mutate(dprime_occlusion=`0.1`-`0.35`)
t.test(dprime$dprime_occlusion)
mean(dprime$`0.1`)
mean(dprime$`0.35`)

## driven by hit and fa rate so still calculate those as well
hit_rate_occlusion <- accuracy_occlusion %>%
  dplyr::select(subj_id,hide_proportion,hit_rate) %>%
  tidyr::pivot_wider(names_from = hide_proportion, values_from = hit_rate) %>%
  dplyr::mutate(hit_rate_occlusion = `0.1` - `0.35`)
t.test(hit_rate_occlusion$hit_rate_occlusion)

fa_rate_occlusion <- accuracy_occlusion %>%
  dplyr::select(subj_id,hide_proportion,fa_rate) %>%
  tidyr::pivot_wider(names_from = hide_proportion, values_from = fa_rate) %>%
  dplyr::mutate(fa_rate_occlusion = `0.1` - `0.35`)
t.test(fa_rate_occlusion$fa_rate_occlusion)

# criterion 
criterion <- accuracy_occlusion %>%
  dplyr::select(subj_id, hide_proportion,c) %>%
  tidyr::pivot_wider(names_from=hide_proportion, values_from=c) %>%
  dplyr::mutate(criterion_occlusion=`0.1`-`0.35`)
t.test(criterion$criterion_occlusion)
mean(criterion$`0.1`)
mean(criterion$`0.35`)

# plots 

## RT

### effects by subject
(RT_plot <- task_df %>%
    dplyr::filter(RT > 100 & RT < 7000 & correct == TRUE) %>%
    dplyr::mutate(response = factor(ifelse(response, 'present','absent'),
                                    levels=c('present','absent')),
                  hide_proportion <- as.factor(hide_proportion)) %>%
    dplyr::group_by(subj_id, hide_proportion, response) %>%
    dplyr::summarise(RT = median(RT)) %>%
    ggplot(aes(x=hide_proportion, y = RT, color = response, group = subj_id)) +
    geom_point() +
    facet_wrap(~response) +
    geom_line() + 
    labs(x = "Proportion Hidden", 
         y = "Median RT (ms)") +
    scale_color_manual(values = c("absent" = "red", "present" = "blue")) +
    theme_bw())

### overall effects 
(RT_plot_overall <- task_df %>%
    dplyr::filter(RT > 100 & RT < 7000 & correct == TRUE) %>%
    dplyr::mutate(response = factor(ifelse(response, 'present','absent'),
                                    levels=c('present','absent')),
                  hide_proportion <- as.factor(hide_proportion)) %>%
    dplyr::group_by(subj_id, hide_proportion, response) %>%
    dplyr::summarise(RT = median(RT)) %>%
    dplyr::group_by(hide_proportion, response) %>%
    dplyr::summarise(mean_RT = mean(RT)) %>% # mean of indiv median
    ggplot(aes(x=hide_proportion, y = mean_RT, color = response)) +
    geom_point() +
    geom_line(aes(group = response)) + 
    labs(x = "Proportion Hidden", 
         y = "RT (ms)", 
         color = "Target presence") +
    scale_color_manual(values = c("absent" = "red", "present" = "blue")) +
    theme_bw())

## confidence 

### effects per individual 
(confidence_plot <- task_df %>%
    dplyr::filter(correct == TRUE) %>%
    dplyr::mutate(response = factor(ifelse(response, 'present','absent'),
                                    levels=c('present','absent')),
                  hide_proportion <- as.factor(hide_proportion)) %>%
    dplyr::group_by(subj_id, hide_proportion, response) %>%
    dplyr::summarise(confidence = mean(confidence)) %>%
    ggplot(aes(x=hide_proportion, y = confidence, color = response, group = subj_id)) +
    geom_point() +
    facet_wrap(~response) +
    geom_line() + 
    labs(x = "Proportion Hidden", 
         y = "Confidence") +
    scale_color_manual(values = c("absent" = "red", "present" = "blue")) +
    theme_bw())

### overall effects 
(confidence_plot_overall <- task_df %>%
    dplyr::filter(correct == TRUE) %>%
    dplyr::mutate(response = factor(ifelse(response, 'present','absent'),
                                    levels=c('present','absent')),
                  hide_proportion <- as.factor(hide_proportion)) %>%
    dplyr::group_by(hide_proportion, response) %>%
    dplyr::summarise(confidence = mean(confidence)) %>%
    ggplot(aes(x=hide_proportion, y = confidence, color = response)) +
    geom_point() +
    geom_line(aes(group = response)) + 
    labs(x = "Proportion Hidden", 
         y = "Confidence", 
         color = "Target presence") +
    scale_color_manual(values = c("absent" = "red", "present" = "blue")) +
    theme_bw())
  
## accuracy

### effects per participant
accuracy_plot <- accuracy_occlusion %>%
  tidyr::pivot_longer(cols = c(hit_rate, fa_rate), names_to = "rate_type", values_to = "rate")

(hit_fa_plot <- ggplot(accuracy_plot, aes(x=hide_proportion, y = rate, color = rate_type, group = subj_id)) +
    geom_point() +
    facet_wrap(~rate_type) +
    geom_line() +
    labs(x = "Hide Proportion", y = "Rate", color = "Hit / False alarm rate") +
    scale_color_manual(values = c("fa_rate" = "red", "hit_rate" = "blue")) +
    theme_bw() +
    theme(legend.position = "none"))

### effects overall 
overall_accuracy <- task_df %>%
  dplyr::group_by(hide_proportion) %>%
  dplyr::summarise(
    hit_rate = (sum(correct & present))/(sum(present)),
    fa_rate = (sum(!correct & !present))/(sum(!present))) %>%
  tidyr::pivot_longer(cols = c(hit_rate, fa_rate), names_to = "rate_type", values_to = "rate")

(hit_fa_plot <- ggplot(overall_accuracy, aes(x=hide_proportion, y = rate, color = rate_type)) +
    geom_point() +
    geom_line(aes(group = rate_type)) +
    labs(x = "Hide Proportion", y = "Rate", color = "Hit / False alarm rate") +
    scale_color_manual(values = c("fa_rate" = "red", "hit_rate" = "blue")) +
    theme_bw())

