library(tidyverse)
library(ggplot2)

# load df
df <- read.csv('../experiments/pilots/pilot1/data/jatos_results_data_batch1.csv') 

# filter relevant content
task_df <- df %>%
  filter(trial_type=='jsShapes') %>%
  dplyr::select(PROLIFIC_PID,shapes,occluder,options,response,RT,confidence,confidence_RT) %>%
  rename(subj_id=PROLIFIC_PID) %>%
  mutate(decision=ifelse(response==0,as.numeric(substr(options,2,2)),as.numeric(substr(options,4,4))),
         RT=as.numeric(RT),
         confidence=as.numeric(confidence))

# Are participants more confident making decisions when the occluder is on the right?
task_df %>%
  group_by(subj_id, occluder) %>%
  summarise(confidence=mean(confidence, na.rm=T)) %>%
  ggplot(aes(x=occluder,y=confidence,color=as.factor(subj_id)))+
  geom_point()

#yes!
  

