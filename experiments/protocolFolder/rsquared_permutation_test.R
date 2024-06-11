analysis_df <- df;

get_rsquared_diff <- function(df) {
  
  present_df <- df %>% 
    filter(present==1);
  
  absent_df <- df %>%
    filter(present==0);
  
  present_model <- lm(occlusion_RT~MT+MP+DA, data=present_df) %>%
    summary()
  
  absent_model <- lm(occlusion_RT~MT+MP+DA, data=absent_df) %>%
    summary()
  
  return(absent_model$r.squared-present_model$r.squared)
}

shuffle_present <- function(df) {
   new_df <- df %>%
     group_by(subj_id) %>%
     mutate(flip = rbinom(1,1,0.5),
            old_present =present,
            present = ifelse(flip, as.integer(!present),present))
   return(new_df)
}

true_difference <- get_rsquared_diff(analysis_df);

## generate null distribution

N = 10000; #number of permutations
null_dist = c();

for (i in 1:N) {
  shuffled_df <- shuffle_present(analysis_df);
  shuffled_diff <- get_rsquared_diff(shuffled_df);
  null_dist = c(null_dist, shuffled_diff);
}

#get p_value
p_value <- mean(abs(true_difference)<=abs(null_dist))
