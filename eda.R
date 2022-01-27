library(readxl)
library(janitor)
library(tidyverse)
library(lme4)
library(lmerTest)

count_df <- tibble()
years <- sapply(2010:2019, as.character)
for (y in years) {
  data <- read_xls('raw-data/abortions_2010-2019.xls',
                   skip = 1,
                   sheet = y) %>% 
    clean_names() %>% 
    mutate(year = y) %>% 
    select(-1) %>% 
    drop_na(state_area) %>% 
    mutate(across(everything(), function(x) ifelse(x == '--', NA, x))) %>% 
    mutate(across(2:ncol(.), as.numeric)) %>% 
    select(year, everything()) 
  count_df <- bind_rows(count_df, data)
}

# making some graphs to make sure the data works

get_state_graph <- function(state) {
  count_df %>% 
    filter(str_detect(state_area, state)) %>% 
    ggplot(aes(year, total_by_location_of_service)) +
    geom_line() +
    scale_x_continuous(breaks = 2010:2019) +
    theme_minimal() +
    labs(title = paste('Total abortions in', state, 'from 2010 to 2019'),
         x = '',
         y = 'Count')
}
get_state_graph('Texas')
get_state_graph('Alabama')
get_state_graph('Oklahoma')

# merging count_df with ipums draft (need to update the actual ipums df before I
# do any serious analysis with it)

baseline_df <- count_df %>% 
  mutate(state_area = str_remove_all(state_area, fixed('*'))) %>% 
  select(year, state_area, total_by_location_of_service) %>% 
  inner_join(ipums, by = c('year', 'state_area' = 'state')) %>% 
  mutate(abortion_rate = total_by_location_of_service / pop,
         abortion_female_rate = total_by_location_of_service / pop_female) %>% 
  select(-c(total_by_location_of_service, pop, pop_female))

# fitting a full model & stepping to get a better model

lmfull <- lm(abortion_rate ~ .-abortion_female_rate, data = baseline_df)
lmstep <- step(lmfull, direction = 'both', trace = 0)
formula(lmstep)

# # testing to see if it's any different using abortion as prop of female
# # population... it returns the same model. since I have a term in there for
# # proportion of female, I will use the overall abortion rate
# 
# lmfull2 <- lm(abortion_female_rate ~ .-abortion_rate, data = baseline_df)
# lmstep2 <- step(lmfull2, direction = 'both', trace = 0)
# formula(lmstep2)

# now going to fit a mixed-effects model with random intercepts for state (using
# controls identified by stepwise regression)

lmerstep <- lmer(abortion_rate ~ year + prop_pop_btwn_18_45 + prop_married + 
                   prop_blk + prop_wht + prop_priv_hcov + (1 | state_area),
                  data = baseline_df)
summary(lmerstep)

# next step is getting policy data and incorporating that as a term in this regression


