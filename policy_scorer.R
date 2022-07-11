
# necessary packages for cleaning and getting scoring data from my google sheet

{
  library(tidyverse)
  library(janitor)
  library(googlesheets4)
  library(geosphere)
}

# reading policies from the saved csv

policy_df <- read_csv('raw-data/scraped/scraped_policies.csv')

# getting info on the different levels for each variable

gs4_deauth()
policy_levels <- read_sheet('https://docs.google.com/spreadsheets/d/1fs9IIEJaZjr4O1pQCCNhaaMRccI4Pyg5ONC_OZ0n7Z8/edit#gid=657372929',
                      sheet = 'score_key') %>% 
  clean_names() 

# now I want to calculate a value to add to the hostility score based on a
# state's harshness in each area

score_key <- policy_levels %>% 
  group_by(policy) %>% 
  mutate(n_ranks = max(harsh_rank),
         score = (n_ranks-harsh_rank+1) / n_ranks) %>% 
  ungroup() %>% 
  select(-c(harsh_rank, n_ranks))

score_key %>% 
  select(-policy) %>% 
  mutate(score = round(score, 3)) %>% 
  knitr::kable(booktabs = TRUE,
               caption = 'Points allotted to policy levels') %>% 
  kableExtra::pack_rows(index = table(fct_inorder(score_key$policy))) %>% 
  kableExtra::kable_minimal()

# working on cleaning to get rid of non-alphanumeric characters (but then
# putting the triangles back)... also getting rid of Alaska & Hawaii since their
# distance measurementswill likely bias any analyses

clean <- policy_df %>% 
  filter(!state %in% c('HI', 'AK')) %>% 
  mutate(across(licensed_physician:parental_involv_minors, 
                function(x) str_remove_all(x, '[^a-zA-Z0-9 -]'))) %>% 
  mutate(across(licensed_physician:parental_involv_minors,
                function(x) ifelse(x == '', 'Enjoined', x))) %>% 
  mutate(across(licensed_physician:parental_involv_minors, 
                function(x) replace_na(x, '0'))) %>% 
  mutate(fund_life_rape_incest = str_replace(fund_life_rape_incest,
                                             ' Onl$', ' Only'),
         parental_involv_minors = str_replace(parental_involv_minors,
                                              '^onsent and Notic$',
                                              'Consent and Notice'))

# writing this file to CSV so I can easily have the cleaned scores alongside the
# point values for each policy level

clean_policies <- clean %>% 
  pivot_longer(licensed_physician:parental_involv_minors, 
               names_to = 'policy', values_to = 'level') %>% 
  left_join(score_key, by = c('policy', 'level')) %>% 
  mutate(score = case_when(level %in% c('0', 'Enjoined') ~ 0,
                           TRUE ~ score)) 
write_csv(clean_policies, 'raw-data/clean_policies.csv')

# seeing how many unique levels there are of each policy

clean_policies %>% 
  count(policy, level, score)

# now I want to use the score key alongside the policy_df to calculate a
# hostility score for each state... 

within_scores_df <- clean %>% 
  pivot_longer(licensed_physician:parental_involv_minors, 
               names_to = 'policy', values_to = 'level') %>% 
  left_join(score_key, by = c('policy', 'level')) %>% 
    mutate(score = case_when(level %in% c('0', 'Enjoined') ~ 0,
                             TRUE ~ score)) %>% 
  group_by(state, year) %>% 
  summarize(hostility_score = sum(score),
            .groups = 'drop') %>% 
  arrange(desc(hostility_score)) 

# throwing it up on a graph so I can view change over time

within_scores_df %>% 
  ggplot(aes(year, hostility_score)) +
  geom_line() +
  facet_wrap(~state)

# seeing which states have seen the largest magnitude of change over the years

within_scores_df %>% 
  group_by(state) %>% 
  mutate(annual_chg = hostility_score - lag(hostility_score)) %>% 
  summarise(abs_chg = sum(abs(annual_chg), na.rm = TRUE)) %>% 
  arrange(desc(abs_chg))

# trying to get distances between states so I can construct spatially-weighted scores
# using 2020 state centers of population data from Census

pop_centers <- read.delim('https://www2.census.gov/geo/docs/reference/cenpop2020/CenPop2020_Mean_ST.txt') %>% 
  separate(STATEFP.STNAME.POPULATION.LATITUDE.LONGITUDE, sep = ',',
           into = c('statefip', 'state', 'population', 'latitude', 'longitude')) %>% 
  mutate(across(c(latitude, longitude), as.numeric))

dist_matrix <- distm(cbind(pop_centers$longitude, pop_centers$latitude))
colnames(dist_matrix) <- pop_centers$state

dist_df <- dist_matrix %>% 
  as_tibble() %>% 
  mutate(state = pop_centers$state) %>% 
  select(state, everything()) 

# want the policies to be a factor of inverse distance & population...
# normalized to mean of 0 and sd of 1

pops <- read_csv('raw-data/downloads/statepops2010_2019.csv') %>% 
  clean_names() %>% 
  select(name, popestimate2010:popestimate2019) %>% 
  rename(state = name) %>% 
  pivot_longer(popestimate2010:popestimate2019, names_to = 'year', values_to = 'pop') %>% 
  mutate(year = parse_number(year)) %>% 
  filter(!state %in% c('Hawaii', 'Alaska'))

# population / (dist^2) to get the weight... making sure Hawaii and Alaska are
# removed because they would severely bias the standard deviations without
# providing any substantive contribution to the research question

weight_df <- dist_df %>% 
  select(-c(Hawaii, Alaska)) %>% 
  filter(!state %in% c('Hawaii', 'Alaska', 'Puerto Rico')) %>% 
  mutate(across(Alabama:ncol(.), function(x) 1 / x^2)) %>% 
  mutate(across(Alabama:ncol(.), function(x) ifelse(is.infinite(x), 0, x))) %>% 
  pivot_longer(Alabama:ncol(.), names_to = 'state2', values_to = 'weight') %>%
  right_join(pops, by = c('state2' = 'state')) %>% 
  mutate(weight = weight * pop) %>% 
  select(-pop)
  
# joining weight df with score df, computing score by summing weights*intrastate
# score across all 50 states for each state & then normalizing... making sure
# weights sum to 1 for each state/year pairing

surrounding_scores_df <- weight_df %>% 
  mutate(state2 = state.abb[match(state2, state.name)]) %>% 
  inner_join(within_scores_df, by = c('state2' = 'state', 'year')) %>% 
  group_by(state, year) %>% 
  mutate(weight_norm = weight / sum(weight)) %>% 
  summarise(surrounding_score = sum(weight_norm * hostility_score),
            .groups = 'drop') 
# %>%
#   mutate(surrounding_score = (surrounding_score - mean(surrounding_score)) /
#            sd(surrounding_score))

# combining all scores

all_scores <- within_scores_df %>% 
  mutate(state = state.name[match(state, state.abb)]) %>% 
  inner_join(surrounding_scores_df, by = c('state', 'year')) %>% 
  rename(within_score = hostility_score)

# seeing the distribution of scores... I think it makes sense that intrastate is
# more symmetric/bell-shaped since it's a distribution of weighted averages

all_scores %>% 
  pivot_longer(within_score:surrounding_score) %>% 
  ggplot(aes(value)) +
  geom_histogram() +
  facet_wrap(~name, scales = 'free')

all_scores %>% 
  filter(is.na(surrounding_score))

# saving this df

write_csv(all_scores, 'raw-data/policy_scores.csv')

