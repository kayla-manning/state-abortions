
# necessary packages for cleaning and getting socring data from my google sheet

library(tidyverse)
library(janitor)
library(googlesheets4)

# reading policies from the saved csv

policy_df <- read_csv('raw-data/scraped_policies.csv') %>% 
  select(-X1)

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

# working on cleaning to get rid of non-alphanumeric characters (but then
# putting the triangles back)

clean <- policy_df %>% 
  mutate(across(licensed_physician:parental_involv_minors, 
                function(x) str_remove_all(x, '[^a-zA-Z0-9 -]'))) %>% 
  mutate(across(licensed_physician:parental_involv_minors,
                function(x) ifelse(x == '', 'Enjoined', x))) %>% 
  mutate(across(licensed_physician:parental_involv_minors, 
                function(x) replace_na(x, 0))) %>% 
  mutate(fund_life_rape_incest = str_replace(fund_life_rape_incest,
                                             ' Onl$', ' Only'),
         parental_involv_minors = str_replace(parental_involv_minors,
                                              '^onsent and Notic$',
                                              'Consent and Notice'))

# now I want to use the score key alongside the policy_df to calculate a
# hostility score for each state... 

scores_df <- clean %>% 
  pivot_longer(licensed_physician:parental_involv_minors, 
               names_to = 'policy', values_to = 'level') %>% 
  left_join(score_key, by = c('policy', 'level')) %>% 
  group_by(state, year) %>% 
  summarize(hostility_score = sum(score, na.rm = TRUE),
            .groups = 'drop') 

# throwing it up on a graph so I can view change over time

scores_df %>% 
  ggplot(aes(year, hostility_score)) +
  geom_line() +
  facet_wrap(~state)

# seeing which states have seen the largest magnitude of change over the years

scores_df %>% 
  group_by(state) %>% 
  mutate(annual_chg = hostility_score - lag(hostility_score)) %>% 
  summarise(abs_chg = sum(abs(annual_chg), na.rm = TRUE)) %>% 
  arrange(desc(abs_chg))


# another idea... I could/should relevel many of the factors to the follow
# categories, as described in the text at
# https://www.guttmacher.org/state-policy/explore/overview-abortion-laws

# physician & hospital requirements:
# - require licensed physician
# - require abortion in a hospital after a specified point
# - require second physician after a specified point

clean %>% 
  mutate(hospital_if = ifelse(hospital_if != '0', 
                              'specified point', hospital_if),
         second_physician_if = ifelse(second_physician_if != '0',
                                      'specified point', second_physician_if))

# gestational limits:
# - prohibit abortions after a specified point
# - some exceptions when necessary for patient's life or health

# ********* need to figure out if I want to include exceptions or not *********

clean %>% 
  mutate(prohibited_except_death_if = ifelse(!prohibited_except_death_if %in% 
                                               c('0', 'Enjoined'),
                                             'specified point',
                                             prohibited_except_death_if))

# "partial-birth" abortions:
# - prohibit "partial-birth" abortion
# - only apply to postviability abortions



# public funding: 
# - use own funds to pay for all/most medically necessary abortions for Medicaid 
#   enrollees in the state 
# - prohibit use of state funds except in cases when federal funds are available 
#   (where patient's life is in danger or the pregnancy is the result of rape or incest) 
# - life endangerment only (SD is the only state that does this & it is in defiance of 
#   federal requirements)

# coverage by private insurance:
# - restrict coverage of abortion in private insurance plans, most often when 
#   the patient's life would be endangered if carried to term

# refusal:
# - allow individual health care providers to refuse to participate in an abortion
# - allow institutions to refuse to perform abortions
# - limit refusal to private or religious institutions

# state-mandated counseling:
# - individuals be given counseling before an abortion that includes information on at least
#   one of the following: (1) breast cancer link, (2) ability of a fetus to feel pain, or
#   (3) long-term mental health consequences for the patient

# waiting periods:
# - person seeking an abortion must wait a specified period of time between counseling and procedure
# - effectively require patient to make two separate tips to the clinic to obtain the procedure

# parental involvement:
# - require some type of parental involvement in a minor's decision to have an abortion 
#   (consent OR notified)
# - require one or both parents to consent to the procedure
# - require that one or both parents be notified



