
# combining data / creating variables in this script

###############################
######## SETUP
###############################

{
  # loading libraries as needed
  
  {
    # basic cleaning tasks
    
    library(tidyverse)
    library(readxl)
    library(janitor)
    library(zoo)
    
    # dealing with NA data
    
    library(naniar)
    
    # mixed effects modeling
    
    library(lme4)
    
    # helping format tables
    
    library(reshape2)
    library(kableExtra)
    
    # for maps
    
    library(maps)
    library(gganimate)
    library(transformr)
  }
  
  # data <3
  
  {
    # reading in gestational data that I scraped
    
    gestation <- read_csv('raw-data/scraped/gestational_combined.csv') %>% 
      select(-X1) %>% distinct() %>% 
      mutate(state = ifelse(state == 'New York State', 'New York', state))
    
    # looping through the different excel sheets to get the abortion count data
    
    {
      abortion_counts <- tibble()
      for (y in 2010:2019) {
        storage <- read_excel('raw-data/downloads/abortions_2010-2019.xls', 
                              skip = 1, sheet = as.character(y)) %>% 
          select(-1) %>% 
          clean_names() %>% 
          mutate(year = y) %>% 
          rename(state_location = state_area) %>% 
          select(state_location, year, everything())
        abortion_counts <- bind_rows(abortion_counts, storage)
      }
      }
    
    # reading in the birth & policy data
    
    births <- read.delim('raw-data/downloads/births_2007-2020.txt') %>% 
      clean_names() %>% 
      select(-c(notes, state_code, year_code))
    policies <- read_csv('raw-data/clean_policies.csv')
    policy_scores <- read_csv('raw-data/policy_scores.csv')
    
    # presidential data for a control variable
    
    prez <- read_csv('raw-data/downloads/presidential_votes.csv')
    
    # household income for a control variable
    
    income <- read_excel('raw-data/downloads/household_income.xlsx', skip = 7) %>% 
      clean_names()
  }
  
  # cleaning the presidential data
  
  dem2party <- prez %>% 
    filter(!writein) %>% 
    filter(party_simplified %in% c('DEMOCRAT', 'REPUBLICAN')) %>% 
    select(year, state, party_simplified, candidatevotes) %>% 
    pivot_wider(names_from = party_simplified, values_from = candidatevotes) %>% 
    mutate(dem_2party = DEMOCRAT / (DEMOCRAT + REPUBLICAN),
           state = str_replace_all(str_to_title(state), ' Of ', ' of ')) %>% 
    select(year, state, dem_2party) 
  
  # cleaning the income data... filtering to get appropriate year data when
  # there's a clear right answer (2017)... when there's conflicting numbers, I
  # will take a weighted average based on the addresses surveyed for each (2013)
  
  income <- income %>% 
    select(state, seq(2, ncol(.), by = 2)) %>% 
    slice(-1) %>% 
    slice(1:52) %>% 
    mutate(across(x2020:ncol(.), as.numeric)) %>% 
    mutate(x2013 = (68/98) * x2013_38 + (30/98) * x2013_39) %>% 
    pivot_longer(x2020:ncol(.), names_to = 'year', values_to = 'hh_income') %>% 
    filter(!year %in% c('x2017', 'x2013_38', 'x2013_39')) %>% 
    mutate(year = str_replace_all(str_remove_all(year, 'x'), '_', ' '),
           year = as.numeric(str_sub(year, 1, 4)))
}

###############################
######## ABORTION LONG
###############################

{
  # replacing missing data with NA values
  
  abortion_counts <- abortion_counts %>% 
    mutate(across(everything(), function(x) na_if(x, '--')))
  
  # converting df to long format
  
  abortion_long <- abortion_counts %>% 
    pivot_longer(alabama:ncol(.), 
                 names_to = 'state_residence', 
                 values_to = 'count') %>% 
    mutate(state_residence = str_replace_all(state_residence, '_', ' ')) %>% 
    mutate(across(c(state_location, state_residence), str_to_title),
           count = as.integer(count)) %>% 
    mutate(across(c(state_location, state_residence), 
                  function(x) str_replace(x, ' Of ', ' of ')))
}

###############################
######## POLICY SCORES
###############################

{
  
}

###############################
######## CATEGORY PLACEMENTS
###############################

###############################
######## DEPENDENT VARS
###############################

###############################
######## CONTROLS
###############################

# education



###############################
######## FINAL DF
###############################

{
  # combining all of our data into a single df
  
  final_df <- dep_var_df %>% 
    right_join(policy_scores, by = c('state_location' = 'state', 'year')) %>% 
    inner_join(category_placements, by = c('state_location' = 'state', 'year')) %>% 
    left_join(dem2party, by = c('state_location' = 'state', 'year')) %>% 
    arrange(state_location, year) %>% 
    mutate(dem_2party = ifelse(year %in% 2010:2011, 
                               map2_dbl(state_location, year, 
                                        ~ ifelse(.y %in% 2010:2011, 
                                                 dem2party$dem_2party[dem2party$state == .x &
                                                                        dem2party$year == 2008],
                                                 dem_2party)),
                               dem_2party),
           dem_2party = na.locf(dem_2party)) %>% 
    inner_join(income, by = c('state_location' = 'state', 'year'))
  
  # writing to a csv for easy analysis
  
  write_csv(final_df, 'raw-data/combined_data.csv')
}