
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
######## POLICY CATEGORIES
###############################

{
  # putting states in categories similar to Kaufman et al.
  
  category_placements <- policy_scores %>% 
    pivot_longer(intrastate_score:interstate_score) %>% 
    group_by(name) %>% 
    mutate(first_cutoff = quantile(value, 1/3),
           second_cutoff = quantile(value, 2/3)) %>% 
    mutate(low = value <= first_cutoff,
           med = value > first_cutoff & value <= second_cutoff,
           high = value > second_cutoff) %>% 
    select(-c(first_cutoff, second_cutoff, value)) %>% 
    pivot_longer(low:high, names_to = 'category', values_to = 'placement') %>% 
    filter(placement) %>% 
    select(-placement) %>% 
    pivot_wider(names_from = name, values_from = category) %>% 
    mutate(within_between = paste(intrastate_score, interstate_score, sep = '-'),
           within_between = fct_relevel(within_between,
                                        levels = c('low-low', 'low-med', 'low-high',
                                                   'med-low', 'med-med', 'med-high',
                                                   'high-low', 'high-med', 'high-high'))) %>% 
    select(-c(intrastate_score, interstate_score))
}

###############################
######## DEPENDENT VARS
###############################

{
  # getting the individual dependent variables
  {
    # import-export ratio
    {
      # getting number of non-resident abortions obtained within X state (i.e.
      # abortions imported from other states)
      
      imports <- abortion_long %>% 
        filter(state_location != state_residence,
               !str_detect(state_residence, 'Total')) %>% 
        group_by(state_location, year) %>% 
        summarize(imports = sum(count, na.rm = TRUE),
                  .groups = 'drop') %>% 
        distinct()
      
      # getting number of abortions obtained by residents of X state in OTHER states
      # (i.e. abortions exported to other states)
      
      exports <- abortion_long %>% 
        filter(state_location != state_residence,
               !str_detect(state_residence, 'Total')) %>% 
        group_by(state_residence, year) %>% 
        summarize(exports = sum(count, na.rm = TRUE),
                  .groups = 'drop') %>% 
        distinct()
      
      # computing import/export ratio for each state & year, with import and export
      # defined above... need to exclude CO from 2018 because it does not have any
      # exports reported
      
      ie_df <- inner_join(imports, exports, 
                          by = c('state_location' = 'state_residence', 
                                 'year')) %>% 
        filter(exports != 0) %>%
        mutate(ie_ratio = imports / exports) %>% 
        filter(!state_location %in% c('Hawaii', 'Alaska', 'District of Columbia'),
               imports != 0)
    }
    
    # non-resident to resident ratio
    {
      nonres_res_df <- abortion_long %>% 
        mutate(resident = state_location == state_residence) %>% 
        group_by(resident, state_location, year) %>% 
        summarise(count = sum(count, na.rm = TRUE),
                  .groups = 'drop') %>% 
        pivot_wider(names_from = resident, values_from = count, names_prefix = 'resident') %>% 
        mutate(nonres_res_ratio = residentFALSE / residentTRUE) %>% 
        filter(!state_location %in% c('Hawaii', 'Alaska', 'District of Columbia')) %>% 
        select(-c(residentFALSE:residentNA))
    }
    
    # abortion rates
    {
      abortion_rates <- abortion_long %>% 
        inner_join(births, by = c('state_location' = 'state', 'year')) %>% 
        group_by(state_location, year, births, total_population) %>% 
        filter(str_detect(state_residence, 'Total')) %>% 
        summarize(abortion_per_1k_births = count / (births / 1000),
                  abortion_per_100k_pop = count / (total_population / 100000),
                  .groups = 'drop') %>% 
        filter(!state_location %in% c('Hawaii', 'Alaska', 'District of Columbia'))
    }
    
    # late-to-early ratio
    {
      # now I want to get counts for pre13 and post13... since we have these missing
      # counts, I will subtract total_report - x0_13 to get the post13 (which will
      # hopefully pick up on the abortions that were excluded for having low counts in
      # specific windows)... dropping Wyoming & SD 2019 (missing data)
      
      late_early_df <- gestation %>% 
        filter(!state %in% c('Hawaii', 'Alaska', 'District of Columbia'),
               !is.na(x0_13)) %>% 
        mutate(post13 = total_reported - x0_13,
               prop_late = post13 / total_reported) %>% 
        rename(pre13 = x0_13) %>% 
        select(state, year, pre13, post13) 
      
    }
  }
  
  # combining everything
  {
    # combining all of the dataframes with data on dependent variables
    
    dep_var_df <- inner_join(ie_df, abortion_rates, by = c('state_location', 'year')) %>% 
      inner_join(late_early_df, by = c('state_location' = 'state', 'year')) %>% 
      mutate(late_to_early = post13 / pre13) %>% 
      inner_join(nonres_res_df, by = c('state_location', 'year'))
    
  }
}

###############################
######## CONTROLS
###############################

{
  # reading in the individual dfs
  {
    # presidential data for a control variable
    
    prez <- read_csv('raw-data/downloads/presidential_votes.csv') %>% 
      filter(!writein) %>% 
      filter(party_simplified %in% c('DEMOCRAT', 'REPUBLICAN')) %>% 
      select(year, state, party_simplified, candidatevotes) %>% 
      pivot_wider(names_from = party_simplified, values_from = candidatevotes) %>% 
      mutate(dem_2party = DEMOCRAT / (DEMOCRAT + REPUBLICAN),
             state = str_replace_all(str_to_title(state), ' Of ', ' of ')) %>% 
      select(year, state, dem_2party) 
    
    # cleaning the income data... filtering to get appropriate year data when
    # there's a clear right answer (2017)... when there's conflicting numbers,
    # I will take a weighted average based on the addresses surveyed for each
    # (2013)
    
    
    income <- read_excel('raw-data/downloads/household_income.xlsx', skip = 7) %>% 
      clean_names() %>% 
      select(state, seq(2, ncol(.), by = 2)) %>% 
      slice(-1) %>% 
      slice(1:52) %>% 
      mutate(across(x2020:ncol(.), as.numeric)) %>% 
      mutate(x2013 = (68/98) * x2013_38 + (30/98) * x2013_39) %>% 
      pivot_longer(x2020:ncol(.), names_to = 'year', values_to = 'hh_income') %>% 
      filter(!year %in% c('x2017', 'x2013_38', 'x2013_39')) %>% 
      mutate(year = str_replace_all(str_remove_all(year, 'x'), '_', ' '),
             year = as.numeric(str_sub(year, 1, 4)))
    
    # getting population isolated to make life easier
    
    pops <- select(births, state, year, total_population)
    
    # education for % with bachelor's degree
    
    pct_bachelors <- read_excel('raw-data/downloads/bachelors-degree-holders-per-25-44-year-olds.xlsx', 
                                skip = 2) %>% 
      clean_names() %>% 
      select(x1, 
             bachelors_degree_holders_individuals_25_44_years_old_percent:ncol(.)) %>% 
      row_to_names(1) %>% 
      clean_names() %>% 
      select(state, x2010:x2019) %>% 
      pivot_longer(x2010:x2019, names_to = 'year', values_to = 'pct_bachelors') %>% 
      mutate(year = parse_number(year))
    
    # race / ethnicity (want percent non-white and percent hispanic)...
    # origin==2 is Hispanic, race==1 means white alone or in combination, and
    # origin==0 is total Hispanic+non-Hispanic
    
    {
      hisp_or_white <- read_csv('raw-data/downloads/state_race_ethnicity.csv') %>% 
        clean_names() %>% 
        filter(sex == 0,
               (origin == 2 | race == 1)) %>% 
        select(-c(census2010pop:estimatesbase2010)) %>%
        pivot_longer(popestimate2010:popestimate2019, names_to = 'year', 
                     values_to = 'popestimate') %>% 
        group_by(name, origin, race, year) %>% 
        summarise(popestimate = sum(popestimate)) %>% 
        ungroup() %>% 
        mutate(year = parse_number(year)) %>% 
        rename(state = name)
      race_controls <- hisp_or_white %>% 
        filter(origin == 2) %>% 
        group_by(state, year) %>% 
        summarize(hisp_popestimate = sum(popestimate)) %>% 
        inner_join(hisp_or_white %>% 
                     filter(race == 1, 
                            origin == 0) %>% 
                     group_by(state, year) %>% 
                     summarize(white_popestimate = sum(popestimate)),
                   by = c('state', 'year')) %>% 
        inner_join(pops, by = c('state', 'year')) %>% 
        mutate(prop_hisp = hisp_popestimate / total_population,
               prop_nonwhite = 1 - (white_popestimate / total_population)) %>% 
        select(state, year, prop_hisp, prop_nonwhite)
      }
    
  }
  
  # combining into a single control df
  {
    controls <- pct_bachelors %>% 
      inner_join(pops, by = c('state', 'year')) %>% 
      inner_join(race_controls, by = c('state', 'year')) %>% 
      inner_join(income, by = c('state', 'year')) %>% 
      left_join(dem2party, by = c('state', 'year')) %>% 
      arrange(state, year) %>% 
      mutate(dem_2party = ifelse(year %in% 2010:2011, 
                                 map2_dbl(state, year, 
                                          ~ ifelse(.y %in% 2010:2011, 
                                                   dem2party$dem_2party[dem2party$state == .x &
                                                                          dem2party$year == 2008],
                                                   dem_2party)),
                                 dem_2party),
             dem_2party = na.locf(dem_2party))
  }
}

###############################
######## FINAL DF
###############################

{
  # combining all of our data into a single df
  
  final_df <- dep_var_df %>% 
    right_join(policy_scores, by = c('state_location' = 'state', 'year')) %>% 
    inner_join(category_placements, by = c('state_location' = 'state', 'year')) %>% 
    inner_join(controls, by = c('state_location' = 'state', 'year')) %>% 
    filter(!state_location %in% c('District of Columbia', 'Hawaii', 'Alaska'),
           !str_detect(state_location, 'Total')) %>% 
    select(-total_population.x) %>% 
    rename(state = state_location,
           total_population = total_population.y)
  
  # writing to a csv for easy analysis
  
  write_csv(final_df, 'raw-data/combined_data.csv')
}