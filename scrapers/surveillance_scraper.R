
#############################################
### libraries
#############################################

{
  # packages for data cleaning
  
  library(tidyverse)
  library(lubridate)
  library(janitor)
  
  # used for scraping or reading in the data
  
  library(googlesheets4)
  library(rvest)
  library(tabulizer)
}

#############################################
### reading in data info from sheets
#############################################

# reading in range data from sheets
  
gs4_deauth()


gestational_range <- tibble()
for (y in 2007:2019) {
  
  # getting rid of percent columns & just keeping summed total of abortions at
  # each gestational age
  
  good_cols <- c('x1', 'x8', 'x9_13', 'x14_15', 'x16_17', 'x18_20', 'x21', 'total_reported')
  
  # reading the data in from the google sheets
  
  if (y != 2017) {
    
    # reading in the data I want
    
    sheet_name <- paste0('range_', y)
    
    # the data is formatted slightly differently for years 2014 and later
    
    if (y %in% c(2014, 2016, 2019)) {skip <- 1}
    else if (y %in% c(2015, 2018)) {skip <- 3}
    else (skip <- 2)
    
    # fixing column names for 2019 good cols because the ranges are slightly different
    
    if (y == 2019) {
      good_cols <- c('x1', 'x6', 'x7_9', 'x10_13', 'x14_15', 'x16_17', 
                     'x18_20', 'x21', 'total_reported')
    }
    else if (y == 2006) {
      good_cols <- c('x1', 'x8', 'x9_10', 'x11_12', 'x13_15', 'x16_20', 'x21',
                     )
    }
    
    # finally calling the sheet
    
    this_yr_df <- read_sheet('https://docs.google.com/spreadsheets/d/1r2J7jU_HtME_6OifxmbuGU5QVvWPY1m6SbayFtsuA6Y/edit?usp=sharing',
                       sheet = sheet_name, skip = skip) %>% 
      clean_names()
    
    # renaming the total column
    
    if (length(colnames(this_yr_df)) == 15) {
      colnames(this_yr_df)[14] <- 'total_reported'
    }
    else if (y == 2006) {
      this_yr_df <- rename(this_yr_df, total_reported = total)
    }
    else {
      colnames(this_yr_df)[length(colnames(this_yr_df))] <- 'total_reported'
    }
    
    this_yr_df <- this_yr_df %>% 
      slice(-1) %>% 
      mutate(across(everything(), as.character)) %>% 
      select(good_cols) %>% 
      rename(state = x1) %>% 
      mutate(year = y)
  }
  
  # now I need to manually read in the 2017 data from the saved PDF
  
  else {
    
    # manually reading in 2017 data from the pdf (they did not release a normal
    # report, so I couldn't copy from the HTML & instead needed to manually read
    # in a pdf... also note that the weekly cut-offs are different)
    
    remove_cols <- sapply(2:6, function(x) paste0('x_', x))
    range_2017 <- as.data.frame(extract_tables('raw-data/downloads/gestational_2017.pdf', 
                                               pages = 1)[[1]]) %>% 
      slice(-1) %>% 
      row_to_names(1) %>% 
      clean_names() %>% 
      slice(-1) %>% 
      mutate(across(everything(), function(x) str_remove_all(x, '\\s*\\([^\\)]+\\)'))) %>% 
      separate(x7_9, into = c('x6', 'x7_9'), sep = ' ') %>% 
      rename(x21 = x_7,
             state = x,
             total_reported = gestational_age) %>% 
      select(-remove_cols) %>% 
      mutate(across(everything(), function(x) str_remove_all(x, ','))) %>% 
      mutate(across(x6:ncol(.), parse_number)) %>% 
      mutate(year = 2017) %>% 
      as_tibble()
    
  }
  
  # adding that year's data to the big df
  
  if (y != 2019) {
    gestational_range <- bind_rows(gestational_range, this_yr_df)
  }
  
  # need to figure out something to do with 2019 data
  
  else {
    range_2019 <- this_yr_df %>% 
      mutate(across(everything(), function(x) str_remove_all(x, '\\s*\\([^\\)]+\\)'))) %>% 
      mutate(across(x6:year, parse_number))
  }
}


# now cleaning the big dataframe

test <- gestational_range %>% 
  mutate(across(everything(), function(x) str_remove_all(x, '\\s*\\([^\\)]+\\)'))) %>% 
  mutate(across(x8:year, parse_number))

# previewing the separate dataframes before I combine them

head(test)
head(bind_rows(range_2017, range_2019))

# will combine first 13 weeks for the big dataframe
# we can remove symbols from all state names... the only year with duplicates is
# 2016, but all of the observations are the same in that year, so should just
# drop duplicates

all_combined <- test %>% 
  mutate(x0_13 = x8+x9_13) %>% 
  select(-c(x8, x9_13)) %>% 
  select(state, x0_13, everything()) %>% 
  bind_rows(
    bind_rows(range_2017, range_2019) %>% 
      mutate(x0_13 = x6 + x7_9 + x10_13) %>% 
      select(-c(x6, x7_9, x10_13)) %>% 
      select(state, x0_13, everything())
  ) %>% 
  mutate(state = str_replace_all(state, '[^a-z A-Z 0-9]', ''))

# writing to csv now

write.csv(all_combined, 'raw-data/scraped/gestational_combined.csv')
