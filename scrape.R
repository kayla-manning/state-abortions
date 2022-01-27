library(tidyverse)
library(rvest)
library(janitor)

base <- 'http://www.johnstonsarchive.net/policy/abortion'
page <- read_html(base)
links <- html_nodes(page, 'a') %>% 
  html_attr('href')

# want to only get the links with the href 'usac/ab-usac-' tag... use ab-usac2
# if you want the rates rather than the counts

county_links <- vector()
for (i in 1:length(links)) {
  if (!is.na(links[i])) {
    if (str_detect(links[i], 'usac/ab-usac-') & str_detect(links[i], '[A-Z]')) {
      county_links <- c(county_links, links[i])
    }
  }
}

# extracting tables from each link... will put the corresponding title as a
# column with the data source so that I can tidy later on

{
  all_states <- tibble()
  for (s in 1:length(county_links)) {
    
    # extracting info for this state's tables
    
    this_state <- read_html(paste(base, county_links[s], sep = '/')) 
    tables <- this_state %>% 
      html_table(fill = TRUE)
    titles <- this_state %>% 
      html_nodes('b') %>% 
      html_text()
    
    # putting it all together for this state
    
    state_combined <- tibble()
    for (i in 1:length(tables)) {
      this_data <- tables[[i]][!duplicated(as.list(tables[[i]]))] %>% 
        mutate(state_info = titles[1],
               data_type = titles[i+1]) %>% 
        clean_names()
      state_combined <- bind_rows(state_combined, this_data)
    }
    
    # doing some preliminary cleaning... converting appropriate columns to
    # numeric, adding column with state name, adding year range, and adding column
    # to indicate type of data
    
    state_name <- sapply(str_extract_all(county_links[s], "\\b[A-Z]+\\b"), paste, collapse= ' ')
    state_cleaned <- state_combined %>%
      mutate(across(starts_with('x'), function(x) as.numeric(str_remove_all(x, ',')))) %>% 
      mutate(state = state_name,
             year_range = word(state_info, -1),
             data_type = ifelse(str_detect(data_type, 'abortions'), 
                                'abortions',
                                'live_births')) %>% 
      select(state, data_type, county, year_range, everything()) %>% 
      select(-c(state_info)) %>% 
      pivot_longer(starts_with('x'), names_to = 'year') %>% 
      mutate(year = as.numeric(str_remove(year, '^x'))) %>% 
      pivot_wider(names_from = data_type, values_from = value) %>% 
      unnest(c(abortions, live_births)) %>% 
      drop_na(abortions)
    all_states <- bind_rows(all_states, state_cleaned)
  }
}

###########################################################################
###########################################################################
###########################################################################

colnames <- c('State', 'State Code', 'County', 'County Code',
  'Births	% of Total Births', 'Total Population	Birth Rate',
  'Female Population	Fertility Rate',	'Average Birth Weight',
  'Standard Deviation for Average Birth Weight',	
  'Average Age of Mother',	'Standard Deviation for Average Age of Mother',
  'Average LMP Gestational Age',	
  'Standard Deviation for Average LMP Gestational Age',	
  'Average OE Gestational Age',	
  'Standard Deviation for Average OE Gestational Age')
library(tidyverse)
colnames <- sapply(colnames, function(x) 
  str_replace_all(str_to_lower(x), ' ', '_'))

library(janitor)

dat <- tibble()
for (i in 2007:2020) {
  filename <- paste0('raw-data/wonder_downloads/natality_', i, '.txt')
  temp <- read.delim(filename) %>% 
    clean_names() %>% 
    mutate(year = i)
  dat <- bind_rows(dat, temp)
}

View(dat)
