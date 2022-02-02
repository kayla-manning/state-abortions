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

{
  # reading in google sheets file that has the links and info for each policy file I want
  
  gs4_deauth()
  sources <- read_sheet('https://docs.google.com/spreadsheets/d/1fs9IIEJaZjr4O1pQCCNhaaMRccI4Pyg5ONC_OZ0n7Z8/edit#gid=657372929',
                        sheet = 'scrape_policies') %>% 
    clean_names()
  
}

#############################################
### functions
#############################################

{
  # cleans Guttmacher's policy tables in the formatting used for 2006-2022
  
  clean_policy_tables <- function(tables) {
    
      # getting right column names for the tables (click links in column titles at
      # https://web.archive.org/web/20170203044806/https://www.guttmacher.org/state-policy/explore/overview-abortion-laws
      # for descriptions of each)
      
      colnames_pg1 <- c('state', 'licensed_physician', 'hospital_if', 
                        'second_physician_if', 'prohibited_except_death_if',
                        'partial_birth_ban', 'fund_all_most', 'fund_life_rape_incest',
                        'priv_insurance_limit')
      colnames_pg2 <- c('state', 'indiv_prov_refuse', 'instit_prov_refuse', 'mandate_breast_cancer',
                        'mandate_fetal_pain', 'mandate_neg_psych', 'hr_wait_post_counseling',
                        'parental_involv_minors')
      colnames_list <- list(colnames_pg1, colnames_pg2)
      
      # reading in the tables from the link
      
      for (t in 1:length(tables)) {
        test <- tables[[t]] %>% 
          filter(X1 %in% state.abb)
        colnames(test) <- colnames_list[[t]]
        assign(paste0('df_', t), test)
      }
      
      # combining the tables from the separate pages into a single df
      
      this_yr_df <- inner_join(df_1, df_2, by = 'state') 
      return(this_yr_df)
  }
}


#############################################
### scraping 2018-2022
#############################################

{
  # now I want to iterate through links and read in the data
  
  policy_df <- tibble()
  
  for (i in 1:nrow(sources)) {
    
    # scraping the webpage
    
    base <- sources$links[i]
    page <- read_html(base)
    tables <- html_nodes(page, 'table') %>% 
      html_table(fill = TRUE)
   
    if (length(tables) != 0) {   
      # using function I wrote above to clean the Guttmacher data
        
      this_yr_df <- clean_policy_tables(tables) %>% 
        mutate(as_of = sources$as_of_date[i],
               accessed = sources$date_accessed[i])
      
      # adding to an empty df with policies across all of the years
      
      policy_df <- bind_rows(policy_df, this_yr_df)
    }
  }
}

#############################################
### reading in 2006-2016 pdfs from scraper
#############################################

# now reading in data from pdf files that I downloaded from wayback machine

for (y in 2006:2016) {
  
  # reading in table from pdf file that I saved from the internet archive
  
  filepath <- paste0('raw-data/policy_archives/laws_', y, '.pdf')
  
  # creting a vector with the column locations to that tabulizer will correctly
  # parse the columns (the extract_tables() function combines columns if I don't
  # take this step)
  # https://rpubs.com/behzod/tabulizer
  
  col_coords <- list(c(39.27494, 24.88029, 654.54147, 64.64656))
  tables <- extract_tables(filepath, output = 'data.frame',
                           columns = col_coords)
  this_yr_df <- clean_policy_tables(tables) %>% 
    mutate(as_of = sources$as_of_date[sapply(sources$links, function(x) str_detect(x, as.character(y)))])
  policy_df <- bind_rows(policy_df, this_yr_df)
  
}

# adding a year column & other cleaning

policy_df <- policy_df %>% 
  mutate(year = year(as_of)) %>% 
  select(state, year, everything())


