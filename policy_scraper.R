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
  library(RSelenium)
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
  
  # creating a vector with the column locations to that tabulizer will correctly
  # parse the columns (the extract_tables() function combines columns if I don't
  # take this step)
  # https://rpubs.com/behzod/tabulizer
  
  col_coords <- list(c(39.27494, 24.88029, 654.54147, 64.64656))
  tables <- extract_tables(filepath, output = 'data.frame',
                           columns = col_coords,
                           area = list(c(124.32973, 14.38593, 
                                         693.20274, 585.47094)))
  this_yr_df <- clean_policy_tables(tables) %>% 
    mutate(as_of = sources$as_of_date[sapply(sources$links, function(x) str_detect(x, as.character(y)))])
  policy_df <- bind_rows(policy_df, this_yr_df)
  
}

# adding a year column & other cleaning

policy_df <- policy_df %>% 
  mutate(year = year(as_of)) %>% 
  select(state, year, everything())

base <- sources$links[1]
page <- read_html(base)
tables <- html_nodes(page, 'embed') %>% 
  html_table(fill = TRUE)

# going to read in one column at a time... here's a list of column coordinates for page 1

colcoords_pg1 <- list(c(123.22513, 22.11822, 694.30735, 65.19814),
                  c(124.32973, 64.09352, 694.30735, 118.21957),
                  c(124.3297, 121.5334, 692.0981, 185.6010),
                  c(121.0159, 183.3917, 695.4120, 247.4593),
                  c(122.1205, 244.1455, 693.2027, 321.4684),
                  c(127.6436, 321.4684, 692.0981, 395.4775),
                  c(128.7482, 396.5821, 692.0981, 446.2897),
                  c(127.6436, 451.8127, 692.0981, 514.7757),
                  c(123.2251, 514.7757, 694.3073, 587.6802))

# repeating the same painful process for pg 2

colcoords_pg2 <- list(c(86.77307, 21.01361, 656.75068, 65.19814),
                      c(86.77307, 62.98891, 657.85529, 114.90573),
                      c(85.66846, 116.01034, 653.43686, 183.39175),
                      c(85.66846, 178.97329, 656.75068, 227.57627),
                      c(86.77307, 226.47166, 654.54147, 272.86541),
                      c(86.77307, 270.65619, 657.85529, 341.35143),
                      c(87.87768, 339.14220, 657.85529, 419.77897),
                      c(87.87768, 419.77897, 657.85529, 493.78805),
                      c(87.87768, 492.68344, 654.54147, 568.90175))

# need to throw in a warning that will break the loop if the vector is longer than 52

for (p in 1:2) {
  
  # making sure I have the right column coordinates based on which page I am reading in
  
  if (p==1) colcoords <- colcoords_pg1
  else colcoords <- colcoords_pg2

  # reading in the data one column as a time
  
  for (i in 1:length(colnames_list[[p]])) {
    test <- extract_tables(filepath,
                           pages = p+1,
                           area = list(colcoords[[i]]))[[1]] %>% 
      as.data.frame()
    
    # cleaning the data and storing it in an empty vector
    
    clean_col <- rep(NA, nrow(test))
    for (r in 1:nrow(test)) {
      
      # it splits the data into multiple columns, so I am combining all row
      # values into a single column and removing any leading/trailing white
      # space
      
      this_row <- ''
      for (c in 1:ncol(test)) {
        this_row <- paste(this_row, test[r,c])
      }
      clean_col[r] <- str_squish(this_row)
    }
    
    # assigning values to the appropriate column name
    print(colnames_list[[p]][i])
    assign(colnames_list[[p]][i], clean_col)
  }
}

# now I want to bind all of these named vectors together

messy_df <- do.call(cbind, sapply(c(colnames_list[[1]], colnames_list[[2]]), get))
