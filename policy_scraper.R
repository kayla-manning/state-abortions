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

# for (y in 2006:2016) {
#   
#   # reading in table from pdf file that I saved from the internet archive
#   
  filepath <- paste0('raw-data/policy_archives/laws_', y, '.pdf')
#   
#   # creating a vector with the column locations to that tabulizer will correctly
#   # parse the columns (the extract_tables() function combines columns if I don't
#   # take this step)
#   # https://rpubs.com/behzod/tabulizer
#   
#   col_coords <- list(c(39.27494, 24.88029, 654.54147, 64.64656))
#   tables <- extract_tables(filepath, output = 'data.frame',
#                            columns = col_coords,
#                            area = list(c(124.32973, 14.38593, 
#                                          693.20274, 585.47094)))
#   this_yr_df <- clean_policy_tables(tables) %>% 
#     mutate(as_of = sources$as_of_date[sapply(sources$links, function(x) str_detect(x, as.character(y)))])
#   policy_df <- bind_rows(policy_df, this_yr_df)
#   
# }

# adding a year column & other cleaning

policy_df <- policy_df %>% 
  mutate(year = year(as_of)) %>% 
  select(state, year, everything())

# going to read in one column at a time... here's a list of column coordinates
# for page 1

top_pg1 <- 124.3297
bottom_pg1 <- 679.94745

coords_df <- tibble(colname = c(colnames_pg1, colnames_pg2),
                    top = c(rep(top_pg1, length(colnames_pg1)),
                            rep(top_pg2, length(colnames_pg2))),
                    left = c(22.11822, 64.09352, 121.5334, 183.3917, 248.5066, 321.4684,
                             396.5821, 451.8127, 514.7757, 21.01361, 62.98891, 124.60346,
                             178.97329, 234.73962, 270.65619, 419.77897, 492.68344),
                    bottom = c(rep(bottom_pg1, length(colnames_pg1)),
                               rep(bottom_pg2, length(colnames_pg2))),
                    right = c(65.19814, 118.21957, 185.6010, 247.4593, 322.2007, 395.4775,
                              446.2897, 514.7757, 587.6802, 65.19814, 114.90573, 174.00277,
                              227.57627, 262.27367, 341.35143, 493.78805, 568.90175))

# top adjustments for 2006 and 2007 are the same but the bottom ones are different

if (y==2006) {
  these_coords <- coords_df %>% 
    mutate(top = case_when(colname %in% c('instit_prov_refuse',
                                          'mandate_fetal_pain') ~ top-10,
                           TRUE ~ top),
           bottom = case_when(colname == 'instit_prov_refuse' ~ bottom+10,
                              colname == 'mandate_fetal_pain' ~ bottom+5))
}
if (y==2007) {
  these_coords <- coords_df %>% 
    mutate(top = case_when(colname %in% c('instit_prov_refuse',
                                          'mandate_fetal_pain') ~ top-10,
                           colname == 'mandate_breast_cancer' ~ top-25,
                           colname == 'state' ~ top+10,
                           TRUE ~ top),
           bottom = case_when(colname %in% c('instit_prov_refuse',
                                             'parental_involv_minors') ~ bottom+20,
                              colname %in% c('mandate_fetal_pain',
                                             'mandate_breast_cancer') ~ bottom+15,
                              colname == 'state' ~ bottom+20,
                              TRUE ~ bottom))
}
if (y %in% c(2008, 2009, 2010)) {
  these_coords <- coords_df %>% 
    mutate(top = case_when(colname %in% c('instit_prov_refuse',
                                          'mandate_fetal_pain') ~ top-10,
                           colname == 'mandate_breast_cancer' ~ top-25,
                           colname == 'state' ~ top+10,
                           TRUE ~ top),
           bottom = case_when(colname %in% c('instit_prov_refuse',
                                             'parental_involv_minors',
                                             'mandate_fetal_pain') ~ bottom+10,
                              colname == 'mandate_breast_cancer' ~ bottom+5,
                              colname == 'state' ~ bottom+20,
                              TRUE ~ bottom))  
}
if (y %in% c(2011, 2012)) {
  these_coords <- coords_df %>% 
    mutate(top = case_when(colname %in% c('state', 
                                          'mandate_fetal_pain',
                                          'mandate_neg_psych',
                                          'parental_involv_minors') ~ top+5,
                           colname %in% c('instit_prov_refuse',
                                          'mandate_fetal_pain') ~ top-10,
                           colname == 'mandate_breast_cancer' ~ top-25,
                           TRUE ~ top),
           bottom = case_when(colname %in% colnames_pg1[-1] ~ bottom-20,
                              TRUE ~ bottom-10),
           left = case_when(colname == 'mandate_breast_cancer' ~ left+4,
                            colname == 'parental_involv_minors' ~ left-7,
                            colname == 'invidi_prov_refuse' ~ left+10,
                            TRUE ~ left),
           right = case_when(colname == 'mandate_breast_cancer' ~ right-7,
                             colname == 'invidi_prov_refuse' ~ right-5,
                             TRUE ~ right))  
}

# pg 1 coords

top_pg1 <- 124.3297
bottom_pg1 <- 679.94745

colcoords_pg1 <- list(c(top_pg1+10, 22.11822, bottom_pg1+20, 65.19814),
                      c(top_pg1, 64.09352, bottom_pg1-20, 118.21957),
                      c(top_pg1, 125.9334, bottom_pg1-20, 181.6010),
                      c(top_pg1, 183.3917+4, bottom_pg1-20, 247.4593-7),
                      c(top_pg1, 248.5066, bottom_pg1-20, 322.2007),
                      c(top_pg1, 321.4684, bottom_pg1-20, 395.4775),
                      c(top_pg1, 396.5821, bottom_pg1-20, 446.2897),
                      c(top_pg1, 451.8127, bottom_pg1-20, 514.7757),
                      c(top_pg1, 514.7757, bottom_pg1-20, 587.6802))

# repeating the same painful process for pg 2
# (note that 2006 has an additional column about mandating
# information about abortion alternatives and support services... not going to
# include that since it's not in future data)

top_pg2 <- 87.87768
bottom_pg2 <- 643.49539
colcoords_pg2 <- list(c(top_pg2+5, 21.01361, bottom_pg2-10, 65.19814),
                      c(top_pg2, 62.98891+10, bottom_pg2-10, 114.90573-5),
                      c(top_pg2-10, 124.60346, bottom_pg2-10, 174.00277),
                      c(top_pg2-25, 178.97329, bottom_pg2-10, 227.57627),
                      c(top_pg2-10, 234.73962, bottom_pg2-10, 262.27367),
                      c(top_pg2+5, 270.65619, bottom_pg2-10, 341.35143),
                      c(top_pg2+5, 419.77897, bottom_pg2-10, 493.78805),
                      # c(top_pg2+6, 490.47421, bottom_pg2-21, 568.90175+1),
                      c(top_pg2+5, 483.25240, bottom_pg2-10, 567.22083))

# need to throw in a warning that will break the loop if the vector is longer than 52

y <- 2013
filepath <- paste0('raw-data/policy_archives/laws_', y, '.pdf')

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
    print(length(clean_col))
    assign(colnames_list[[p]][i], clean_col)
  }
}

# now I want to bind all of these named vectors together

results_matrix <- sapply(c(colnames_list[[1]], colnames_list[[2]]), get) 
duplicated_cols <- duplicated(t(results_matrix))
messy_df <- results_matrix[,!duplicated_cols] %>% 
  as.data.frame() %>% 
  mutate(as_of = sources$as_of_date[sapply(sources$links, function(x) str_detect(x, as.character(y)))])
