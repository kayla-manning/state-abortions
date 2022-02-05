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
### data prep & functions
#############################################

{
  # getting right column names for the tables (click links in column titles at
  # https://web.archive.org/web/20170203044806/https://www.guttmacher.org/state-policy/explore/overview-abortion-laws
  # for descriptions of each)
  
  {
    colnames_pg1 <- c('state', 'licensed_physician', 'hospital_if', 
                      'second_physician_if', 'prohibited_except_death_if',
                      'partial_birth_ban', 'fund_all_most', 'fund_life_rape_incest',
                      'priv_insurance_limit')
    colnames_pg2 <- c('state', 'indiv_prov_refuse', 'instit_prov_refuse', 'mandate_breast_cancer',
                      'mandate_fetal_pain', 'mandate_neg_psych', 'hr_wait_post_counseling',
                      'parental_involv_minors')
    colnames_list <- list(colnames_pg1, colnames_pg2)
  }
  
  # cleans Guttmacher's policy tables in the formatting used for 2006-2022
  
  clean_policy_tables <- function(tables) {
      
      # reading in the tables from the link
      
      for (t in 1:length(tables)) {
        if (length(colnames(tables[[t]]))>=8) {
          test <- tables[[t]] %>% 
            filter(X1 %in% state.abb)
          colnames(test) <- colnames_list[[t]]
          assign(paste0('df_', t), test)          
        }
      }
      
      # combining the tables from the separate pages into a single df
      
      this_yr_df <- inner_join(df_1, df_2, by = 'state') 
      return(this_yr_df)
  }
}

#############################################
### scraping 2017-2022
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

# going to read in one column at a time... here's a list of column coordinates
# for page 1 (will have to adjust these values for different years, but it
# serves as a solid baseline)

{
  top_pg1 <- 124.3297
  bottom_pg1 <- 679.94745
  top_pg2 <- 87.87768
  bottom_pg2 <- 643.49539
  coords_df <- tibble(colname = c(colnames_pg1, colnames_pg2),
                      page = c(rep(1, length(colnames_pg1)), rep(2, length(colnames_pg2))),
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
}

for (y in 2006:2016) {
  
  filepath <- paste0('raw-data/policy_archives/laws_', y, '.pdf')
  
  # adjusting coordinates because the alignment of the pdf files varies slightly
  # by year (can see what coordinates I need by running locate_areas())
  
  {
    if (y==2006) {
      these_coords <- coords_df %>% 
        mutate(top = case_when(colname %in% c('instit_prov_refuse',
                                              'mandate_fetal_pain') ~ top-10,
                               TRUE ~ top),
               bottom = case_when(colname == 'instit_prov_refuse' ~ bottom+10,
                                  colname == 'mandate_fetal_pain' ~ bottom+5,
                                  TRUE ~ bottom))
    }
    if (y==2007) {
      these_coords <- coords_df %>% 
        mutate(top = case_when(colname %in% c('instit_prov_refuse',
                                              'mandate_fetal_pain') ~ top-10,
                               colname == 'mandate_breast_cancer' ~ top-25,
                               colname %in% c('indiv_prov_refuse',
                                              'mandate_neg_psych') ~ top+15,
                               colname %in% c('state',
                                              'hr_wait_post_counseling') ~ top+10,
                               TRUE ~ top),
               bottom = case_when(colname %in% c('instit_prov_refuse',
                                                 'parental_involv_minors') ~ bottom+20,
                                  colname %in% c('indiv_prov_refuse',
                                                 'mandate_neg_psych', 
                                                 'hr_wait_post_counseling') ~ bottom+20,
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
                               TRUE ~ top),
               bottom = case_when(colname %in% c('instit_prov_refuse',
                                                 'parental_involv_minors',
                                                 'mandate_fetal_pain') ~ bottom+10,
                                  colname == 'mandate_breast_cancer' ~ bottom+5,
                                  TRUE ~ bottom),
               left = case_when(colname == 'state' ~ left-15,
                                TRUE ~ left))  
    }
    if (y %in% c(2011, 2012)) {
      these_coords <- coords_df %>% 
        mutate(top = case_when(colname %in% c('state', 
                                              'mandate_neg_psych',
                                              'parental_involv_minors',
                                              'mandate_breast_cancer',
                                              'hr_wait_post_counseling') ~ top+5,
                               colname %in% c('instit_prov_refuse') ~ top-10,
                               colname == 'mandate_fetal_pain' ~ top-20,
                               TRUE ~ top),
               bottom = case_when(colname %in% c(colnames_pg1[-1],
                                                 'parental_involv_minors') ~ bottom-20,
                                  TRUE ~ bottom-10),
               left = case_when(colname == 'mandate_breast_cancer' ~ left+4,
                                colname == 'parental_involv_minors' ~ left-10,
                                colname == 'indiv_prov_refuse' ~ left+10,
                                TRUE ~ left),
               right = case_when(colname == 'mandate_breast_cancer' ~ right-7,
                                 colname == 'indiv_prov_refuse' ~ right-5,
                                 colname == 'parental_involv_minors' ~ right-2,
                                 TRUE ~ right))  
    }
    if (y %in% c(2013, 2014, 2015)) {
      these_coords <- coords_df %>% 
        mutate(top = case_when(colname %in% c('mandate_neg_psych',
                                              'mandate_breast_cancer',
                                              'mandate_fetal_pain') ~ top+5,
                               colname %in% c('state',
                                              'mandate_neg_psych',
                                              'hr_wait_post_counseling',
                                              'parental_involv_minors') ~ top+10,
                               colname %in% c('instit_prov_refuse') ~ top-10,
                               colname == 'priv_insurance_limit' ~ top-25,
                               colname %in% c(colnames_pg1[-1]) ~ top-15,
                               TRUE ~ top),
               bottom = case_when(colname == 'priv_insurance_limit' ~ bottom-30,
                                  colname %in% c(colnames_pg1[-1]) ~ bottom-40,
                                  colname %in% c('indiv_prov_refuse',
                                                 'mandate_breast_cancer') ~ bottom,
                                  colname %in% c('instit_prov_refuse',
                                                 'mandate_neg_psych') ~ bottom-5,
                                  TRUE ~ bottom-10),
               left = case_when(colname == 'mandate_breast_cancer' ~ left+4,
                                colname == 'parental_involv_minors' ~ left-10,
                                colname == 'hr_wait_post_counseling' ~ left-5,
                                colname == 'indiv_prov_refuse' ~ left+10,
                                colname %in% c('priv_insurance_limit',
                                               'mandate_fetal_pain') ~ left+25,
                                colname %in% c('mandate_neg_psych') ~ left+50,
                                TRUE ~ left),
               right = case_when(colname == 'mandate_breast_cancer' ~ right-7,
                                 colname == 'indiv_prov_refuse' ~ right-5,
                                 colname == 'parental_involv_minors' ~ right+10,
                                 colname %in% c('mandate_fetal_pain',
                                                'mandate_neg_psych') ~ right+30,
                                 TRUE ~ right))  
    }
    if (y==2016) {
      these_coords <- coords_df %>% 
        mutate(top = case_when(colname %in% c('mandate_breast_cancer',
                                              'state') ~ top+5,
                               colname %in% c('hr_wait_post_counseling',
                                              'parental_involv_minors') ~ top+10,
                               colname %in% c('instit_prov_refuse',
                                              'mandate_fetal_pain') ~ top-10,
                               colname %in% c('priv_insurance_limit') ~ top-25,
                               colname %in% c(colnames_pg1[-1],
                                              'mandate_neg_psych') ~ top-15,
                               TRUE ~ top),
               bottom = case_when(colname == 'priv_insurance_limit' ~ bottom-30,
                                  colname %in% c(colnames_pg1[-1]) ~ bottom-40,
                                  colname %in% c('mandate_breast_cancer') ~ bottom,
                                  colname %in% c('indiv_prov_refuse',
                                                 'mandate_neg_psych') ~ bottom-5,
                                  TRUE ~ bottom-10),
               left = case_when(colname == 'mandate_breast_cancer' ~ left+4,
                                colname == 'parental_involv_minors' ~ left-10,
                                colname == 'hr_wait_post_counseling' ~ left-5,
                                colname == 'indiv_prov_refuse' ~ left+10,
                                colname %in% c('priv_insurance_limit',
                                               'mandate_fetal_pain') ~ left+25,
                                colname %in% c('mandate_neg_psych') ~ left+50,
                                TRUE ~ left),
               right = case_when(colname == 'mandate_breast_cancer' ~ right-7,
                                 colname == 'indiv_prov_refuse' ~ right-5,
                                 colname == 'parental_involv_minors' ~ right+10,
                                 colname %in% c('mandate_fetal_pain',
                                                'mandate_neg_psych') ~ right+30,
                                 TRUE ~ right))  
    }
  }
  
  for (p in 1:2) {
    
    # making sure I have the right column coordinates based on which page I am reading in
    
    colcoords <- these_coords %>% 
      filter(page==p) %>% 
      select(top:right) %>% 
      split(seq(nrow(.))) %>% 
      lapply(., function(x) unlist(slice(x, 1), use.names = FALSE))
    
    # reading in the data one column at a time
    
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
      # print(colnames_list[[p]][i])
      # print(length(clean_col))
      assign(colnames_list[[p]][i], clean_col)
    }
  }
  
  # hard-coding in the states because 2008-2010 weren't reading in correctly

  state <- c('AL', 'AK', 'AZ', 'AR', 'CA', 'CO', 'CT', 'DE', 'DC', 'FL', 'GA', 'HI', 'ID', 
             'IL', 'IN', 'IA', 'KS', 'KY', 'LA', 'ME', 'MD', 'MA', 'MI', 'MN', 'MS', 'MO',
             'MT', 'NE', 'NV', 'NH', 'NJ', 'NM', 'NY', 'NC', 'ND', 'OH', 'OK', 'OR', 'PA', 
             'RI', 'SC', 'SD', 'TN', 'TX', 'UT', 'VT', 'VA', 'WA', 'WV', 'WI', 'WY')
  
  # now I want to bind all of these named vectors together
  
  results_matrix <- sapply(c(colnames_list[[1]], colnames_list[[2]]), get) 
  duplicated_cols <- duplicated(t(results_matrix))
  messy_df <- results_matrix[,!duplicated_cols] %>% 
    as.data.frame() %>% 
    mutate(as_of = sources$as_of_date[sapply(sources$links, function(x) str_detect(x, as.character(paste0('/', y))))],
           accessed = sources$date_accessed[sapply(sources$links, function(x) str_detect(x, as.character(paste0('/', y))))])
  
  # adding to the main policy df
  
  policy_df <- bind_rows(policy_df, messy_df)
  print(paste('finished', y))
}

# adding a year column to the policy df to make it easier to read

policy_df <- policy_df %>% 
  mutate(year = year(as_of)) %>% 
  select(state, year, everything()) %>% 
  arrange(year)
