
###############################
##### SETUP (packages & data)
###############################

{
  # packages
  
  {
    library(MASS)
    library(tidyverse)
    library(raster)
    library(spdep)
    library(spatialreg)
    library(nlme)
    select <- dplyr::select
    library(ggpubr)
    library(grid)
    library(knitr)
    library(kableExtra)
    library(janitor)
    library(broom)
  }
  
  # data (getting income in 1000s & standardizing all quantitative dependent
  # variables so I can interpret as increase/decrease in 1 std. dev)
  
  standardize <- function(x) {(x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)}
  nonspatial_df <- read_csv('raw-data/combined_data.csv') %>% 
    mutate(hh_income = hh_income / 1000) %>% 
    mutate(across(c(within_score, surrounding_score, hh_income, pct_bachelors, 
                    prop_hisp, prop_nonwhite, hh_income, dem_2party, total_population), 
                  standardize)) %>% 
    inner_join(
      read.delim('https://www2.census.gov/geo/docs/reference/cenpop2020/CenPop2020_Mean_ST.txt') %>% 
        separate(STATEFP.STNAME.POPULATION.LATITUDE.LONGITUDE, sep = ',',
                 into = c('statefip', 'state', 'population', 'latitude', 'longitude')) %>% 
        mutate(across(c(latitude, longitude), as.numeric)) %>% 
        select(state, latitude, longitude), by = 'state')
  
  # getting df that drops observations with NA in predictors/outcomes
  
  nonspatial_df <- nonspatial_df %>%
    drop_na(abortions, births, within_score, surrounding_score,
            pct_bachelors, total_population, prop_hisp, prop_nonwhite,
            hh_income, dem_2party)
  
  # joining nonspatial data with spatial data
  {
    # getting spatial data for US state boundaries & subsetting out Alaska & Hawaii
    # (using tutorial at https://mhallwor.github.io/_pages/basics_SpatialPolygons)
    
    usa <- raster::getData('GADM', country='USA', level=1)
    usa <- usa[!usa$NAME_1 %in% c('Alaska', 'Hawaii'),]
    
    # merging with data
    
    usa <- merge(usa, nonspatial_df,
                 by.x = 'NAME_1', by.y = 'state',
                 duplicateGeoms = TRUE, all.x = FALSE)
    
    # ordering within-between categories so that the reference grouping makes sense
    
    usa@data$within_between <- fct_relevel(usa@data$within_between,
                                           'low-low', 'med-low', 'high-low',
                                           'low-med', 'med-med', 'high-med',
                                           'low-high', 'med-high', 'high-high')
    usa@data$within_between <- fct_relevel(usa@data$within_between,
                                           'high-high')
    
    # introducing negligible random noise to latitude/longitude so that I don't
    # get an error for repeat observations in the same location
    
    set.seed(1973)
    usa@data$latitude <- usa@data$latitude + rnorm(length(usa@data$latitude), 0, 0.0001)
    usa@data$longitude <- usa@data$longitude + rnorm(length(usa@data$longitude), 0, 0.0001)
    
  }
  
  # I call this within a few of my functions
  
  methods <- c('category_lm', 'category_car', 'category_errorsar', 'category_lagsar',
               'raw_lm', 'raw_car', 'raw_errorsar', 'raw_lagsar')
  
}

###############################
##### FITTING MODELS
###############################

{
  # creating list of variables to iterate through
  
  outcome_vars <- c('rate', 'ie', 'late_early')
  fitted_models <- list()
  
  for (var in outcome_vars) {
    
    # fitting models for each of the response variables
    
    if (var == 'rate') {
      
      category_f <- as.formula(sqrt(abortion_per_1k_births) ~ within_between + pct_bachelors + prop_hisp + 
                                 prop_nonwhite + hh_income + dem_2party + total_population)
      raw_f <- as.formula(sqrt(abortion_per_1k_births) ~ surrounding_score * within_score + pct_bachelors + 
                            prop_hisp + prop_nonwhite + hh_income + dem_2party + total_population)
      
    }
    
    if (var == 'ie') {
      
      category_f <- as.formula(log(ie_ratio) ~ within_between + pct_bachelors + prop_hisp + 
                                 prop_nonwhite + hh_income + dem_2party + total_population)
      raw_f <- as.formula(log(ie_ratio) ~ surrounding_score * within_score + pct_bachelors + 
                            prop_hisp + prop_nonwhite + hh_income + dem_2party + total_population)
      
    }
    
    if (var == 'late_early') {
      
      category_f <- as.formula(sqrt(late_to_early) ~ within_between + pct_bachelors + prop_hisp + 
                                 prop_nonwhite + hh_income + dem_2party + total_population)
      raw_f <- as.formula(sqrt(late_to_early) ~ surrounding_score * within_score + pct_bachelors + 
                            prop_hisp + prop_nonwhite + hh_income + dem_2party + total_population)
      
    }
    
    # creating list of spatial structures so I can fit models in a loop
    
    struct_types <- c('none', 'gaus')
    cor_structs <- list(NA, corGaus(1, form = ~ (longitude + latitude) | year, 
                                    nugget = TRUE))
    
    # fitting models for the two treatments
    
    return_list <- list()
    # treatments <- c('raw', 'category')
    treatments <- c('raw')
    formulas <- list(raw_f)
    # formulas <- list(raw_f, category_f)
    
    # fitting all models for the current outcome variable and treatment
    
    for (i in 1:length(treatments)) {
      
      # printing which treatment I'm on so I can keep track of things
      
      print(paste(var, treatments[i], sep = ': '))
      
      # fitting nonspatial mixed-effects model & will update with the
      # appropriate correlation structures
      
      nonspatial <- lme(formulas[[i]], 
                        data = usa@data,
                        random = list(year = ~1),
                        control=lmeControl(opt = 'optim'))
      mods <- list(nonspatial)
      
      # adding spatial structure
      
      for (k in 1:(length(struct_types)-1)) {
        
        mods[[k+1]] <- update(nonspatial, 
                              correlation = cor_structs[[k+1]])
        
      }
      
      # naming list
      
      names(mods) <- struct_types
      
      # getting list of AICs
      
      AICs <- lapply(mods, AIC)
      
      # getting index of the best AIC
      
      best_idx <- which.min(AICs)
      best_mod <- mods[[best_idx]]
      best_struct <- struct_types[best_idx]
      
      # creating list of objects
      
      this_list <- list('AICs' = AICs,
                        'best_mod' = best_mod,
                        'best_struct' = best_struct,
                        'all_mods' = mods)
      
      # adding that list of lists to the final return list (w/ attributes
      # labeled for 'raw' and 'category' to allow for easy access)
      
      return_list[[i]] <- this_list
      
    }
    
    # returning everything
    
    names(return_list) <- treatments
    fitted_models[[which(outcome_vars == var)]] <- return_list
    
  }
  
  # naming the final list with the appropriate outcome variable
  
  names(fitted_models) <- outcome_vars
}


  
