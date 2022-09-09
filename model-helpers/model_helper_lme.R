
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
  nonspatial_df <- read_csv('data-creation/raw-data/combined_data.csv') %>% 
    mutate(across(c(within_score, surrounding_score, hh_income, prop_bachelors, 
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
    drop_na(within_score, surrounding_score, prop_bachelors, total_population, 
            prop_hisp, prop_nonwhite, hh_income, dem_2party, abortion_per_1k_births, 
            rate_nonres, rate_res, rate_late, rate_early, prop_late)
  
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
    
    # making sure I don't end up with duplicates anywhere
    
    usa@data <- distinct(usa@data)
    
  }
  
}

###############################
##### FITTING MODELS
###############################

{
  # creating list of variables to iterate through
  
  outcome_vars <- c('rate', 'rate_nonres', 'rate_res',
                    'rate_late', 'rate_early', 'prop_late')
  fitted_models <- list()
  
  for (var in outcome_vars) {
    
    # fitting models for each of the rate response variables
    
    if (var == 'rate') {
      
      raw_f <- as.formula(sqrt(abortion_per_1k_births) ~ surrounding_score * within_score + prop_bachelors + 
                            prop_hisp + prop_nonwhite + hh_income + dem_2party + total_population)
      
    }
    
    if (var == 'rate_nonres') {
      
      raw_f <- as.formula(log(rate_nonres) ~ surrounding_score * within_score + prop_bachelors + 
                            prop_hisp + prop_nonwhite + hh_income + dem_2party + total_population)
      
    }
    
    if (var == 'rate_res') {
      
      raw_f <- as.formula(sqrt(rate_res) ~ surrounding_score * within_score + prop_bachelors + 
                            prop_hisp + prop_nonwhite + hh_income + dem_2party + total_population)
      
    }
    
    # model formulas for abortion timing

    if (var == 'rate_late') {

      raw_f <- as.formula(sqrt(rate_late) ~ surrounding_score * within_score + prop_bachelors +
                            prop_hisp + prop_nonwhite + hh_income + dem_2party + total_population)

    }
    
    if (var == 'rate_early') {
      
      raw_f <- as.formula(sqrt(rate_early) ~ surrounding_score * within_score + prop_bachelors + 
                            prop_hisp + prop_nonwhite + hh_income + dem_2party + total_population)
      
    }

    if (var == 'prop_late') {

      raw_f <- as.formula(sqrt(prop_late) ~ surrounding_score * within_score + prop_bachelors +
                            prop_hisp + prop_nonwhite + hh_income + dem_2party + total_population)

    }
    
    # creating list of spatial structures so I can fit models in a loop
    
    struct_types <- c('none', 'gaus')
    cor_structs <- list(NA, corGaus(1, form = ~ (longitude + latitude) | year))
    
    # fitting models for the two treatments
    
    return_list <- list()
    treatments <- c('raw')
    formulas <- list(raw_f)
    
    # fitting all models for the current outcome variable and treatment
    
    for (i in 1:length(treatments)) {
      
      # printing which treatment I'm on so I can keep track of things
      
      print(paste(var, treatments[i], sep = ': '))
      
      # fitting nonspatial mixed-effects model & will update with the
      # appropriate correlation structures
      
      nonspatial <- lme(formulas[[i]], 
                        data = usa@data,
                        random = ~ 1 | year,
                        control = lmeControl(maxIter = 1e100,
                                             singular.ok = TRUE, 
                                             opt = 'optim'))
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

# adding our chosen model objects to the environment under easy-to-reference
# names

rate_mod <- fitted_models$rate$raw$best_mod
rate_nonres_mod <- fitted_models$rate_nonres$raw$best_mod
rate_res_mod <- fitted_models$rate_res$raw$best_mod
rate_late_mod <- fitted_models$rate_late$raw$best_mod
rate_early_mod <- fitted_models$rate_early$raw$best_mod
prop_late_mod <- fitted_models$prop_late$raw$best_mod
  
