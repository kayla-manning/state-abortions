# using this script to load all packages, data, and functions necessary for the
# models.Rmd script

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
  
  # joining nonspatial data with spatial polygon data
  {
    # getting spatial data for US state boundaries & subsetting out Alaska & Hawaii
    # (using tutorial at https://mhallwor.github.io/_pages/basics_SpatialPolygons)
    
    usa <- raster::getData('GADM', country='USA', level=1)
    usa <- usa[!usa$NAME_1 %in% c('Alaska', 'Hawaii'),]
    
    # merging with data
    
    usa <- merge(usa, nonspatial_df,
                 by.x = 'NAME_1', by.y = 'state',
                 duplicateGeoms = TRUE, all.x = FALSE)
    }
  
  # I call this within a few of my functions
  
  methods <- c('raw_lm', 'raw_car', 'raw_errorsar', 'raw_lagsar')
               
}

###############################
##### HELPER FUNCTIONS
###############################


# model fitting & associated diagnostics (moran test, qqplot/residual plot,
# model output, etc.)
{
  make_models <- function(weights_matrix, var = 'rate') {
    
    # fitting models for each of the response variables
    
    if (var == 'rate') {
      
      raw_f <- as.formula(sqrt(abortion_per_1k_births) ~ surrounding_score * within_score + pct_bachelors + 
                            prop_hisp + prop_nonwhite + hh_income + dem_2party + 
                            as.factor(year) + total_population)
      
    }
    
    if (var == 'ie') {

      raw_f <- as.formula(log(ie_ratio) ~ surrounding_score * within_score + pct_bachelors + 
                            prop_hisp + prop_nonwhite + hh_income + dem_2party + as.factor(year) + 
                            total_population)
      
    }
    
    if (var == 'late_early') {
      
      raw_f <- as.formula(sqrt(late_to_early) ~ surrounding_score * within_score + pct_bachelors + 
                            prop_hisp + prop_nonwhite + hh_income + dem_2party + as.factor(year) + 
                            total_population)
      
    }
    
    # fitting models
    
    {
      
      # using the raw scores
      
      raw_lm <- lm(raw_f, data = usa@data)
      raw_car <- spautolm(raw_f,
                          data = usa@data,
                          family = 'CAR',
                          listw = weights_matrix)
      raw_errorsar <- errorsarlm(raw_f,
                                 data = usa@data,
                                 listw = weights_matrix)
      raw_lagsar <- lagsarlm(raw_f,
                             data = usa@data,
                             listw = weights_matrix,
                             tol.solve=1.0e-30)
    }
    
    # creating a list to store everything
    
    models <- list(raw_lm, raw_car, raw_errorsar, raw_lagsar)
    
    # I want to return the methods & models
    
    return_values <- list(methods, models)
    print(paste('done with', var))
    return(return_values)
  }
}

# model comparison (AIC tables for different specifications)
{
  # producing metrics for each model
  
  get_metrics <- function(mod) {
    if (is.null(summary(mod)$lambda)) {
      metrics <- c(AIC(mod), BIC(mod), logLik(mod), NA, NA)
    }
    else {
      metrics <- c(AIC(mod), BIC(mod), logLik(mod), summary(mod)$lambda, summary(mod)$lambda.se)
    }
    return(metrics)
  }
  
  # getting metrics for a specific model list
  
  metrics <- c('AIC', 'BIC', 'logLik', 'lambda', 'lambda se')
  get_metrics_table <- function(model_list) {
    tibble(method = rep(methods, each = length(metrics)),
           metric = rep(metrics, times = length(methods)),
           value = unlist(lapply(model_list[[2]], get_metrics))) %>% 
      pivot_wider(names_from = metric, values_from = value)
  }
  
  # making a function to produce model comparisons table (across different weight
  # specifications)
  
  get_model_comparisons <- function(inv_dist_list, inv_dist2_list, contig_list) {
    get_metrics_table(inv_dist_list) %>% 
      mutate(weights = 'inv_dist') %>% 
      bind_rows(get_metrics_table(inv_dist2_list) %>% 
                  mutate(weights = 'inv_dist2')) %>% 
      bind_rows(get_metrics_table(contig_list) %>% 
                  mutate(weights = 'contig')) %>% 
      select(method, AIC, BIC, logLik, weights) %>% 
      pivot_longer(AIC:logLik, names_to = 'criteria')  %>% 
      mutate(criteria = paste0(criteria, '_', weights)) %>% 
      select(-weights) %>% 
      pivot_wider(names_from = 'criteria', values_from = 'value')
  }
}

# model interpretation (summary dfs & coefficient plots)
{
  # writing a function to get the summary df object for my SAR models
  
  get_summary_df <- function(sar_mod) {
    unclass(summary(sar_mod))$Coef %>% 
      as.data.frame() %>% 
      rownames_to_column('term') %>% 
      clean_names()
  }
}

