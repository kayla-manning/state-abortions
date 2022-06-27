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
    library(knitr)
    library(kableExtra)
    library(janitor)
  }
  
  # data
  
  combined <- read_csv('raw-data/combined_data.csv') %>% 
    inner_join(
      read.delim('https://www2.census.gov/geo/docs/reference/cenpop2020/CenPop2020_Mean_ST.txt') %>% 
        separate(STATEFP.STNAME.POPULATION.LATITUDE.LONGITUDE, sep = ',',
                 into = c('statefip', 'state', 'population', 'latitude', 'longitude')) %>% 
        mutate(across(c(latitude, longitude), as.numeric)) %>% 
        select(state, latitude, longitude), by = 'state')
  
  # getting df that drops observations with NA in predictors/outcomes
  
  nonspatial_df <- combined %>% 
    drop_na(abortions, births, intrastate_score, interstate_score,
            pct_bachelors, total_population, prop_hisp, prop_nonwhite,
            hh_income, dem_2party)
}

###############################
##### HELPER FUNCTIONS
###############################


# model fitting & associated diagnostics (moran test, qqplot/residual plot,
# model output, etc.)
{
  methods <- c('lm', 'car', 'sar')
  
  # fitting models for abortion rates
  {
    # writing a function to fit all of the models
    
    make_rate_models <- function(weights_matrix) {
      
      # creating each model object
      
      rate_lm <- lm(abortion_per_1k_births ~ within_between + pct_bachelors + prop_hisp + 
                      prop_nonwhite + hh_income + dem_2party + as.factor(year),
                    data = usa@data)
      rate_car <- spautolm(abortion_per_1k_births ~ within_between + pct_bachelors + prop_hisp + 
                             prop_nonwhite + hh_income + dem_2party + as.factor(year),
                           data = usa@data,
                           family = 'CAR',
                           listw = weights_matrix)
      rate_sar <- spautolm(abortion_per_1k_births ~ within_between + pct_bachelors + prop_hisp + 
                             prop_nonwhite + hh_income + dem_2party + as.factor(year),
                           data = usa@data,
                           family = 'SAR',
                           listw = weights_matrix)
      models <- list(rate_lm, rate_car, rate_sar)
      
      
      
      morans <- vector('list', length = length(models))
      plots <- vector('list', length = length(models))
      
      for (i in 1:length(models)) {
        
        ################
        #### MORAN
        ################
        
        {
          # conducting a Moran test on each of the models, assessing whether (p < 0.05) or
          # not (p >= 0.05) we have evidence of spatial autocorrelation not captured by
          # the model...
          # using built-in function for our lm objects, but it doesn't work on spautolm
          # objects
          
          if (class(models[[i]]) == 'lm') {
            morans[[i]] <- lm.morantest(models[[i]], listw = weights_matrix, alternative = 'two.sided')
          }
          
          # conduct a Moran test of the residuals, where p < 0.05 provides sufficient
          # evidence of additional autocorrelation not captured by the model
          
          else {
            morans[[i]] <- moran.test(residuals(models[[i]]), listw = weights_matrix, alternative = 'two.sided')
          }
        }
        
        ################
        #### PLOTS
        ################
        
        {
          # generating diagnostic plots for each model
          
          resid_vs_fit <- tibble(fit = fitted(models[[i]]), 
                                 resid = residuals(models[[i]])) %>% 
            ggplot(aes(fit, resid)) +
            geom_point() +
            geom_hline(yintercept = 0,
                       color = 'red',
                       linetype = 'dashed') +
            theme_minimal() +
            labs(title = 'Residuals vs. fitted values',
                 x = 'Fitted values',
                 y = 'Residuals')
          plot_data <- tibble(fit = fitted(models[[i]]), 
                              resid = residuals(models[[i]])) 
          qqplot <- plot_data %>% 
            ggplot(aes(sample = resid)) +
            stat_qq() +
            stat_qq_line(col = 'red') +
            theme_minimal() +
            labs(title = 'QQ-plot for normality of residuals',
                 x = 'Theoretical quantiles',
                 y = 'Sample quantiles')
          
          plots[[i]] <- annotate_figure(ggarrange(resid_vs_fit, qqplot),
                                        top = text_grob(paste('Diagnosic plots for', methods[[i]]),size = 14))
        }
      }
      
      # I want to return the methods, models, moran tests, and diagnostic plots
      
      return_values <- list(methods, models, morans, plots)
      return(return_values)
      
    }
    
  }
  
  # fitting models for import-export ratio
  {
    # writing a function to fit all of the models
    
    make_ie_models <- function(weights_matrix) {
      
      # creating each model object
      
      rate_lm <- lm(log(ie_ratio) ~ within_between + pct_bachelors + prop_hisp + 
                      prop_nonwhite + hh_income + dem_2party + as.factor(year) +
                      abortions,
                    data = usa@data)
      rate_car <- spautolm(log(ie_ratio) ~ within_between + pct_bachelors + prop_hisp + 
                             prop_nonwhite + hh_income + dem_2party + as.factor(year) +
                             abortions,
                           data = usa@data,
                           family = 'CAR',
                           listw = weights_matrix)
      rate_sar <- spautolm(log(ie_ratio) ~ within_between + pct_bachelors + prop_hisp + 
                             prop_nonwhite + hh_income + dem_2party + as.factor(year) +
                             abortions,
                           data = usa@data,
                           family = 'SAR',
                           listw = weights_matrix)
      models <- list(rate_lm, rate_car, rate_sar)
      
      
      
      morans <- vector('list', length = length(models))
      plots <- vector('list', length = length(models))
      
      for (i in 1:length(models)) {
        
        ################
        #### MORAN
        ################
        
        {
          # conducting a Moran test on each of the models, assessing whether (p < 0.05) or
          # not (p >= 0.05) we have evidence of spatial autocorrelation not captured by
          # the model...
          # using built-in function for our lm objects, but it doesn't work on spautolm
          # objects
          
          if (class(models[[i]]) == 'lm') {
            morans[[i]] <- lm.morantest(models[[i]], listw = weights_matrix, alternative = 'two.sided')
          }
          
          # conduct a Moran test of the residuals, where p < 0.05 provides sufficient
          # evidence of additional autocorrelation not captured by the model
          
          else {
            morans[[i]] <- moran.test(residuals(models[[i]]), listw = weights_matrix, alternative = 'two.sided')
          }
        }
        
        ################
        #### PLOTS
        ################
        
        {
          # generating diagnostic plots for each model
          
          resid_vs_fit <- tibble(fit = fitted(models[[i]]), 
                                 resid = residuals(models[[i]])) %>% 
            ggplot(aes(fit, resid)) +
            geom_point() +
            geom_hline(yintercept = 0,
                       color = 'red',
                       linetype = 'dashed') +
            theme_minimal() +
            labs(title = 'Residuals vs. fitted values',
                 x = 'Fitted values',
                 y = 'Residuals')
          plot_data <- tibble(fit = fitted(models[[i]]), 
                              resid = residuals(models[[i]])) 
          qqplot <- plot_data %>% 
            ggplot(aes(sample = resid)) +
            stat_qq() +
            stat_qq_line(col = 'red') +
            theme_minimal() +
            labs(title = 'QQ-plot for normality of residuals',
                 x = 'Theoretical quantiles',
                 y = 'Sample quantiles')
          
          plots[[i]] <- annotate_figure(ggarrange(resid_vs_fit, qqplot),
                                        top = text_grob(paste('Diagnosic plots for', methods[[i]]),size = 14))
        }
      }
      
      # I want to return the methods, models, moran tests, and diagnostic plots
      
      return_values <- list(methods, models, morans, plots)
      return(return_values)
      
    }
  }
  
  # fitting models for nonresident-to-resident ratio
  {
    # writing a function to fit all of the models
    
    make_nonres_res_models <- function(weights_matrix) {
      
      # creating each model object
      
      rate_lm <- lm(log(nonres_res_ratio-1) ~ within_between + pct_bachelors + 
                      prop_hisp + prop_nonwhite + hh_income + dem_2party +
                      as.factor(year),
                    data = usa@data)
      rate_car <- spautolm(log(nonres_res_ratio-1) ~ within_between + pct_bachelors + 
                             prop_hisp + prop_nonwhite + hh_income + dem_2party +
                             as.factor(year),
                           data = usa@data,
                           family = 'CAR',
                           listw = weights_matrix)
      rate_sar <- spautolm(log(nonres_res_ratio-1) ~ within_between + pct_bachelors + 
                             prop_hisp + prop_nonwhite + hh_income + dem_2party +
                             as.factor(year),
                           data = usa@data,
                           family = 'SAR',
                           listw = weights_matrix)
      models <- list(rate_lm, rate_car, rate_sar)
      
      
      
      morans <- vector('list', length = length(models))
      plots <- vector('list', length = length(models))
      
      for (i in 1:length(models)) {
        
        ################
        #### MORAN
        ################
        
        {
          # conducting a Moran test on each of the models, assessing whether (p < 0.05) or
          # not (p >= 0.05) we have evidence of spatial autocorrelation not captured by
          # the model...
          # using built-in function for our lm objects, but it doesn't work on spautolm
          # objects
          
          if (class(models[[i]]) == 'lm') {
            morans[[i]] <- lm.morantest(models[[i]], listw = weights_matrix, alternative = 'two.sided')
          }
          
          # conduct a Moran test of the residuals, where p < 0.05 provides sufficient
          # evidence of additional autocorrelation not captured by the model
          
          else {
            morans[[i]] <- moran.test(residuals(models[[i]]), listw = weights_matrix, alternative = 'two.sided')
          }
        }
        
        ################
        #### PLOTS
        ################
        
        {
          # generating diagnostic plots for each model
          
          resid_vs_fit <- tibble(fit = fitted(models[[i]]), 
                                 resid = residuals(models[[i]])) %>% 
            ggplot(aes(fit, resid)) +
            geom_point() +
            geom_hline(yintercept = 0,
                       color = 'red',
                       linetype = 'dashed') +
            theme_minimal() +
            labs(title = 'Residuals vs. fitted values',
                 x = 'Fitted values',
                 y = 'Residuals')
          plot_data <- tibble(fit = fitted(models[[i]]), 
                              resid = residuals(models[[i]])) 
          qqplot <- plot_data %>% 
            ggplot(aes(sample = resid)) +
            stat_qq() +
            stat_qq_line(col = 'red') +
            theme_minimal() +
            labs(title = 'QQ-plot for normality of residuals',
                 x = 'Theoretical quantiles',
                 y = 'Sample quantiles')
          
          plots[[i]] <- annotate_figure(ggarrange(resid_vs_fit, qqplot),
                                        top = text_grob(paste('Diagnosic plots for', methods[[i]]),size = 14))
        }
      }
      
      # I want to return the methods, models, moran tests, and diagnostic plots
      
      return_values <- list(methods, models, morans, plots)
      return(return_values)
      
    }
  }
  
  # fitting models for post13-to-pre13 ratio
  {
    # writing a function to fit all of the models
    
    make_late_early_models <- function(weights_matrix) {
      
      # creating each model object
      
      rate_lm <- lm(sqrt(late_to_early) ~ within_between + pct_bachelors + 
                      prop_hisp + prop_nonwhite + hh_income + dem_2party +
                      abortions + as.factor(year),
                    data = usa@data)
      rate_car <- spautolm(sqrt(late_to_early) ~ within_between + pct_bachelors + 
                             prop_hisp + prop_nonwhite + hh_income + dem_2party +
                             abortions + as.factor(year),
                           data = usa@data,
                           family = 'CAR',
                           listw = weights_matrix)
      rate_sar <- spautolm(sqrt(late_to_early) ~ within_between + pct_bachelors + 
                             prop_hisp + prop_nonwhite + hh_income + dem_2party +
                             abortions + as.factor(year),
                           data = usa@data,
                           family = 'SAR',
                           listw = weights_matrix)
      models <- list(rate_lm, rate_car, rate_sar)
      
      morans <- vector('list', length = length(models))
      plots <- vector('list', length = length(models))
      
      for (i in 1:length(models)) {
        
        ################
        #### MORAN
        ################
        
        {
          # conducting a Moran test on each of the models, assessing whether (p < 0.05) or
          # not (p >= 0.05) we have evidence of spatial autocorrelation not captured by
          # the model...
          # using built-in function for our lm objects, but it doesn't work on spautolm
          # objects
          
          if (class(models[[i]]) == 'lm') {
            morans[[i]] <- lm.morantest(models[[i]], listw = weights_matrix, alternative = 'two.sided')
          }
          
          # conduct a Moran test of the residuals, where p < 0.05 provides sufficient
          # evidence of additional autocorrelation not captured by the model
          
          else {
            morans[[i]] <- moran.test(residuals(models[[i]]), listw = weights_matrix, alternative = 'two.sided')
          }
        }
        
        ################
        #### PLOTS
        ################
        
        {
          # generating diagnostic plots for each model
          
          resid_vs_fit <- tibble(fit = fitted(models[[i]]), 
                                 resid = residuals(models[[i]])) %>% 
            ggplot(aes(fit, resid)) +
            geom_point() +
            geom_hline(yintercept = 0,
                       color = 'red',
                       linetype = 'dashed') +
            theme_minimal() +
            labs(title = 'Residuals vs. fitted values',
                 x = 'Fitted values',
                 y = 'Residuals')
          plot_data <- tibble(fit = fitted(models[[i]]), 
                              resid = residuals(models[[i]])) 
          qqplot <- plot_data %>% 
            ggplot(aes(sample = resid)) +
            stat_qq() +
            stat_qq_line(col = 'red') +
            theme_minimal() +
            labs(title = 'QQ-plot for normality of residuals',
                 x = 'Theoretical quantiles',
                 y = 'Sample quantiles')
          
          plots[[i]] <- annotate_figure(ggarrange(resid_vs_fit, qqplot),
                                        top = text_grob(paste('Diagnosic plots for', methods[[i]]),size = 14))
        }
      }
      
      # I want to return the methods, models, moran tests, and diagnostic plots
      
      return_values <- list(methods, models, morans, plots)
      return(return_values)

    }
  }
  
}

# model comparison (AIC tables for different specifications)
{
  # producing metrics for each model
  
  get_metrics <- function(mod) {
    if (is.null(summary(mod)$lambda)) {
      metrics <- c(AIC(mod), logLik(mod), NA, NA)
    }
    else {
      metrics <- c(AIC(mod), logLik(mod), summary(mod)$lambda, summary(mod)$lambda.se)
    }
    return(metrics)
  }
  
  # getting metrics for a specific model list
  
  metrics <- c('AIC', 'logLik', 'lambda', 'lambda se')
  get_metrics_table <- function(model_list) {
    tibble(method = rep(methods, each = length(metrics)),
           metric = rep(metrics, times = length(methods)),
           value = unlist(lapply(model_list[[2]], get_metrics))) %>% 
      pivot_wider(names_from = metric, values_from = value)
  }
  
  # making a function to produce model comparisons table (across different weight
  # specifications)
  
  get_model_comparisons <- function(inv_dist_list, inv_dist2_list, contig_list) {
    inv_dist_list <- inv_dist_list
    get_metrics_table(inv_dist_list) %>% 
      mutate(weights = 'inv_dist') %>% 
      bind_rows(get_metrics_table(inv_dist2_list) %>% 
                  mutate(weights = 'inv_dist2')) %>% 
      bind_rows(get_metrics_table(contig_list) %>% 
                  mutate(weights = 'contig')) %>% 
      select(method, AIC, weights) %>% 
      pivot_wider(names_from = weights,
                  values_from = AIC,
                  names_prefix = 'AIC_') %>% 
      mutate(best_weights = case_when(AIC_inv_dist < AIC_inv_dist2 & AIC_inv_dist < AIC_contig ~ 'dist',
                                      AIC_inv_dist > AIC_inv_dist2 & AIC_inv_dist2 < AIC_contig ~ 'dist2',
                                      AIC_contig < AIC_inv_dist2 & AIC_inv_dist > AIC_contig ~ 'contig',
                                      TRUE ~ ''),
             best_weights = ifelse(best_weights == '', NA, best_weights))
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
  
  # generating a plot with error bars for my SAR model
  
  get_coef_plot <- function(sar_mod) {
    get_summary_df(sar_mod) %>% 
      filter(str_detect(term, 'within_between')) %>% 
      mutate(conf.low = estimate - qnorm(0.975)*std_error,
             conf.high = estimate + qnorm(0.975)*std_error,
             term = str_remove_all(term, 'within_between')) %>% 
      select(term, estimate, conf.low, conf.high) %>% 
      ggplot(aes(estimate, fct_reorder(term, estimate))) +
      geom_point() +
      geom_errorbar(aes(xmin = conf.low, xmax = conf.high)) +
      theme_minimal() +
      labs(x = 'Estimated coefficient',
           y = 'Policy category',
           subtitle = 'Relative to high-high reference group')
  }
}

sar_mod <- ie_sar
get_summary_df(sar_mod) %>% 
  filter(str_detect(term, 'within_between')) %>% 
  mutate(conf.low = exp(estimate - qnorm(0.975)*std_error),
         conf.high = exp(estimate + qnorm(0.975)*std_error),
         estimate = exp(estimate),
         term = str_remove_all(term, 'within_between')) %>% 
  select(term, estimate, conf.low, conf.high) %>% 
  ggplot(aes(estimate, fct_reorder(term, estimate))) +
  geom_point() +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high)) +
  theme_minimal() +
  labs(x = 'exp(coefficient)',
       y = 'Policy category',
       subtitle = 'Relative to high-high reference group')
