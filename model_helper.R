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
    
    }
  
  # I call this within a few of my functions
  
  methods <- c('category_lm', 'category_car', 'category_errorsar', 'category_lagsar',
               'raw_lm', 'raw_car', 'raw_errorsar', 'raw_lagsar')
               
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
      
      category_f <- as.formula(abortion_per_1k_births ~ within_between + pct_bachelors + prop_hisp + 
                        prop_nonwhite + hh_income + dem_2party + as.factor(year))
      raw_f <- as.formula(abortion_per_1k_births ~ surrounding_score * within_score + pct_bachelors + 
                            prop_hisp + prop_nonwhite + hh_income + dem_2party + 
                            as.factor(year))
      
    }
    
    if (var == 'ie') {
      
      category_f <- as.formula(log(ie_ratio) ~ within_between + pct_bachelors + prop_hisp + 
                                 prop_nonwhite + hh_income + dem_2party + as.factor(year) + 
                                 abortions)
      raw_f <- as.formula(log(ie_ratio) ~ surrounding_score * within_score + pct_bachelors + 
                            prop_hisp + prop_nonwhite + hh_income + dem_2party + as.factor(year) + 
                            abortions)
      
    }
    
    if (var == 'late_early') {
      
      category_f <- as.formula(sqrt(late_to_early) ~ within_between + pct_bachelors + prop_hisp + 
                                 prop_nonwhite + hh_income + dem_2party + as.factor(year) + 
                                 abortions)
      raw_f <- as.formula(sqrt(late_to_early) ~ surrounding_score * within_score + pct_bachelors + 
                            prop_hisp + prop_nonwhite + hh_income + dem_2party + as.factor(year) + 
                            abortions)
      
    }
    
    # fitting models
    
    {
      # using the policy categories
      
      category_lm <- lm(category_f, data = usa@data)
      category_car <- spautolm(category_f,
                               data = usa@data,
                               family = 'CAR',
                               listw = weights_matrix)
      category_errorsar <- errorsarlm(category_f,
                                      data = usa@data,
                                      listw = weights_matrix)
      category_lagsar <- lagsarlm(category_f,
                                  data = usa@data,
                                  listw = weights_matrix,
                                  tol.solve=1.0e-30)
      # category_sma <- spautolm(category_f,
      #                          data = usa@data,
      #                          family = 'SMA',
      #                          listw = weights_matrix,
      #                          tol.solve = 1.0e-50)
      
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
      # raw_sma <- spautolm(raw_f,
      #                     data = usa@data,
      #                     family = 'SMA',
      #                     listw = weights_matrix,
      #                     tol.solve=1.0e-50)
    }
    
    # creating lists to store everything
    
    {
      models <- list(category_lm, category_car, category_errorsar, category_lagsar,
                     raw_lm, raw_car, raw_errorsar, raw_lagsar)
      morans <- vector('list', length = length(models))
      plots <- vector('list', length = length(models))
    }
    
    for (i in 1:length(models)) {
      
      ################
      #### MORAN
      ################
      
      {
        # conduct a Moran test of the residuals, where p < 0.05 provides sufficient
        # evidence of additional autocorrelation not captured by the model
        
        morans[[i]] <- moran.mc(residuals(models[[i]]), 
                                listw = weights_matrix, 
                                nsim = 9999,
                                alternative = 'two.sided')
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
    print(paste('done with', var))
    return(return_values)
    
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

# trying out nlme model... jittering lat/long so that the model will fit

usa@data$latitude <- jitter(usa@data$latitude)
usa@data$longitude <- jitter(usa@data$longitude)
test <- lme(log(ie_ratio) ~ surrounding_score * within_score + pct_bachelors + 
            prop_hisp + prop_nonwhite + hh_income + dem_2party + abortions,
          data = usa@data,
          #correlation = corGaus(1, form = ~ latitude + longitude|year),
          random = ~ longitude + latitude | year)
summary(test)
test2gaus <- update(test, 
                    correlation = corGaus(form = ~ longitude + latitude | year),
                    control = lmeControl(opt = "optim"))
summary(test2)
AIC(test2)

# note that contiguous neighbors are much less significant... is that the most
# important relationship?

moran.mc(residuals(test2), 
         weights.contig.W, 
         nsim = 999, 
         alternative = 'two.sided')

# doing the same thing with different correlation structure

test2exp <- update(test, 
                   correlation = corExp(form = ~ longitude + latitude | year),
                   control = lmeControl(opt = "optim"))
AIC(test2exp)

test2lin <- update(test, 
                   correlation = corLin(form = ~ longitude + latitude | year),
                   control = lmeControl(opt = "optim"))
AIC(test2lin)

test2ratio <- update(test, 
                   correlation = corRatio(form = ~ longitude + latitude | year),
                   control = lmeControl(opt = "optim"))
AIC(test2ratio)

test2sphere <- update(test, 
                     correlation = corSpher(form = ~ longitude + latitude | year),
                     control = lmeControl(opt = "optim"))
AIC(test2sphere)

test2car <- update(test, 
                    correlation = corCAR1(form = ~ longitude + latitude | year),
                    control = lmeControl(opt = "optim"))
AIC(test2car)

test2compsymm <- update(test, 
                   correlation = corCompSymm(form = ~ longitude + latitude | year),
                   control = lmeControl(opt = "optim"))
AIC(test2compsymm)

test2ar1 <- update(test,
                   correlation = corAR1(form = ~ longitude + latitude | year))
AIC(test2ar1)

c('none', 'gaus', 'exp', 'lin', 'ratio', 'spher', 'symm', 'compsymm')
