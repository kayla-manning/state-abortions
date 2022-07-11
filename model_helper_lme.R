# creating list of variables to iterate through

outcome_vars <- c('rate', 'ie', 'late_early')
fitted_models <- list()

for (var in outcome_vars) {
  
  # fitting models for each of the response variables
  
  if (var == 'rate') {
    
    category_f <- as.formula(sqrt(abortion_per_1k_births) ~ within_between + pct_bachelors + prop_hisp + 
                               prop_nonwhite + hh_income + dem_2party)
    raw_f <- as.formula(sqrt(abortion_per_1k_births) ~ surrounding_score * within_score + pct_bachelors + 
                          prop_hisp + prop_nonwhite + hh_income + dem_2party)
    
  }
  
  if (var == 'ie') {
    
    category_f <- as.formula(log(ie_ratio) ~ within_between + pct_bachelors + prop_hisp + 
                               prop_nonwhite + hh_income + dem_2party + abortions)
    raw_f <- as.formula(log(ie_ratio) ~ surrounding_score * within_score + pct_bachelors + 
                          prop_hisp + prop_nonwhite + hh_income + dem_2party + abortions)
    
  }
  
  if (var == 'late_early') {
    
    category_f <- as.formula(sqrt(late_to_early) ~ within_between + pct_bachelors + prop_hisp + 
                               prop_nonwhite + hh_income + dem_2party + abortions)
    raw_f <- as.formula(sqrt(late_to_early) ~ surrounding_score * within_score + pct_bachelors + 
                          prop_hisp + prop_nonwhite + hh_income + dem_2party + abortions)
    
  }
  
  # creating list of spatial structures so I can fit models in a loop
  
  struct_types <- c('none', 'gaus')
  cor_structs <- list(NA, corGaus(1, form = ~ longitude + latitude | year, nugget = TRUE))
  
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
                      random = ~ longitude + latitude | year,
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
  
