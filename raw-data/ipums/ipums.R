# NOTE: To load data, you must download both the extract's data and the DDI
# and also set the working directory to the folder with these files (or change the path below).

if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")
library(janitor)
library(tidyverse)
library(sjlabelled)
library(labelled)
library(haven)

ddi <- read_ipums_ddi("raw-data/ipums/usa_00011.xml")
data <- read_ipums_micro(ddi)

# now I want to clean the data to be state-level... need to redo this & account
# for suppressed responses or other options for categories that aren't simply
# binary

ipums <- data %>% 
  clean_names() %>% 
  group_by(year, stateicp) %>% 
  summarise(pop = n()*100,
            prop_female = mean(sex == 2),
            pop_female = pop * prop_female,
            prop_pop_btwn_18_45 = mean(age %in% 18:45),
            prop_div_in_yr = mean(divinyr == 2),
            prop_married = mean(marst == 1),
            prop_fert_in_yr = mean(fertyr == 2),
            prop_blk = mean(racblk == 2),
            prop_wht = mean(racwht == 2),
            prop_priv_hcov = mean(hcovpriv == 2),
            prop_medicaid = mean(hinscaid == 2),
            prop_bach_plus = mean(educd %in% 101:116),
            prop_unemployed = mean(empstatd == 20),
            .groups = 'drop') %>% 
  mutate(stateicp = as_label(stateicp)) %>% 
  rename(state = stateicp)

# once I've done a better job of cleaning this up nicely, I should save it as a
# CSV so I don't have to run this script again (I'll just read the csv into the
# eda rmd & join with the abortion data)
