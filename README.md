# Quantifying Spillover Effects of State Abortion Restrictions
## Kayla Manning

## Important files / directories

- [`policy_scraper.R`](policy_scraper.R): This script reads in data on state abortion restrictions from 2006-2022, originally compiled by the Guttmacher Institute. All data was obtained through the [Internet Archive](https://web.archive.org/).
  - The 2006-2016 data came from PDF files obtained through the Internet Archive. The saved versions of these files are available under [`raw-data/policy_archives`](raw-data/policy_archives). The data were converted to dataframes using the `tabulizer` package.
  - The 2017-2022 data were scraped from HTML tables on the archived versions of the Guttmacher website.
  

## Notes for later (and to delete when they're no longer useful)

- need to figure out how to treat permanently / temporarily enjoined in the data... right now I have them all coded as 'Enjoined', but I'm not sure if I should distinguish between the two. furthermore, I do not know how many points I should assign them in the score
- [`policy_cleaning.R`](policy_cleaning.R) has some code that calculates the scores from the policy data. However, I will want to clean up my methodology before it's final. right now it is a very coarse measure and I need something more well-documented and theoretically sound before I finalize the script... also, the script is not long at all, so I could probably just add it to the end of the [`policy_scraper.R`](policy_scraper.R) script

### CDC abortion surveillance 2010-2019 

source: https://www.cdc.gov/reproductivehealth/data_stats/abortion.htm

### Data sources

Presidential vote share - MIT Elections Project
Median Household Income by State - Table H-8 from https://www.census.gov/data/tables/time-series/demo/income-poverty/historical-income-households.html

Education data:
https://ncses.nsf.gov/indicators/states/indicator/bachelors-degree-holders-per-25-44-year-olds
