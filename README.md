# Quantifying Spillover Effects of State Abortion Restrictions
## Kayla Manning

## Important files / directories

- [`policy_scraper.R`](policy_scraper.R): This script reads in data on state abortion restrictions from 2006-2022, originally compiled by the Guttmacher Institute. All data was obtained through the [Internet Archive](https://web.archive.org/).
  - The 2006-2016 data came from PDF files obtained through the Internet Archive. The saved versions of these files are available under [raw-data/policy_archives](raw-data/policy_archives). The data were converted to dataframes using the `tabulizer` package.
  - The 2017-2022 data were scraped from HTML tables on the archived versions of the Guttmacher website.
  

## Notes for later (and to delete when they're no longer useful)

### CDC abortion surveillance 2010-2019 

source: https://www.cdc.gov/reproductivehealth/data_stats/abortion.htm

NA: Reporting area did not report, or data not reportable by CDC.	

** Reporting area did not report, or only reported by occurrence; because abortion numbers by residence are available only from other reporting areas where residents obtained abortions, data by state/area of maternal residence should be interpreted with caution.