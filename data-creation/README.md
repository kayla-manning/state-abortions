# [`data-creation`](data-creation)

- [`make_data.R`](make_data.R): This script joins the abortion data, state policies, and control variables into a single dataframe under [`raw-data/combined_data.csv`](raw-data/combined-data.csv). This dataset is what is used in most of the exploratory data analysis, and it will be used for modeling.
- [`policy_scorer.R`](policy_scraper.R): This script converts the scraped policy data in [`cleaned_policies.csv`](raw-data/cleaned_policies.csv) into the scores contained in [`policy_scores.csv`](raw-data/policy_scores.csv).
- [`reweighted_policy_scorer.R`](reweighted_policy_scorer.R): This is nearly identical to [`policy_scorer.R`](policy_scraper.R), but it creates [`reweighted_policy_scores.csv`](raw-data/reweighted_policy_scores.csv), an alternative policy scoring dataset that weighs certain policies more than others when computing within-state scores.

#### [`scrapers`](scrapers)
- [`policy_scraper.R`](scrapers/policy_scraper.R): This script reads in data on state abortion restrictions from 2006-2022, originally compiled by the Guttmacher Institute. All data was obtained through the [Internet Archive](https://web.archive.org/).
  - The 2006-2016 data came from PDF files obtained through the Internet Archive. The saved versions of these files are available under [`raw-data/policy_archives`](raw-data/policy_archives). The data were converted to dataframes using the `tabulizer` package.
  - The 2017-2022 data were scraped from HTML tables on the archived versions of the Guttmacher website.
- [`surveillance_scraper.R`](scrapers/surveillance_scraper.R): This script contains the code used to create the [`gestational_combined.csv`](raw-data/scraped/gestational_combined.csv) data from the annual Abortion Surveillance reports compiled by the CDC.

#### [`raw-data`](raw-data)
- [`cleaned_policies.csv`](raw-data/cleaned_policies.csv): This file contains the finished dataset produced in the [`policy_scraper.R`](policy_scraper.R) script.
- [`combined_data.csv`](raw-data/combined_data.csv): This file contains the combined dataset produced in the [`make_data.R`](make_data.R) script.
- [`rewighted_combined_data.csv`](raw-data/reweighted_combined_data.csv): This file contains the combined dataset produced in the [`make_data.R`](make_data.R) script, except with the policy scores from [`reweighted_policy_scores.csv`](raw-data/reweighted_policy_scores.csv). I produced this file by replacing the filepath for the original policy score dataset in the [`make_data.R`](make_data.R) with the reweighted policy score dataset.
- [`downloads`](raw-data/downloads): This directory contains datasets downloaded from the Internet that did not require additional cleaning. Most of these datasets are for control variables, but it also includes the abortion counts by occurrence and residence.
- [`helper`](raw-data/helper): This directory contains datasets created by myself that are used to ease the data wrangling process. For example, [`raw-data/helper/nonreport_gestation.xlsx`](raw-data/helper/nonreport_gestation.xlsx) contains state-year observations that fail to report gestational age data that meets the CDC criteria.
- [`policy_archives`](raw-data/policy_archives): This folder contains the PDF files for policy data obtained through the Internet Archives for years 2006 to 2016. The data were converted to dataframes and stored as observations in [`cleaned_policies.csv`](`raw-data/cleaned_policies.csv`) using the `tabulizer` package. From 2017 onward, the Guttmacher Institute posted the state policy data in HTML tables, which allowed for direct scraping. 
- [`policy_scores.csv`](raw-data/policy_scores.csv): This dataset contains the within-state and interstate policy scores derived in the [`policy_scraper.R`](policy_scraper.R) script.
- [`scraped`](raw-data/scraped): This directory contains the raw data obtained through scrapers. 
  - [`gestational_combined.csv`](raw-data/scraped/gestation_combined.csv) contains the cleaned gestational age data from the various sources.
  - [`scraped_policies.csv`](raw-data/scraped/scraped_policies.csv) contains the policy data scraped from the Internet Archive before it was processed, combined with the PDF data from [`policy_archives`](raw-data/policy_archives), and saved under [`cleaned_policies.csv`](raw-data/cleaned_policies.csv). 
  