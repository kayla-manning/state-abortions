# Quantifying Spillover Effects of State Abortion Restrictions
## Kayla Manning

## Important files / directories

### Home directory
- [`eda.Rmd`](eda.Rmd): This file contains the code to produce the graphics in my exploratory data analysis [draft](https://docs.google.com/document/d/1Qx2dA7brUXPuVSNrtzat9nIu997nC7L1ukSNEbSgXyU/edit?usp=sharing).
- [`models.Rmd`](models.Rmd): This file contains the code to produce the models and graphs used in the model analysis [draft](https://docs.google.com/document/d/1Q4LM2GzBWwJPKrnyrXjgPlXF6KYtuPUjVUYLoExEdwQ/edit).

### [`model-helpers`](model-helpers)
This directory contains scripts that either directly fit or aid in the process of fitting the spatial and nonspatial models used in [`models.Rmd`](models.Rmd).

### [`data-creation`](data-creation)
This directory contains the scripts used to scrape, clean, and merge datasets. In addition, the [`raw-data`](data-creation/raw-data) subdirectory contains all data downloads from the Internet and CSV files used in this analysis.

### [`archive`](archive)
This directory contains files that aren't being directly used but contain some code that might be useful in the future.

## Data sources

### Policies
- State policies (current): https://www.guttmacher.org/state-policy/explore/overview-abortion-laws

### Dependent variables
- Abortion surveillance report with gestational data (2019): https://www.cdc.gov/mmwr/volumes/70/ss/ss7009a1.htm
- Abortions by occurrence and residence: https://www.cdc.gov/reproductivehealth/data_stats/abortion.htm
- Births: https://wonder.cdc.gov/natality-current.html
- Additional state abortion counts: https://www.guttmacher.org/sites/default/files/report_pdf/abortion-incidence-service-availability-us-2017.pdf

### Control variables
- Presidential vote share: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/42MVDX
- Median Household Income by State: Table H-8 from https://www.census.gov/data/tables/time-series/demo/income-poverty/historical-income-households.html
- Education data: https://ncses.nsf.gov/indicators/states/indicator/bachelors-degree-holders-per-25-44-year-olds
- Proportion non-white & proportion Hispanic: SC-EST2019-ALLDATA5 from https://www.census.gov/newsroom/press-kits/2020/population-estimates-detailed.html


