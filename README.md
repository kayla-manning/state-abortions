# Abortion Policy Spillover: When Policy Consequences Contradict the Intent
## Kayla Manning

This repository contains the code for my joint thesis in Statistics and Government. The final product is available at [`final_thesis.pdf`](final_thesis.pdf).

## Important files / directories

### Home directory
- [`eda.Rmd`](eda.Rmd): This file contains the code to produce the graphics in Chapters 3 and 6 of my thesis.
- [`models.Rmd`](models.Rmd): This file contains the code to produce the models and graphs used in Chapters 4-5.

### [`model-helpers`](model-helpers)
This directory contains scripts that either directly fit or aid in the process of fitting the spatial and nonspatial models used in [`models.Rmd`](models.Rmd).

### [`data-creation`](data-creation)
This directory contains the scripts used to scrape, clean, and merge datasets. In addition, the [`raw-data`](data-creation/raw-data) subdirectory contains all data downloads from the Internet and CSV files used in this analysis.

### [`figures`](figures)
This directory contains all maps and charts included in the final thesis. I generate the tables with `kable` LaTeX code, so they are not included in this directory.

### [`archive`](archive)
This directory contains files that aren't being directly used but contain some code that might be useful in the future.


