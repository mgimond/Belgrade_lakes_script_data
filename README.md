[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.3416552.svg)](https://doi.org/10.5281/zenodo.3416552)

# Belgrade lakes paper R-scripts
Data and R script to accompany paper on Belgrade  lakes shoreline study

## Primary scripts

* R_scripts/data_prep.R: Creates an .RData file under data_derived/ for use in the plotting and analysis scripts.
* R_scripts/stats_function.R: Generates the statistical outputs. Data tables are saved under data_derived/.
* R_scripts/plots.R: Generates all plots presented in paper.

## Miscellaneous scripts (not necessarily presented in the paper and may not have been completely validated)

* R_scripts/percent_organics_median_polish.R: This R script creates median polish tables from the organics.xls dataset
* R_scripts/explore_for_transformation.R: This R script was initially used to check for data distribution and suggested re-expression. This was no longer needed given the statistical analyses adopted in the paper.
* R_scripts/holm_vs_bh_correction.R: R script used to compare holm vs BH corrections.
