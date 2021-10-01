# Notes on Data Cleaning

Aid Data (`01_clean_aid_data.R`):

  - Aid data comes from `stats.oecd.org`: Aid (ODA) commitments to countries and regions [DAC3a]
  - Data is for 2000-2014.
  - Aid commitments are in millions of 2019 constant US dollars.
  
Covariate data (`02_clean_recipient_covariates.R`) comes from a variety of sources as specified in code annotations.

The file `03_final_merge.R` finalizes the merge between datasets.