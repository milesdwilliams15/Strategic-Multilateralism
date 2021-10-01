###########################
# Final merge of datasets
###########################


# setup -------------------------------------------------------------------

rm(list = ls())
library(tidyverse)


# data --------------------------------------------------------------------

aid_data <- 
  read_csv("01_data/clean_aid_data.csv")
cov_data <-
  read_csv("01_data/clean_covariate_data.csv")

# merge -------------------------------------------------------------------

data <- left_join(
  aid_data, cov_data,
  by = c("recipient_iso3", "year")
)



# save --------------------------------------------------------------------

write_csv(
  data,
  file = "01_data/final_data.csv"
)
