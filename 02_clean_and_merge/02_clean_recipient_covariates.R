####################################
# Clean recipient-level covariates
####################################


# setup -------------------------------------------------------------------

rm(list = ls())       # tidy the workspace
library(tidyverse)    # for grammar
library(democracyData)# a bunch of democracy datasets


# democracy data ----------------------------------------------------------

# For democracy data, we'll use 
#   * Polity 2
#   * Freedom House
dem_data <-
  generate_democracy_scores_dataset(
    datasets = c(
      "polity_pmm", # polity
      "fh_pmm"      # political + civil liberities
    ),
    "wide"
  )

# limit to years needed (2000-2014)
dem_data <- dem_data %>%
  filter(year %in% 2000:2014)

# use iso3 code for later merging
dem_data <- dem_data %>%
  mutate(
    recipient_iso3 = countrycode::countrycode(
      cown, "cown", "iso3c"
    )
  ) %>%
  
  # Only keep vars of interest
  select(year, recipient_iso3, pmm_fh, pmm_polity)


# economic data -----------------------------------------------------------

# Variables to keep:
#   * population
#   * unemployed rate (ILO)
#   * real GDP in 2011 dollars

econ_data <- read_csv(
  paste0(getwd(), "/01_data/world_bank_data.csv")
)

econ_data <- econ_data %>%
  
  # ensure codes are iso3 codes
  mutate(
    recipient_iso3 = countrycode::countrycode(
      country, "country.name", "iso3c"
    )
  ) %>%
  
  # NAs are regions that we can drop
  filter(!is.na(recipient_iso3)) %>%
  
  # Drop human capital index
  select(-hc, -country)



# disaster data -----------------------------------------------------------

# Data on deaths from natural disasters:

disaster_data <- 
  read_csv(
    paste0(getwd(),"/01_data/deaths-natural-disasters-ihme.csv")
  )

# We already have iso3 code, so just rename:
disaster_data <- disaster_data %>%
  rename(recipient_iso3 = rcode) %>%
  select(-recipient)


# civil war data ----------------------------------------------------------

cw_data <- read_csv(
  paste0(getwd(),"/01_data/prio_civilwars.csv")
)

# Some cleaning:
cw_data <- cw_data %>%
  mutate(
    
    # replace NAs with 0
    civilwar = replace_na(civilwar, 0),
    
    # get iso3 country codes
    recipient_iso3 = 
      countrycode::countrycode(
        location, "country.name", "iso3c"
      )
  ) %>%
  
  # get rid of conflict IDs (make some trouble for merging)
  select(-conflict_id) %>%
  
  # Drop NAs 
  na.omit %>%
  
  # Reduce to one observation per year per country
  group_by(year, recipient_iso3) %>%
  summarize(
    civilwar = max(civilwar)
  )


# combine into final covariate dataset ------------------------------------

cov_data <- # recipient + year to match 
  read_csv(
    paste0(getwd(), "/01_data/clean_aid_data.csv")
  ) %>%
  select(year, recipient_iso3)

cov_list <- list(
  dem_data, econ_data, disaster_data,
  cw_data
)

start_n <- nrow(cov_data)
for(i in 1:length(cov_list)) {
  if(i != 5) {
    by_vars <- c("recipient_iso3", "year")
  } else {
    by_vars <- "recipient_iso3"
  }
  cov_data <- cov_data %>%
    left_join(
      cov_list[[i]],
      by = by_vars
    )
  if(start_n != nrow(cov_data)) 
    stop("Something went wrong in merge ", i, "!")
} 



# Clean up missing values -------------------------------------------------

# how many obs have missing data?
start_n - nrow(na.omit(cov_data)) # okay that's a lot

# Some vars with NAs we can impute missing as '0'
cov_data <- cov_data %>%
  
  # get rid of some additional junk variables
  select(-code) %>%
  
  # impute ally and civil war variables
  mutate(
    civilwar = replace_na(civilwar, 0)
  )

start_n - nrow(na.omit(cov_data)) # that's better, but not great

# Use random forests to impute remaining missing data
library(missRanger)
cov_data <- cov_data %>%
  missRanger(pmm.k = 3) # combine imputations with predictive mean matching

# for citation of missRanger, see: https://academic.oup.com/bioinformatics/article/28/1/112/219101



# save the results --------------------------------------------------------

write_csv(
  cov_data,
  paste0(getwd(), "/01_data/clean_covariate_data.csv")
)
