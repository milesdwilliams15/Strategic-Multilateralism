##################
# Clean aid data
##################


# setup -------------------------------------------------------------------

rm(list = ls())
library(tidyverse)
library(countrycode)


# raw data ----------------------------------------------------------------

data <- 
  read_csv(
    "01_data/data.csv"
  )
data <- data[-nrow(data), ]



# clean -------------------------------------------------------------------

schedule <- expand.grid(
  Recipient = unique(data$Recipient),
  Year = 2008:2012
)
data <- left_join(
  schedule, data, by = c("Recipient", "Year")
)
data <- data %>%
  mutate_if(
    is.numeric,
    ~ replace_na(.x, 0)
  ) %>% as_tibble() %>%
  group_by(Year) %>%
  mutate(
    WB_budget = max(WB_budget)
  ) %>%
  ungroup %>%
  mutate_if(
    is.numeric,
    ~ (.x > 0) * .x
  )

data <- data %>%
  mutate(
    Recipient = str_replace(
      Recipient, "C\xf4te d'Ivoire", "Ivory Coast"
    ),
    recipient_iso3 = countrycode(Recipient, "country.name", "iso3c")
  ) %>%
  na.omit %>%
  rename(
    c(recipient = Recipient, year = Year, dac = DAC, wb = WB, 
      non_dac = nonDAC, g7 = G7, wb_budget = WB_budget)
  )



# save --------------------------------------------------------------------

write_csv(
  data,
  file = "01_data/clean_aid_data.csv"
)
