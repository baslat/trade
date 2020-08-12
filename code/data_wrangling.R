# Helping with trade data wrangling
# devtools::install_github("ropensci/tradestatistics")
library(tradestatistics)
library(tidyverse)

# Which of T's countries are available?
tc <- read_csv("data/wanted_countries.csv")

# Any countries in T's list that aren't available in the trade data?
tc %>% 
  anti_join(ots_countries, by = c("country" = "country_name_english"))




# Extract the ISO code for T's countries
ts_countries <- tc %>% 
  inner_join(ots_countries, by = c("country" = "country_name_english")) %>% 
  pull(country_iso)


# What years does she want?
years <- c(1962:2018) # this is all


# Create the data table
# This takes about an hour as it's querying and downloading a lot
trade_data_raw <- ots_create_tidy_data(
  years = years,
  reporters = ts_countries,
  partners = "all",
  table = "yrp"
) 

# Create the percentages
trade_data <- trade_data_raw %>% 
  as_tibble() %>% 
  arrange(year, reporter_fullname_english, partner_fullname_english) %>% 
  group_by(year, reporter_fullname_english) %>% 
  mutate(report_import_pct = import_value_usd / sum(import_value_usd),
         report_export_pct = export_value_usd / sum(export_value_usd))

# Write the data
trade_data %>% 
  write_csv("trade_flows_open_trade_stats.csv")
