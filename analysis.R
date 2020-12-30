library("tidyverse")
library("plotly")
library("shiny")

data <- read.csv("owid-co2-data.csv")

#######################################
# 5 pieces of summary information
#######################################
# FOCUS: carbon emission in different sectors US, 1990-2014

# 1. number of features and elements
num_features <- length(data)
num_elem <- length(data$iso_code) * num_features
  
# 2. number of countries and years
num_countries <- length(unique(data$country))
range_years <- range(data$year)
num_years <- max(data$year) - min(data$year)
  
# 3. range of co2 in US b/t 1990-2014 as %
data_in_use <- data %>%
  filter(country == "United States") %>%
  filter(year >= 1990) %>%
  filter(year <= 2014) %>%
  select(year, co2, cement_co2, coal_co2, flaring_co2, gas_co2, oil_co2)
  
range_co2 <- range(data_in_use$co2)
  
# 4. Different economic sectors and their ranges:
#    cement_co2, coal_co2, flaring_co2, gas_co2, oil_co2
range_cement_co2 <- range(data_in_use$cement_co2)

range_coal_co2 <- range(data_in_use$coal_co2)

range_flaring_co2 <- range(data_in_use$flaring_co2)

range_gas_co2 <- range(data_in_use$gas_co2)

range_oil_co2 <- range(data_in_use$oil_co2)

# 5. proportion of non-NA values from 1990-2014 for US values
prop_valid_data <- round((sum(!is.na(data_in_use)) /
                            (length(data_in_use) *
                               length(data_in_use$year))) * 100)

data_to_render <- data_in_use %>%
  rename(Total = co2,
           Cement = cement_co2,
           Coal = coal_co2,
           Flaring = flaring_co2,
           Gas = gas_co2,
           Oil = oil_co2) %>%
  gather(key = "source", value = "co2_output", -year)
