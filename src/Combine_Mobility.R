# Libraries
library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggrepel)
library(ggcorrplot)

###### 2020 US Region Mobility ######
us_mobility_2020 <- as_tibble((read_csv("~/Documents/Development/GradSchool/data-mining-COVID-Project/data/2020_US_Region_Mobility_Report.csv")))
# Remove columns #
us_mobility_2020 <- us_mobility_2020 %>% select(-place_id)
us_mobility_2020 <- us_mobility_2020 %>% select(-metro_area)
us_mobility_2020 <- us_mobility_2020 %>% select(-iso_3166_2_code)
# Rename columns #
us_mobility_2020 <- rename(us_mobility_2020, `Country Code` = `country_region_code`)
us_mobility_2020 <- rename(us_mobility_2020, `Country` = `country_region`)
us_mobility_2020 <- rename(us_mobility_2020, `State` = `sub_region_1`)
us_mobility_2020 <- rename(us_mobility_2020, `County` = `sub_region_2`)
us_mobility_2020 <- rename(us_mobility_2020, `FIPS` = `census_fips_code`)
us_mobility_2020 <- rename(us_mobility_2020, `County` = `sub_region_2`)
us_mobility_2020 <- rename(us_mobility_2020, `Date` = `date`)
us_mobility_2020 <- rename(us_mobility_2020, `Retail & Recreation % Delta` = `retail_and_recreation_percent_change_from_baseline`)
us_mobility_2020 <- rename(us_mobility_2020, `Grocery & Pharmacy % Delta` = `grocery_and_pharmacy_percent_change_from_baseline`)
us_mobility_2020 <- rename(us_mobility_2020, `Parks % Delta` = `parks_percent_change_from_baseline`)
us_mobility_2020 <- rename(us_mobility_2020, `Public Transit % Delta` = `transit_stations_percent_change_from_baseline`)
us_mobility_2020 <- rename(us_mobility_2020, `Workplaces % Delta` = `workplaces_percent_change_from_baseline`)
us_mobility_2020 <- rename(us_mobility_2020, `Residential % Delta` = `residential_percent_change_from_baseline`)
# Removing NAs #
us_mobility_2020 <- us_mobility_2020 %>% na.omit(`sub_region_1`)
###### 2021 US Region Mobility ######
us_mobility_2021 <- as_tibble((read_csv("~/Documents/Development/GradSchool/data-mining-COVID-Project/data/2021_US_Region_Mobility_Report.csv")))
# Remove columns #
us_mobility_2021 <- us_mobility_2021 %>% select(-place_id)
us_mobility_2021 <- us_mobility_2021 %>% select(-metro_area)
us_mobility_2021 <- us_mobility_2021 %>% select(-iso_3166_2_code)
# Rename columns #
us_mobility_2021 <- rename(us_mobility_2021, `Country Code` = `country_region_code`)
us_mobility_2021 <- rename(us_mobility_2021, `Country` = `country_region`)
us_mobility_2021 <- rename(us_mobility_2021, `State` = `sub_region_1`)
us_mobility_2021 <- rename(us_mobility_2021, `County` = `sub_region_2`)
us_mobility_2021 <- rename(us_mobility_2021, `FIPS` = `census_fips_code`)
us_mobility_2021 <- rename(us_mobility_2021, `County` = `sub_region_2`)
us_mobility_2021 <- rename(us_mobility_2021, `Date` = `date`)
us_mobility_2021 <- rename(us_mobility_2021, `Retail & Recreation % Delta` = `retail_and_recreation_percent_change_from_baseline`)
us_mobility_2021 <- rename(us_mobility_2021, `Grocery & Pharmacy % Delta` = `grocery_and_pharmacy_percent_change_from_baseline`)
us_mobility_2021 <- rename(us_mobility_2021, `Parks % Delta` = `parks_percent_change_from_baseline`)
us_mobility_2021 <- rename(us_mobility_2021, `Public Transit % Delta` = `transit_stations_percent_change_from_baseline`)
us_mobility_2021 <- rename(us_mobility_2021, `Workplaces % Delta` = `workplaces_percent_change_from_baseline`)
us_mobility_2021 <- rename(us_mobility_2021, `Residential % Delta` = `residential_percent_change_from_baseline`)
# Removing NAs #
us_mobility_2021 <- us_mobility_2021 %>% na.omit(`sub_region_1`)
###### 2022 US Region Mobility ######
us_mobility_2022 <- as_tibble((read_csv("~/Documents/Development/GradSchool/data-mining-COVID-Project/data/2021_US_Region_Mobility_Report.csv")))
us_mobility_2022 <- as_tibble((read_csv("~/Documents/Development/GradSchool/data-mining-COVID-Project/data/2021_US_Region_Mobility_Report.csv")))
# Remove columns #
us_mobility_2022 <- us_mobility_2022 %>% select(-place_id)
us_mobility_2022 <- us_mobility_2022 %>% select(-metro_area)
us_mobility_2022 <- us_mobility_2022 %>% select(-iso_3166_2_code)
# Rename columns #
us_mobility_2022 <- rename(us_mobility_2022, `Country Code` = `country_region_code`)
us_mobility_2022 <- rename(us_mobility_2022, `Country` = `country_region`)
us_mobility_2022 <- rename(us_mobility_2022, `State` = `sub_region_1`)
us_mobility_2022 <- rename(us_mobility_2022, `County` = `sub_region_2`)
us_mobility_2022 <- rename(us_mobility_2022, `FIPS` = `census_fips_code`)
us_mobility_2022 <- rename(us_mobility_2022, `County` = `sub_region_2`)
us_mobility_2022 <- rename(us_mobility_2022, `Date` = `date`)
us_mobility_2022 <- rename(us_mobility_2022, `Retail & Recreation % Delta` = `retail_and_recreation_percent_change_from_baseline`)
us_mobility_2022 <- rename(us_mobility_2022, `Grocery & Pharmacy % Delta` = `grocery_and_pharmacy_percent_change_from_baseline`)
us_mobility_2022 <- rename(us_mobility_2022, `Parks % Delta` = `parks_percent_change_from_baseline`)
us_mobility_2022 <- rename(us_mobility_2022, `Public Transit % Delta` = `transit_stations_percent_change_from_baseline`)
us_mobility_2022 <- rename(us_mobility_2022, `Workplaces % Delta` = `workplaces_percent_change_from_baseline`)
us_mobility_2022 <- rename(us_mobility_2022, `Residential % Delta` = `residential_percent_change_from_baseline`)
# Removing NAs #
us_mobility_2022 <- us_mobility_2022 %>% na.omit(`sub_region_1`)
##### Data Aggregate by Mean #####
US2020Aggregate <- aggregate( cbind(`Retail & Recreation % Delta`, 
                                    `Grocery & Pharmacy % Delta`, 
                                    `Parks % Delta`, 
                                    `Public Transit % Delta`, 
                                    `Workplaces % Delta`, 
                                    `Residential % Delta`) ~ 
                                County, us_mobility_2020, mean)

US2021Aggregate <- aggregate( cbind(`Retail & Recreation % Delta`, 
                                    `Grocery & Pharmacy % Delta`, 
                                    `Parks % Delta`, 
                                    `Public Transit % Delta`, 
                                    `Workplaces % Delta`, 
                                    `Residential % Delta`) ~ 
                                County, us_mobility_2021, mean)

US2022Aggregate <- aggregate( cbind(`Retail & Recreation % Delta`, 
                                    `Grocery & Pharmacy % Delta`, 
                                    `Parks % Delta`, 
                                    `Public Transit % Delta`, 
                                    `Workplaces % Delta`, 
                                    `Residential % Delta`) ~ 
                                County, us_mobility_2022, mean)

AggregateMeanList <- list(US2020Aggregate, US2020Aggregate, US2020Aggregate)
AggregateMeanDF <- Reduce(function(x, y) merge(x, y, all=TRUE), AggregateMeanList) 
head(AggregateDF)
##### Data Aggregate #####
AggregateList <- list(us_mobility_2020, us_mobility_2021, us_mobility_2022)
AggregateDF <- Reduce(function(x, y) merge(x, y, all=TRUE), AggregateList)

##### Export to CSV#####
write_csv(AggregateMeanDF, file = "Project1_DataPrep_AggregMean.csv")
write_csv(AggregateDF, file = "Project1_DataPrep_Aggreg.csv")