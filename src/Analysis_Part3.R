###### Libraries ######
library(dplyr)
library(tibble)
library(tidyverse)
library(readr)
library(stringr)
library(ggplot2)
library(seriation)
library(FSelector)
library(caret)

###### Data tibbles ######
covid_cases_newest <- as_tibble(read_csv("~/Documents/Development/GradSchool/data-mining-COVID-Project/data/covid_census_newest.csv"))
#covid_cases_newest <- as_tibble(read_csv("D:/dev/GradSchool/data-mining-COVID-Project/data/covid_census_newest.csv"))
###### Light cleaning  #######
covid_cases_newest <- covid_cases_newest %>% mutate_if(is.character, factor)
covid_cases_newest <- covid_cases_newest %>% filter(confirmed_cases > 0)
covid_cases_newest <- covid_cases_newest %>% arrange(desc(confirmed_cases))

###### Calculate rates per 1000 people ######
covid_cases_newest <- covid_cases_newest %>% mutate(
  cases_per_10000 = confirmed_cases/total_pop * 10000, 
  deaths_per_10000 = deaths/total_pop * 10000, 
  death_per_case = (deaths_per_10000/cases_per_10000) * 10000)

###### Selecting our features ######
covid_cases_newest <- covid_cases_newest %>% select(-`county_fips_code`)
covid_cases_newest <- rename(covid_cases_newest, County = `county_name`)
covid_cases_newest <- rename(covid_cases_newest, State = `state`)
covid_cases_newest <- covid_cases_newest %>% select(-state_fips_code)
covid_cases_newest <- rename(covid_cases_newest, Date = `date`)
covid_cases_newest <- rename(covid_cases_newest, Confirmed = `confirmed_cases`)
covid_cases_newest <- rename(covid_cases_newest, Deaths = `deaths`)
covid_cases_newest <- rename(covid_cases_newest, `Geographic ID` = `geo_id`)
covid_cases_newest <- rename(covid_cases_newest, Population = `total_pop`)
covid_cases_newest <- rename(covid_cases_newest, `Median Age` = `median_age`)
covid_cases_newest <- rename(covid_cases_newest, `Number of Households` = `households`)
covid_cases_newest <- rename(covid_cases_newest, `Median Income` = `median_income`)
covid_cases_newest <- rename(covid_cases_newest, `Income Per Capita` = `income_per_capita`)
covid_cases_newest <- rename(covid_cases_newest, Employed = `employed_pop`)
covid_cases_newest <- rename(covid_cases_newest, Unemployed = `unemployed_pop`)
covid_cases_newest <- rename(covid_cases_newest, `Male Population` = `male_pop`)
covid_cases_newest <- rename(covid_cases_newest, `Female Population` = `female_pop`)
covid_cases_newest <- rename(covid_cases_newest, `Family Households` = `family_households`)
covid_cases_newest <- rename(covid_cases_newest, `Non-family Households` = `nonfamily_households`)
covid_cases_newest <- rename(covid_cases_newest, White = `white_pop`)
covid_cases_newest <- rename(covid_cases_newest, Black = `black_pop`)
covid_cases_newest <- rename(covid_cases_newest, Asian = `asian_pop`)
covid_cases_newest <- rename(covid_cases_newest, Hispanic = `hispanic_pop`)
covid_cases_newest <- rename(covid_cases_newest, `Not Hispanic` = `not_hispanic_pop`)
covid_cases_newest <- rename(covid_cases_newest, `American Indian` = `amerindian_pop`)
covid_cases_newest <- rename(covid_cases_newest, Other = `other_race_pop`)
covid_cases_newest <- rename(covid_cases_newest, `Two or More Races` = `two_or_more_races_pop`)
covid_cases_newest <- rename(covid_cases_newest, `Commuters by Public Transit` = `commuters_by_public_transportation`)
covid_cases_newest <- rename(covid_cases_newest, `Housing Units` = `housing_units`)
covid_cases_newest <- rename(covid_cases_newest, `Vacant Housing Units` = `vacant_housing_units`)
covid_cases_newest <- rename(covid_cases_newest, `Vacant Housing Units For Rent` = `vacant_housing_units_for_rent`)
covid_cases_newest <- rename(covid_cases_newest, `Vacant Housing Units For Sale` = `vacant_housing_units_for_sale`)
covid_cases_newest <- rename(covid_cases_newest, `Median Rent` = `median_rent`)
covid_cases_newest <- rename(covid_cases_newest, `Percent Income Spent on Rent` = `percent_income_spent_on_rent`)
covid_cases_newest <- rename(covid_cases_newest, `Owner Occupied Housing Units` = `owner_occupied_housing_units`)
covid_cases_newest <- rename(covid_cases_newest, `Million Dollar Houses` = `million_dollar_housing_units`)
covid_cases_newest <- rename(covid_cases_newest, `Commuters 16+` = `commuters_16_over`)
covid_cases_newest <- rename(covid_cases_newest, `Work From Home` = `worked_at_home`)
covid_cases_newest <- rename(covid_cases_newest, `Poverty` = `poverty`)
covid_cases_newest <- rename(covid_cases_newest, `Population Determined Poverty Status` = `pop_determined_poverty_status`)
covid_cases_newest <- rename(covid_cases_newest, `Has Car` = `one_car`)
covid_cases_newest <- rename(covid_cases_newest, `Has Two Cars` = `two_cars`)
covid_cases_newest <- rename(covid_cases_newest, `Has Three Cars` = `three_cars`)
covid_cases_newest <- rename(covid_cases_newest, `Has Four+ Cars` = `four_more_cars`)
covid_cases_newest <- rename(covid_cases_newest, `Has No Car(s)` = `no_car`)
covid_cases_newest <- rename(covid_cases_newest, `Non-US Citizen` = `not_us_citizen_pop`)
covid_cases_newest <- rename(covid_cases_newest, `Income Less Than $10,000` = `income_less_10000`)
covid_cases_newest <- rename(covid_cases_newest, `Income Between $10,000 and $14,999` = `income_10000_14999`)
covid_cases_newest <- rename(covid_cases_newest, `Income Between $15,000 and $19,999` = `income_15000_19999`)
covid_cases_newest <- rename(covid_cases_newest, `Income Between $20,000 and $24,999` = `income_20000_24999`)
covid_cases_newest <- rename(covid_cases_newest, `Income Between $25,000 and $29,999` = `income_25000_29999`)
covid_cases_newest <- rename(covid_cases_newest, `Income Between $30,000 and $34,999` = `income_30000_34999`)
covid_cases_newest <- rename(covid_cases_newest, `Income Between $35,000 and $39,999` = `income_35000_39999`)
covid_cases_newest <- rename(covid_cases_newest, `Income Between $40,000 and $44,999` = `income_40000_44999`)
covid_cases_newest <- rename(covid_cases_newest, `Income Between $45,000 and $49,999` = `income_45000_49999`)
covid_cases_newest <- rename(covid_cases_newest, `Income Between $50,000 and $59,999` = `income_50000_59999`)
covid_cases_newest <- rename(covid_cases_newest, `Income Between $60,000 and $74,999` = `income_60000_74999`)
covid_cases_newest <- rename(covid_cases_newest, `Income Between $75,000 and $99,999` = `income_75000_99999`)
covid_cases_newest <- rename(covid_cases_newest, `Income Between $100,000 and $124,999` = `income_100000_124999`)
covid_cases_newest <- rename(covid_cases_newest, `Income Between $125,000 and $149,999` = `income_125000_149999`)
covid_cases_newest <- rename(covid_cases_newest, `Income Between $150,000 and $199,999` = `income_150000_199999`)
covid_cases_newest <- rename(covid_cases_newest, `Income Between $200,000+` = `income_200000_or_more`)
covid_cases_newest <- rename(covid_cases_newest, `Population Not In Labor Force` = `not_in_labor_force`)
covid_cases_newest <- rename(covid_cases_newest, `Population In Labor Force` = `pop_in_labor_force`)
covid_cases_newest <- rename(covid_cases_newest, `GINI Index` = `gini_index`)
covid_cases_newest <- rename(covid_cases_newest, `Households Using Public Assistance or Food Stamps` = `households_public_asst_or_food_stamps`)

###### Grouping Education Level ######
## Higher Education(associates, bachelors, masters)(includes partial)
covid_cases_newest <- covid_cases_newest %>% 
  mutate(`Has Graduated Higher Education` = (`associates_degree` + 
                                               `bachelors_degree` + 
                                               `masters_degree` + 
                                               `graduate_professional_degree`))
covid_cases_newest <- covid_cases_newest %>% 
  select(-c(`associates_degree`, `bachelors_degree`, `masters_degree`,
            `graduate_professional_degree`))
## High School Education
covid_cases_newest <- covid_cases_newest %>% 
  mutate(`Has Graduated High School` = (`high_school_including_ged` + 
                                          `high_school_diploma`))
covid_cases_newest <- covid_cases_newest %>% 
  select(-c(high_school_including_ged, high_school_diploma))
## No High School Education 
covid_cases_newest <- covid_cases_newest %>% 
  mutate(`Has Not Graduated High School` = (`less_than_high_school_graduate`))
covid_cases_newest <- covid_cases_newest %>% 
  select(-c(less_than_high_school_graduate))
###### Grouping Commute Types ######
## Commute by Public Transit Types(bus, train)
covid_cases_newest <- covid_cases_newest %>% 
  mutate(`Commute Less than 10 minutes` = (`commuters_by_bus` + `commuters_by_subway_or_elevated`))
covid_cases_newest <- covid_cases_newest %>% 
  select(-c(`commuters_by_bus`, `commuters_by_subway_or_elevated`))
## Commute by Personal Vehicle(car, truck, van)
covid_cases_newest <- covid_cases_newest %>% 
  mutate(`Commute Less than 10 minutes` = (`commuters_drove_alone` + `commuters_by_car_truck_van`))
covid_cases_newest <- covid_cases_newest %>% 
  select(-c(`commuters_drove_alone`, `commuters_by_car_truck_van`))
###### Grouping Employment Types ######
## "White Collar" Jobs
covid_cases_newest <- covid_cases_newest %>% 
  mutate(`White Collar Workers` = (`employed_education_health_social` + 
                                     `employed_finance_insurance_real_estate` +
                                     `employed_information` +
                                     `employed_other_services_not_public_admin` + 
                                     `employed_public_administration` + 
                                     `employed_science_management_admin_waste`))
covid_cases_newest <- covid_cases_newest %>% 
  select(-c(employed_education_health_social, 
            employed_finance_insurance_real_estate,
            employed_information,
            employed_other_services_not_public_admin,
            employed_public_administration,
            employed_science_management_admin_waste))
## "Blue Collar" Jobs
covid_cases_newest <- covid_cases_newest %>% 
  mutate(`Blue Collar Workers` = (`employed_agriculture_forestry_fishing_hunting_mining` + 
                                    `employed_arts_entertainment_recreation_accommodation_food` +
                                    `employed_construction` +
                                    `employed_manufacturing` + 
                                    `employed_transportation_warehousing_utilities` + 
                                    `employed_retail_trade` +
                                    `employed_wholesale_trade`))
covid_cases_newest <- covid_cases_newest %>% 
  select(-c(employed_agriculture_forestry_fishing_hunting_mining, 
            employed_arts_entertainment_recreation_accommodation_food,
            employed_construction,
            employed_manufacturing,
            employed_transportation_warehousing_utilities,
            employed_retail_trade,
            employed_wholesale_trade))
###### Grouping Commute Times ######
## Commute Less than 10 minutes
covid_cases_newest <- covid_cases_newest %>% 
  mutate(`Commute Less than 10 minutes` = (`commute_5_9_mins` + `commute_less_10_mins`))
covid_cases_newest <- covid_cases_newest %>% 
  select(-c(`commute_5_9_mins`, `commute_less_10_mins`))
## Commute Less than 60 mins
covid_cases_newest <- covid_cases_newest %>% 
  mutate(`Commute Less than 60 minutes` = (`commute_10_14_mins` + 
                                             `commute_15_19_mins` + 
                                             `commute_20_24_mins` + 
                                             `commute_25_29_mins` + 
                                             `commute_30_34_mins` + 
                                             `commute_35_44_mins` + 
                                             `commute_40_44_mins` + 
                                             `commute_45_59_mins`))
covid_cases_newest <- covid_cases_newest %>% 
  select(-c(`commute_10_14_mins`,
            `commute_15_19_mins`, 
            `commute_20_24_mins`, 
            `commute_25_29_mins`, 
            `commute_30_34_mins`, 
            `commute_35_39_mins`,
            `commute_35_44_mins`, 
            `commute_40_44_mins`,
            `commute_45_59_mins`))
## Commute More than 60 mins
covid_cases_newest <- covid_cases_newest %>% 
  mutate(`Commute More than 60 minutes` = (`commute_60_89_mins` + 
                                             `commute_90_more_mins`))
covid_cases_newest <- covid_cases_newest %>% 
  select(-c(`commute_60_more_mins`, `commute_60_89_mins`,
            `commute_90_more_mins`))
###### Grouping Ages Together ######
## Male
### 0-17
covid_cases_newest <- covid_cases_newest %>% 
  mutate(`Male Ages 0-17` = (`male_under_5` + `male_5_to_9` + `male_10_to_14` + `male_15_to_17`))
covid_cases_newest <- covid_cases_newest %>% 
  select(-c(`male_under_5`, `male_5_to_9`, `male_10_to_14`, `male_15_to_17`))
### 18-64
covid_cases_newest <- covid_cases_newest %>% 
  mutate(`Male Ages 18-64` = (`male_18_to_19` + `male_20` + `male_21` + 
                                `male_22_to_24` + `male_25_to_29` +
                                `male_30_to_34` + `male_35_to_39` +
                                `male_40_to_44` + `male_45_to_49` +
                                `male_50_to_54` + `male_55_to_59` +
                                `male_60_61` + `male_62_64`))
covid_cases_newest <- covid_cases_newest %>% 
  select(-c(`male_18_to_19`, `male_20`, `male_21`, 
            `male_22_to_24`, `male_25_to_29`,
            `male_30_to_34`, `male_35_to_39`,
            `male_40_to_44`, `male_45_to_49`,
            `male_50_to_54`, `male_55_to_59`,
            `male_60_61`, `male_62_64`))
### 65-85+
covid_cases_newest <- covid_cases_newest %>% 
  mutate(`Male Ages 65-85+` = (`male_65_to_66` + `male_67_to_69` + 
                                 `male_70_to_74` + `male_70_to_74` +
                                 `male_75_to_79` + `male_80_to_84` + 
                                 `male_85_and_over`))
covid_cases_newest <- covid_cases_newest %>% 
  select(-c(`male_65_to_66`, `male_67_to_69`, `male_70_to_74`, `male_70_to_74`, 
            `male_75_to_79`, `male_80_to_84`, `male_85_and_over`))
## Female
### 0-17
covid_cases_newest <- covid_cases_newest %>% 
  mutate(`Female Ages 0-17` = (`female_under_5` + `female_5_to_9` + 
                                 `female_10_to_14` + `female_15_to_17`))
covid_cases_newest <- covid_cases_newest %>% 
  select(-c(`female_under_5`, `female_5_to_9`, `female_10_to_14`, `female_15_to_17`))
### 18-64
covid_cases_newest <- covid_cases_newest %>% 
  mutate(`Female Ages 18-64` = (`female_18_to_19` + `female_20` + `female_21` + 
                                  `female_22_to_24` + `female_25_to_29` +
                                  `female_30_to_34` + `female_35_to_39` +
                                  `female_40_to_44` + `female_45_to_49` +
                                  `female_50_to_54` + `female_55_to_59` +
                                  `female_60_to_61` + `female_62_to_64`))
covid_cases_newest <- covid_cases_newest %>% 
  select(-c(`female_18_to_19`, `female_20`, `female_21`, 
            `female_22_to_24`, `female_25_to_29`,
            `female_30_to_34`, `female_35_to_39`,
            `female_40_to_44`, `female_45_to_49`,
            `female_50_to_54`, `female_55_to_59`,
            `female_60_to_61`, `female_62_to_64`))
### 65-85+
covid_cases_newest <- covid_cases_newest %>% 
  mutate(`Female Ages 65-85+` = (`female_65_to_66` + `female_67_to_69` + 
                                   `female_70_to_74` + `female_70_to_74` +
                                   `female_75_to_79` + `female_80_to_84` + 
                                   `female_85_and_over`))
covid_cases_newest <- covid_cases_newest %>% 
  select(-c(`female_65_to_66`, `female_67_to_69`, `female_70_to_74`, `female_70_to_74`, 
            `female_75_to_79`, `female_80_to_84`, `female_85_and_over`))

###### Normalize our features by population ######
covid_cases_newest <- covid_cases_newest %>% 
  mutate(`Non-family Households` = `Non-family Households` / `Population`,
         `Family Households` = `Family Households` / `Population`,
         `Male Population` = `Male Population` / `Population`,
         `Female Population` = `Female Population` / `Population`,
         `White` = `White` / `Population`,
         `Black` = `Black` / `Population`,
         `Asian` = `Asian` / `Population`,
         `Hispanic` = `Hispanic` / `Population`,
         `American Indian` = `American Indian` / `Population`,
         `Other` = `Other` / `Population`,
         `Two or More Races` = `Two or More Races` / `Population`,
         `Not Hispanic` = `Not Hispanic` / `Population`,
         `Commuters by Public Transit` = `Commuters by Public Transit` / `Population`,
         `Number of Households` = `Number of Households` / `Population`,
         `Vacant Housing Units` = `Vacant Housing Units` / `Population`,
         `Vacant Housing Units For Rent` = `Vacant Housing Units For Rent` / `Population`,
         `Vacant Housing Units For Sale` = `Vacant Housing Units For Sale` / `Population`,
         `Million Dollar Houses` = `Million Dollar Houses` / `Population`,
         `Housing Units` = `Housing Units` / `Population`,
         `Owner Occupied Housing Units` = `Owner Occupied Housing Units` / `Population`,
         `Income Less Than $10,000` = `Income Less Than $10,000` / `Population`,
         `Income Between $10,000 and $14,999` = `Income Between $10,000 and $14,999` / `Population`,
         `Income Between $15,000 and $19,999` = `Income Between $15,000 and $19,999` / `Population`,
         `Income Between $20,000 and $24,999` = `Income Between $20,000 and $24,999` / `Population`,
         `Income Between $25,000 and $29,999` = `Income Between $25,000 and $29,999` / `Population`,
         `Income Between $30,000 and $34,999` = `Income Between $30,000 and $34,999` / `Population`,
         `Income Between $35,000 and $39,999` = `Income Between $35,000 and $39,999` / `Population`,
         `Income Between $40,000 and $44,999` = `Income Between $40,000 and $44,999` / `Population`,
         `Income Between $45,000 and $49,999` = `Income Between $45,000 and $49,999` / `Population`,
         `Income Between $50,000 and $59,999` = `Income Between $50,000 and $59,999` / `Population`,
         `Income Between $60,000 and $74,999` = `Income Between $60,000 and $74,999` / `Population`,
         `Income Between $75,000 and $99,999` = `Income Between $75,000 and $99,999` / `Population`,
         `Income Between $100,000 and $124,999` = `Income Between $100,000 and $124,999` / `Population`,
         `Income Between $125,000 and $149,999` = `Income Between $125,000 and $149,999` / `Population`,
         `Income Between $150,000 and $199,999` = `Income Between $150,000 and $199,999` / `Population`,
         `Income Between $200,000+` = `Income Between $200,000+` / `Population`,
         `Employed` = `Employed` / `Population`,
         `Unemployed` = `Unemployed` / `Population`,
         `Population Not In Labor Force` = `Population Not In Labor Force` / `Population`,
         `Population In Labor Force` = `Population In Labor Force` / `Population`,
         `Has Four+ Cars` = `Has Four+ Cars` / `Population`,
         `Has No Car(s)` = `Has No Car(s)` / `Population`,
         `Has Car` = `Has Car` / `Population`,
         `Has Two Cars` = `Has Two Cars` / `Population`,
         `Has Three Cars` = `Has Three Cars` / `Population`,
         `Non-US Citizen` = `Non-US Citizen` / `Population`,
         `Population Determined Poverty Status` = `Population Determined Poverty Status` / `Population`,
         `Poverty` = `Poverty` / `Population`,
         `Work From Home` = `Work From Home` / `Population`,
         `Commuters 16+` = `Commuters 16+` / `Population`,
         `Has Graduated Higher Education` = `Has Graduated Higher Education` / `Population`,
         `Has Graduated High School` = `Has Graduated High School` / `Population`,
         `Has Not Graduated High School` = `Has Not Graduated High School` / `Population`,
         `White Collar Workers` = `White Collar Workers` / `Population`,
         `Blue Collar Workers` = `Blue Collar Workers` / `Population`,
         `Commute Less than 60 minutes` = `Commute Less than 60 minutes` / `Population`,
         `Commute More than 60 minutes` = `Commute More than 60 minutes` / `Population`,
         `Commute Less than 10 minutes` = `Commute Less than 10 minutes` / `Population`,
         `Male Ages 0-17` = `Male Ages 0-17` / `Population`,
         `Male Ages 18-64` = `Male Ages 18-64` / `Population`,
         `Male Ages 65-85+` = `Male Ages 65-85+` / `Population`,
         `Female Ages 0-17` = `Female Ages 0-17` / `Population`,
         `Female Ages 18-64` = `Female Ages 18-64` / `Population`,
         `Female Ages 65-85+` = `Female Ages 65-85+` / `Population`,
         `Median Age` = `Median Age` / `Population`,
         `Median Income` = `Median Income` / `Population`,
         `Percent Income Spent on Rent` = `Percent Income Spent on Rent` / `Population`,
         `Households Using Public Assistance or Food Stamps` = `Households Using Public Assistance or Food Stamps` / `Population`
         )
###### Removing features ######
covid_cases_newest <- covid_cases_newest %>% select(-median_year_structure_built)
covid_cases_newest <- covid_cases_newest %>% select(-rent_burden_not_computed)
covid_cases_newest <- covid_cases_newest %>% select(-renter_occupied_housing_units_paying_cash_median_gross_rent)
covid_cases_newest <- covid_cases_newest %>% select(-rent_under_10_percent)
covid_cases_newest <- covid_cases_newest %>% select(-rent_10_to_15_percent)
covid_cases_newest <- covid_cases_newest %>% select(-rent_15_to_20_percent)
covid_cases_newest <- covid_cases_newest %>% select(-rent_20_to_25_percent)
covid_cases_newest <- covid_cases_newest %>% select(-rent_25_to_30_percent)
covid_cases_newest <- covid_cases_newest %>% select(-rent_30_to_35_percent)
covid_cases_newest <- covid_cases_newest %>% select(-rent_35_to_40_percent)
covid_cases_newest <- covid_cases_newest %>% select(-rent_40_to_50_percent)
covid_cases_newest <- covid_cases_newest %>% select(-rent_over_50_percent)
covid_cases_newest <- covid_cases_newest %>% select(-pop_divorced)
covid_cases_newest <- covid_cases_newest %>% select(-pop_widowed)
covid_cases_newest <- covid_cases_newest %>% select(-pop_separated)
covid_cases_newest <- covid_cases_newest %>% select(-do_date)
covid_cases_newest <- covid_cases_newest %>% select(-no_cars)
covid_cases_newest <- covid_cases_newest %>% select(-pop_now_married)
covid_cases_newest <- covid_cases_newest %>% select(-pop_never_married)
covid_cases_newest <- covid_cases_newest %>% select(-pop_15_and_over)
covid_cases_newest <- covid_cases_newest %>% select(-pop_16_over)
covid_cases_newest <- covid_cases_newest %>% select(-pop_5_years_over)
covid_cases_newest <- covid_cases_newest %>% select(-pop_25_64)
covid_cases_newest <- covid_cases_newest %>% select(-pop_25_years_over)
covid_cases_newest <- covid_cases_newest %>% select(-population_1_year_and_over)
covid_cases_newest <- covid_cases_newest %>% select(-population_3_years_over)
covid_cases_newest <- covid_cases_newest %>% select(-speak_only_english_at_home)
covid_cases_newest <- covid_cases_newest %>% select(-speak_spanish_at_home_low_english)
covid_cases_newest <- covid_cases_newest %>% select(-speak_spanish_at_home)
covid_cases_newest <- covid_cases_newest %>% select(-hispanic_any_race)
covid_cases_newest <- covid_cases_newest %>% select(-workers_16_and_over)
covid_cases_newest <- covid_cases_newest %>% select(-walked_to_work)
covid_cases_newest <- covid_cases_newest %>% select(-male_male_households)
covid_cases_newest <- covid_cases_newest %>% select(-male_45_64_associates_degree)
covid_cases_newest <- covid_cases_newest %>% select(-male_45_64_bachelors_degree)
covid_cases_newest <- covid_cases_newest %>% select(-male_45_64_graduate_degree)
covid_cases_newest <- covid_cases_newest %>% select(-female_female_households)
covid_cases_newest <- covid_cases_newest %>% select(-male_45_to_64)
covid_cases_newest <- covid_cases_newest %>% select(-mortgaged_housing_units)
covid_cases_newest <- covid_cases_newest %>% select(-aggregate_travel_time_to_work)
covid_cases_newest <- covid_cases_newest %>% select(-families_with_young_children)
covid_cases_newest <- covid_cases_newest %>% select(-two_parent_families_with_young_children)
covid_cases_newest <- covid_cases_newest %>% select(-two_parents_in_labor_force_families_with_young_children)
covid_cases_newest <- covid_cases_newest %>% select(-two_parents_father_in_labor_force_families_with_young_children)
covid_cases_newest <- covid_cases_newest %>% select(-two_parents_mother_in_labor_force_families_with_young_children)
covid_cases_newest <- covid_cases_newest %>% select(-two_parents_not_in_labor_force_families_with_young_children)
covid_cases_newest <- covid_cases_newest %>% select(-one_parent_families_with_young_children)
covid_cases_newest <- covid_cases_newest %>% select(-father_one_parent_families_with_young_children)
covid_cases_newest <- covid_cases_newest %>% select(-father_in_labor_force_one_parent_families_with_young_children)
covid_cases_newest <- covid_cases_newest %>% select(-owner_occupied_housing_units_lower_value_quartile)
covid_cases_newest <- covid_cases_newest %>% select(-owner_occupied_housing_units_median_value)
covid_cases_newest <- covid_cases_newest %>% select(-owner_occupied_housing_units_upper_value_quartile)
covid_cases_newest <- covid_cases_newest %>% select(-married_households)
covid_cases_newest <- covid_cases_newest %>% select(-occupied_housing_units)
covid_cases_newest <- covid_cases_newest %>% select(-housing_units_renter_occupied)
covid_cases_newest <- covid_cases_newest %>% select(-dwellings_1_units_detached)
covid_cases_newest <- covid_cases_newest %>% select(-dwellings_1_units_attached)
covid_cases_newest <- covid_cases_newest %>% select(-dwellings_2_units)
covid_cases_newest <- covid_cases_newest %>% select(-dwellings_3_to_4_units)
covid_cases_newest <- covid_cases_newest %>% select(-dwellings_5_to_9_units)
covid_cases_newest <- covid_cases_newest %>% select(-dwellings_10_to_19_units)
covid_cases_newest <- covid_cases_newest %>% select(-dwellings_20_to_49_units)
covid_cases_newest <- covid_cases_newest %>% select(-dwellings_50_or_more_units)
covid_cases_newest <- covid_cases_newest %>% select(-mobile_homes)
covid_cases_newest <- covid_cases_newest %>% select(-housing_built_2005_or_later)
covid_cases_newest <- covid_cases_newest %>% select(-housing_built_2000_to_2004)
covid_cases_newest <- covid_cases_newest %>% select(-housing_built_1939_or_earlier)
covid_cases_newest <- covid_cases_newest %>% select(-white_including_hispanic)
covid_cases_newest <- covid_cases_newest %>% select(-black_including_hispanic)
covid_cases_newest <- covid_cases_newest %>% select(-asian_including_hispanic)
covid_cases_newest <- covid_cases_newest %>% select(-amerindian_including_hispanic)
covid_cases_newest <- covid_cases_newest %>% select(-commuters_by_carpool)
covid_cases_newest <- covid_cases_newest %>% select(-households_retirement_income)
covid_cases_newest <- covid_cases_newest %>% select(-armed_forces)
covid_cases_newest <- covid_cases_newest %>% select(-civilian_labor_force)
covid_cases_newest <- covid_cases_newest %>% select(-asian_male_45_54)
covid_cases_newest <- covid_cases_newest %>% select(-asian_male_55_64)
covid_cases_newest <- covid_cases_newest %>% select(-black_male_45_54)
covid_cases_newest <- covid_cases_newest %>% select(-black_male_55_64)
covid_cases_newest <- covid_cases_newest %>% select(-hispanic_male_45_54)
covid_cases_newest <- covid_cases_newest %>% select(-hispanic_male_55_64)
covid_cases_newest <- covid_cases_newest %>% select(-white_male_45_54)
covid_cases_newest <- covid_cases_newest %>% select(-white_male_55_64)
covid_cases_newest <- covid_cases_newest %>% select(-bachelors_degree_or_higher_25_64)
covid_cases_newest <- covid_cases_newest %>% select(-children_in_single_female_hh)
covid_cases_newest <- covid_cases_newest %>% select(-children)
covid_cases_newest <- covid_cases_newest %>% select(-different_house_year_ago_different_city)
covid_cases_newest <- covid_cases_newest %>% select(-different_house_year_ago_same_city)
covid_cases_newest <- covid_cases_newest %>% select(-male_45_64_less_than_9_grade)
covid_cases_newest <- covid_cases_newest %>% select(-male_45_64_grade_9_12)
covid_cases_newest <- covid_cases_newest %>% select(-male_45_64_high_school)
covid_cases_newest <- covid_cases_newest %>% select(-male_45_64_some_college)
covid_cases_newest <- covid_cases_newest %>% select(-in_grades_1_to_4)
covid_cases_newest <- covid_cases_newest %>% select(-in_grades_5_to_8)
covid_cases_newest <- covid_cases_newest %>% select(-in_grades_9_to_12)
covid_cases_newest <- covid_cases_newest %>% select(-in_school)
covid_cases_newest <- covid_cases_newest %>% select(-group_quarters)
covid_cases_newest <- covid_cases_newest %>% select(-management_business_sci_arts_employed)
covid_cases_newest <- covid_cases_newest %>% select(-bachelors_degree_2)
covid_cases_newest <- covid_cases_newest %>% select(-occupation_management_arts)
covid_cases_newest <- covid_cases_newest %>% select(-occupation_natural_resources_construction_maintenance)
covid_cases_newest <- covid_cases_newest %>% select(-occupation_production_transportation_material)
covid_cases_newest <- covid_cases_newest %>% select(-occupation_sales_office)
covid_cases_newest <- covid_cases_newest %>% select(-occupation_services)
covid_cases_newest <- covid_cases_newest %>% select(-sales_office_employed)
covid_cases_newest <- covid_cases_newest %>% select(-one_year_more_college)
covid_cases_newest <- covid_cases_newest %>% select(-in_undergrad_college)
covid_cases_newest <- covid_cases_newest %>% select(-less_one_year_college)
covid_cases_newest <- covid_cases_newest %>% select(-some_college_and_associates_degree)
covid_cases_newest <- covid_cases_newest %>% select(-`Confirmed`)
covid_cases_newest <- covid_cases_newest %>% select(-`Deaths`)
covid_cases_newest <- covid_cases_newest %>% select(-`Population`)
covid_cases_newest <- covid_cases_newest %>% select(-`Income Per Capita`)
covid_cases_newest <- covid_cases_newest %>% select(-`Households Using Public Assistance or Food Stamps`)
covid_cases_newest <- covid_cases_newest %>% select(-`Median Rent`)

###### More Cleaning ######
summary(covid_cases_newest)
table(complete.cases(covid_cases_newest))
str(covid_cases_newest)
###### Check Correlation ######
cm <- cor(covid_cases_newest %>% select_if(is.numeric) %>% na.omit)
## We can adjust the margins to fit whatever it is we want to observe ##
hmap(cm, margins = c(10, 10))
## In case there is an error regarding graphics##
dev.off()

###### Creating Qualifier Class Variables ######
###### Creating a "bad" factor which represents high death:case rate ######
covid_cases_newest <- covid_cases_newest %>% mutate(bad_death_case_count = as.factor(death_per_case > 142))
## Determine a more-or-less even split to avoid class imbalance
covid_cases_newest %>% pull(bad_death_case_count) %>% table()
## We determine further States with interesting percentages of "bad" ##
covid_cases_newest_US_bad_death_cases <- covid_cases_newest %>% group_by(State) %>% 
  summarize(bad_predicted_death_case_count_pct = sum(bad_death_case_count == TRUE)/n()) %>%
  arrange(desc(bad_predicted_death_case_count_pct))
###### Split our data into test/training sets: Death:Case Rate ######
## Training Set ##
covid_cases_newest %>% filter(State %in% c("TX", "CA", "FL", "NY"))
covid_cases_newest_training <- covid_cases_newest %>% filter(State %in% c("TX", "CA", "FL", "NY"))
covid_cases_newest_training %>% pull(bad_death_case_count) %>% table()
## Test Set ##
covid_cases_newest_test <- covid_cases_newest %>% filter(!(State %in% c("TX", "CA", "FL", "NY")))
covid_cases_newest_test %>% pull(bad_death_case_count) %>% table()
###### Plot map for training data ######
counties <- as_tibble(map_data("county"))
counties <- counties %>% 
  rename(c(county = subregion, state = region)) %>%
  mutate(state = state.abb[match(state, tolower(state.name))]) %>%
  select(state, county, long, lat, group)
counties  

counties_all <- counties %>% left_join(covid_cases_newest_training %>% 
                                         mutate(county = County %>% str_to_lower() %>% 
                                                  str_replace('\\s+county\\s*$', '')))
## Not sure why this differs from starter code ##
ggplot(counties_all, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = bad_death_case_count), color = "black", size = 0.1) + 
  coord_quickmap() + scale_fill_manual(values = c('TRUE' = 'red', 'FALSE' = 'grey'))
###### Checking variable importance: Bad Death:Case Rate ######
covid_cases_newest_training %>%  chi.squared(bad_death_case_count ~ ., data = .) %>% 
  arrange(desc(attr_importance)) %>% head()
## Need to remove variable used to create class variable 
## also remove other variables
covid_cases_newest_training <- covid_cases_newest_training %>% select(-c(deaths_per_10000))
covid_cases_newest_training <- covid_cases_newest_training %>% select(-c(`Geographic ID`))
covid_cases_newest_training <- covid_cases_newest_training %>% select(-death_per_case, -cases_per_10000)

covid_cases_newest_training %>%  chi.squared(bad_death_case_count ~ ., data = .) %>% 
  arrange(desc(attr_importance)) %>% head(10)
###### Building a model: Bad Death:Case Rate ######
## Using Random Forest Method ##
bad_death_case_fit <- covid_cases_newest_training %>%
  train(bad_death_case_count ~ . - County - State,
        data = .,
        #method = "rpart",
        method = "rf",
        #method = "svmLinear"
        #method = "nb",
        tuneLength = 5,
        trControl = trainControl(method = "cv", number = 15)
      )
bad_death_case_fit
## Show the most important vars ##
### NOTE: Will need to go back and get data in terms of per day ###
varImp(bad_death_case_fit)
varImp(bad_death_case_fit, compete = FALSE)
bad_death_case_fit$finalModel
###### Apply model to other states in US: Bad Death:Case Rate ######
covid_cases_newest_test <- covid_cases_newest_test %>% na.omit
covid_cases_newest_test$bad_predicted_death_case_count_pct <- predict(bad_death_case_fit, covid_cases_newest_test)

counties_test <- counties %>% left_join(covid_cases_newest_test %>% 
                                          mutate(county = County %>% str_to_lower() %>% 
                                                   str_replace('\\s+county\\s*$', '')))
## Ground Truth ##
ggplot(counties_test, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = bad_death_case_count), color = "black", size = 0.1) + 
  coord_quickmap() + 
  scale_fill_manual(values = c('TRUE' = 'red', 'FALSE' = 'grey'))
## Predictions by plotting with our test data ##
ggplot(counties_test, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = bad_predicted_death_case_count_pct), color = "black", size = 0.1) + 
  coord_quickmap() + 
  scale_fill_manual(values = c('TRUE' = 'red', 'FALSE' = 'grey'))
## Confusion Matrix ##
confusionMatrix(data = covid_cases_newest_test$bad_predicted_death_case_count_pct, 
                ref = covid_cases_newest_test$bad_death_case_count)
###### ######
###### Creating a "bad" factor which represents high case rate ######
covid_cases_newest <- covid_cases_newest %>% mutate(bad_case_count = as.factor(cases_per_10000 > 2400))
## Determine a more-or-less even split to avoid class imbalance
covid_cases_newest %>% pull(bad_case_count) %>% table()
## We determine further States with interesting percentages of "bad" ##
covid_cases_newest_US_bad_cases <- covid_cases_newest %>% group_by(State) %>% 
  summarize(bad_predicted_case_count_pct = sum(bad_case_count == TRUE)/n()) %>%
  arrange(desc(bad_predicted_case_count_pct))
###### Split our data into test/training sets: Bad Case Count ######
## Training Set ##
covid_cases_newest %>% filter(State %in% c("TX", "CA", "FL", "NY"))
covid_cases_newest_training <- covid_cases_newest %>% filter(State %in% c("TX", "CA", "FL", "NY"))
covid_cases_newest_training %>% pull(bad_case_count) %>% table()
## Test Set ##
covid_cases_newest_test <- covid_cases_newest %>% filter(!(State %in% c("TX", "CA", "FL", "NY")))
covid_cases_newest_test %>% pull(bad_case_count) %>% table()

###### Plot map for training data ######
counties <- as_tibble(map_data("county"))
counties <- counties %>% 
  rename(c(county = subregion, state = region)) %>%
  mutate(state = state.abb[match(state, tolower(state.name))]) %>%
  select(state, county, long, lat, group)
counties  

counties_all <- counties %>% left_join(covid_cases_newest_training %>% 
                                         mutate(county = County %>% str_to_lower() %>% 
                                                  str_replace('\\s+county\\s*$', '')))
## Not sure why this differs from starter code ##
ggplot(counties_all, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = bad_case_count), color = "black", size = 0.1) + 
  coord_quickmap() + scale_fill_manual(values = c('TRUE' = 'red', 'FALSE' = 'grey'))

###### Checking variable importance: Bad Case Count ######
covid_cases_newest_training %>%  chi.squared(bad_case_count ~ ., data = .) %>% 
  arrange(desc(attr_importance)) %>% head()
## Need to remove variable used to create class variable 
## also remove other variables
covid_cases_newest_training <- covid_cases_newest_training %>% select(-c(deaths_per_10000))
covid_cases_newest_training <- covid_cases_newest_training %>% select(-c(`Geographic ID`))
covid_cases_newest_training <- covid_cases_newest_training %>% select(-death_per_case, -cases_per_10000)
covid_cases_newest_training %>%  chi.squared(bad_case_count ~ ., data = .) %>% 
  arrange(desc(attr_importance)) %>% head(10)
###### Building a model: Bad Case Count ######
## Using Random Forest Method ##
bad_case_fit <- covid_cases_newest_training %>%
  train(bad_case_count ~ . - County - State,
        data = . ,
        #method = "rpart",
        method = "rf",
        #method = "nb",
        trControl = trainControl(method = "cv", number = 10)
  )
bad_case_fit

## Show the most important vars ##
### NOTE: Will need to go back and get data in terms of per day ###
varImp(bad_case_fit)
###### Apply model to other states in US: Bad Case Count ######
covid_cases_newest_test <- covid_cases_newest_test %>% na.omit
covid_cases_newest_test$bad_predicted_case_count_pct <- predict(bad_case_fit, covid_cases_newest_test)

counties_test <- counties %>% left_join(covid_cases_newest_test %>% 
                                          mutate(county = County %>% str_to_lower() %>% 
                                                   str_replace('\\s+county\\s*$', '')))
## Ground Truth ##
ggplot(counties_test, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = bad_case_count), color = "black", size = 0.1) + 
  coord_quickmap() + 
  scale_fill_manual(values = c('TRUE' = 'red', 'FALSE' = 'grey'))
## Predictions by plotting with our test data ##
ggplot(counties_test, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = bad_predicted_case_count_pct), color = "black", size = 0.1) + 
  coord_quickmap() + 
  scale_fill_manual(values = c('TRUE' = 'red', 'FALSE' = 'grey'))
## Confusion Matrix ##
confusionMatrix(data = covid_cases_newest_test$bad_predicted_case_count_pct, ref = covid_cases_newest_test$bad_case_count)
###### ######
###### Creating a "bad" factor which represents high fatality rate #####
## Note: The "bad" value was chosen through trial and error
### Note: 35 is a pretty large value considering our column - keep in mind
### by this date (2022-04-09) in the pandemic, countless lives are lost already
covid_cases_newest <- covid_cases_newest %>% mutate(bad_death_count = as.factor(deaths_per_10000 > 35))
## Determine a more-or-less even split to avoid class imbalance
covid_cases_newest %>% pull(bad_death_count) %>% table()
## We determine further States with interesting percentages of "bad" ##
###### If we can perhaps create a gradient map of this, we can visualize the US's highest fatality centers ######
covid_cases_newest_US_select <- covid_cases_newest %>% group_by(State) %>% 
  summarize(bad_predicted_death_count_pct = sum(bad_death_count == TRUE)/n()) %>%
  arrange(desc(bad_predicted_death_count_pct))

###### Split our data into test/training sets ######
## Training Set ##
covid_cases_newest %>% filter(State %in% c("TX", "CA", "FL", "NY"))
covid_cases_newest_training <- covid_cases_newest %>% filter(State %in% c("TX", "CA", "FL", "NY"))
covid_cases_newest_training %>% pull(bad_death_count) %>% table()
## Test Set ##
covid_cases_newest_test <- covid_cases_newest %>% filter(!(State %in% c("TX", "CA", "FL", "NY")))
covid_cases_newest_test %>% pull(bad_death_count) %>% table()

###### Plot map for training data ######
counties <- as_tibble(map_data("county"))
counties <- counties %>% 
  rename(c(county = subregion, state = region)) %>%
  mutate(state = state.abb[match(state, tolower(state.name))]) %>%
  select(state, county, long, lat, group)
counties  

counties_all <- counties %>% left_join(covid_cases_newest_training %>% 
                        mutate(county = County %>% str_to_lower() %>% 
                                str_replace('\\s+county\\s*$', '')))
## Not sure why this differs from starter code ##
ggplot(counties_all, aes(long, lat)) + 
    geom_polygon(aes(group = group, fill = bad_death_count), color = "black", size = 0.1) + 
    coord_quickmap() + scale_fill_manual(values = c('TRUE' = 'red', 'FALSE' = 'grey'))

###### Checking variable importance: Bad Fatality Rate ######
covid_cases_newest_training %>%  chi.squared(bad_death_count ~ ., data = .) %>% 
  arrange(desc(attr_importance)) %>% head()
## Need to remove variable used to create class variable 
## also remove other variables
covid_cases_newest_training <- covid_cases_newest_training %>% select(-c(deaths_per_10000))
covid_cases_newest_training <- covid_cases_newest_training %>% select(-c(`Geographic ID`))
covid_cases_newest_training <- covid_cases_newest_training %>% select(-death_per_case, -cases_per_10000)
covid_cases_newest_training %>%  chi.squared(bad_death_count ~ ., data = .) %>% 
  arrange(desc(attr_importance)) %>% head(10)
###### Building a model: Bad Fatality Rate ######
## Using Random Forest Method ##
fit <- covid_cases_newest_training %>%
  train(bad_death_count ~ . - County - State,
        data = . ,
        #method = "rpart",
        method = "rf",
        #method = "nb",
        trControl = trainControl(method = "cv", number = 10)
  )
fit
## Show the most important vars ##
### NOTE: Will need to go back and get data in terms of per day ###
varImp(fit)


###### Apply model to other states in US: Bad Fatality Rate ######
covid_cases_newest_test <- covid_cases_newest_test %>% na.omit
covid_cases_newest_test$bad_predicted_death_count <- predict(fit, covid_cases_newest_test)

counties_test <- counties %>% left_join(covid_cases_newest_test %>% 
                                          mutate(county = County %>% str_to_lower() %>% 
                                                   str_replace('\\s+county\\s*$', '')))
## Ground Truth ##
ggplot(counties_test, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = bad), color = "black", size = 0.1) + 
  coord_quickmap() + 
  scale_fill_manual(values = c('TRUE' = 'red', 'FALSE' = 'grey'))
## Predictions by plotting with our test data ##
ggplot(counties_test, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = bad_predicted_death_count), color = "black", size = 0.1) + 
  coord_quickmap() + 
  scale_fill_manual(values = c('TRUE' = 'red', 'FALSE' = 'grey'))
## Confusion Matrix ##
confusionMatrix(data = covid_cases_newest_test$bad_predicted_death_count, ref = covid_cases_newest_test$bad)





###### Model Evaluation ######
###### Model Comparison ######