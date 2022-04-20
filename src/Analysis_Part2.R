# Libraries
library(dplyr)
library(tibble)
install.packages("tidyverse")
library(tidyverse)
library(cluster)
library(dbscan)
library(GGally)
library(readr)
library(factoextra)
library(stringr)

###### Data tibbles ######
covid_census_tx_ca_ny_expanded <- as_tibble(read_csv("C:/data-mining-COVID-Project/data/covid_tx_ca_ny_expaned_census.csv"))

###### Light cleaning  #######
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% mutate_if(is.character, factor)
#summary(covid_census_tx_ca_ny_expanded)
###### Renaming Columns ######
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-`county_fips_code`)
covid_census_tx_ca_ny_expanded <- rename(covid_census_tx_ca_ny_expanded, County = `county_name`)
covid_census_tx_ca_ny_expanded <- rename(covid_census_tx_ca_ny_expanded, State = `state`)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-state_fips_code)
covid_census_tx_ca_ny_expanded <- rename(covid_census_tx_ca_ny_expanded, Date = `date`)
covid_census_tx_ca_ny_expanded <- rename(covid_census_tx_ca_ny_expanded, Confirmed = `confirmed_cases`)
covid_census_tx_ca_ny_expanded <- rename(covid_census_tx_ca_ny_expanded, Deaths = `deaths`)
covid_census_tx_ca_ny_expanded <- rename(covid_census_tx_ca_ny_expanded, `Geographic ID` = `geo_id`)
covid_census_tx_ca_ny_expanded <- rename(covid_census_tx_ca_ny_expanded, Population = `total_pop`)
covid_census_tx_ca_ny_expanded <- rename(covid_census_tx_ca_ny_expanded, `Median Age` = `median_age`)
covid_census_tx_ca_ny_expanded <- rename(covid_census_tx_ca_ny_expanded, `Number of Households` = `households`)
covid_census_tx_ca_ny_expanded <- rename(covid_census_tx_ca_ny_expanded, `Median Income` = `median_income`)
covid_census_tx_ca_ny_expanded <- rename(covid_census_tx_ca_ny_expanded, `Income Per Capita` = `income_per_capita`)
covid_census_tx_ca_ny_expanded <- rename(covid_census_tx_ca_ny_expanded, Employed = `employed_pop`)
covid_census_tx_ca_ny_expanded <- rename(covid_census_tx_ca_ny_expanded, Unemployed = `unemployed_pop`)
covid_census_tx_ca_ny_expanded <- rename(covid_census_tx_ca_ny_expanded, `Male Population` = `male_pop`)
covid_census_tx_ca_ny_expanded <- rename(covid_census_tx_ca_ny_expanded, `Female Population` = `female_pop`)
covid_census_tx_ca_ny_expanded <- rename(covid_census_tx_ca_ny_expanded, `Family Households` = `family_households`)
covid_census_tx_ca_ny_expanded <- rename(covid_census_tx_ca_ny_expanded, `Non-family Households` = `nonfamily_households`)
covid_census_tx_ca_ny_expanded <- rename(covid_census_tx_ca_ny_expanded, White = `white_pop`)
covid_census_tx_ca_ny_expanded <- rename(covid_census_tx_ca_ny_expanded, Black = `black_pop`)
covid_census_tx_ca_ny_expanded <- rename(covid_census_tx_ca_ny_expanded, Asian = `asian_pop`)
covid_census_tx_ca_ny_expanded <- rename(covid_census_tx_ca_ny_expanded, Hispanic = `hispanic_pop`)
covid_census_tx_ca_ny_expanded <- rename(covid_census_tx_ca_ny_expanded, `Not Hispanic` = `not_hispanic_pop`)
covid_census_tx_ca_ny_expanded <- rename(covid_census_tx_ca_ny_expanded, `American Indian` = `amerindian_pop`)
covid_census_tx_ca_ny_expanded <- rename(covid_census_tx_ca_ny_expanded, Other = `other_race_pop`)
covid_census_tx_ca_ny_expanded <- rename(covid_census_tx_ca_ny_expanded, `Two or More Races` = `two_or_more_races_pop`)
covid_census_tx_ca_ny_expanded <- rename(covid_census_tx_ca_ny_expanded, `Commuters by Public Transit` = `commuters_by_public_transportation`)
covid_census_tx_ca_ny_expanded <- rename(covid_census_tx_ca_ny_expanded, `Housing Units` = `housing_units`)
covid_census_tx_ca_ny_expanded <- rename(covid_census_tx_ca_ny_expanded, `Vacant Housing Units` = `vacant_housing_units`)
covid_census_tx_ca_ny_expanded <- rename(covid_census_tx_ca_ny_expanded, `Vacant Housing Units For Rent` = `vacant_housing_units_for_rent`)
covid_census_tx_ca_ny_expanded <- rename(covid_census_tx_ca_ny_expanded, `Vacant Housing Units For Sale` = `vacant_housing_units_for_sale`)
covid_census_tx_ca_ny_expanded <- rename(covid_census_tx_ca_ny_expanded, `Median Rent` = `median_rent`)
covid_census_tx_ca_ny_expanded <- rename(covid_census_tx_ca_ny_expanded, `% Income Spent on Rent` = `percent_income_spent_on_rent`)
covid_census_tx_ca_ny_expanded <- rename(covid_census_tx_ca_ny_expanded, `Owner Occupied Housing Units` = `owner_occupied_housing_units`)
covid_census_tx_ca_ny_expanded <- rename(covid_census_tx_ca_ny_expanded, `Million Dollar Houses` = `million_dollar_housing_units`)
covid_census_tx_ca_ny_expanded <- rename(covid_census_tx_ca_ny_expanded, `Commuters 16+` = `commuters_16_over`)
covid_census_tx_ca_ny_expanded <- rename(covid_census_tx_ca_ny_expanded, `Work From Home` = `worked_at_home`)
covid_census_tx_ca_ny_expanded <- rename(covid_census_tx_ca_ny_expanded, `Poverty` = `poverty`)
covid_census_tx_ca_ny_expanded <- rename(covid_census_tx_ca_ny_expanded, `Population Determined Poverty Status` = `pop_determined_poverty_status`)
covid_census_tx_ca_ny_expanded <- rename(covid_census_tx_ca_ny_expanded, `Has Car` = `one_car`)
covid_census_tx_ca_ny_expanded <- rename(covid_census_tx_ca_ny_expanded, `Has Two Cars` = `two_cars`)
covid_census_tx_ca_ny_expanded <- rename(covid_census_tx_ca_ny_expanded, `Has Three Cars` = `three_cars`)
covid_census_tx_ca_ny_expanded <- rename(covid_census_tx_ca_ny_expanded, `Has Four+ Cars` = `four_more_cars`)
covid_census_tx_ca_ny_expanded <- rename(covid_census_tx_ca_ny_expanded, `Has No Car(s)` = `no_car`)
covid_census_tx_ca_ny_expanded <- rename(covid_census_tx_ca_ny_expanded, `Non-US Citizen` = `not_us_citizen_pop`)
covid_census_tx_ca_ny_expanded <- rename(covid_census_tx_ca_ny_expanded, `Income Less Than $10,000` = `income_less_10000`)
covid_census_tx_ca_ny_expanded <- rename(covid_census_tx_ca_ny_expanded, `Income Between $10,000 and $14,999` = `income_10000_14999`)
covid_census_tx_ca_ny_expanded <- rename(covid_census_tx_ca_ny_expanded, `Income Between $15,000 and $19,999` = `income_15000_19999`)
covid_census_tx_ca_ny_expanded <- rename(covid_census_tx_ca_ny_expanded, `Income Between $20,000 and $24,999` = `income_20000_24999`)
covid_census_tx_ca_ny_expanded <- rename(covid_census_tx_ca_ny_expanded, `Income Between $25,000 and $29,999` = `income_25000_29999`)
covid_census_tx_ca_ny_expanded <- rename(covid_census_tx_ca_ny_expanded, `Income Between $30,000 and $34,999` = `income_30000_34999`)
covid_census_tx_ca_ny_expanded <- rename(covid_census_tx_ca_ny_expanded, `Income Between $35,000 and $39,999` = `income_35000_39999`)
covid_census_tx_ca_ny_expanded <- rename(covid_census_tx_ca_ny_expanded, `Income Between $40,000 and $44,999` = `income_40000_44999`)
covid_census_tx_ca_ny_expanded <- rename(covid_census_tx_ca_ny_expanded, `Income Between $45,000 and $49,999` = `income_45000_49999`)
covid_census_tx_ca_ny_expanded <- rename(covid_census_tx_ca_ny_expanded, `Income Between $50,000 and $59,999` = `income_50000_59999`)
covid_census_tx_ca_ny_expanded <- rename(covid_census_tx_ca_ny_expanded, `Income Between $60,000 and $74,999` = `income_60000_74999`)
covid_census_tx_ca_ny_expanded <- rename(covid_census_tx_ca_ny_expanded, `Income Between $75,000 and $99,999` = `income_75000_99999`)
covid_census_tx_ca_ny_expanded <- rename(covid_census_tx_ca_ny_expanded, `Income Between $100,000 and $124,999` = `income_100000_124999`)
covid_census_tx_ca_ny_expanded <- rename(covid_census_tx_ca_ny_expanded, `Income Between $125,000 and $149,999` = `income_125000_149999`)
covid_census_tx_ca_ny_expanded <- rename(covid_census_tx_ca_ny_expanded, `Income Between $150,000 and $199,999` = `income_150000_199999`)
covid_census_tx_ca_ny_expanded <- rename(covid_census_tx_ca_ny_expanded, `Income Between $200,000+` = `income_200000_or_more`)
covid_census_tx_ca_ny_expanded <- rename(covid_census_tx_ca_ny_expanded, `Population Not In Labor Force` = `not_in_labor_force`)
covid_census_tx_ca_ny_expanded <- rename(covid_census_tx_ca_ny_expanded, `Population In Labor Force` = `pop_in_labor_force`)
covid_census_tx_ca_ny_expanded <- rename(covid_census_tx_ca_ny_expanded, `GINI Index` = `gini_index`)
covid_census_tx_ca_ny_expanded <- rename(covid_census_tx_ca_ny_expanded, `Households Using Public Assistance or Food Stamps` = `households_public_asst_or_food_stamps`)

###### Removing features ######
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-median_year_structure_built)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-rent_burden_not_computed)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-renter_occupied_housing_units_paying_cash_median_gross_rent)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-rent_under_10_percent)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-rent_10_to_15_percent)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-rent_15_to_20_percent)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-rent_20_to_25_percent)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-rent_25_to_30_percent)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-rent_30_to_35_percent)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-rent_35_to_40_percent)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-rent_40_to_50_percent)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-rent_over_50_percent)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-pop_divorced)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-pop_widowed)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-pop_separated)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-do_date)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-no_cars)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-pop_now_married)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-pop_never_married)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-pop_15_and_over)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-pop_16_over)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-pop_5_years_over)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-pop_25_64)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-pop_25_years_over)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-population_1_year_and_over)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-population_3_years_over)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-speak_only_english_at_home)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-speak_spanish_at_home_low_english)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-speak_spanish_at_home)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-hispanic_any_race)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-workers_16_and_over)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-walked_to_work)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-male_male_households)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-male_45_64_associates_degree)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-male_45_64_bachelors_degree)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-male_45_64_graduate_degree)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-female_female_households)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-male_45_to_64)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-mortgaged_housing_units)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-aggregate_travel_time_to_work)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-families_with_young_children)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-two_parent_families_with_young_children)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-two_parents_in_labor_force_families_with_young_children)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-two_parents_father_in_labor_force_families_with_young_children)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-two_parents_mother_in_labor_force_families_with_young_children)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-two_parents_not_in_labor_force_families_with_young_children)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-one_parent_families_with_young_children)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-father_one_parent_families_with_young_children)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-father_in_labor_force_one_parent_families_with_young_children)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-owner_occupied_housing_units_lower_value_quartile)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-owner_occupied_housing_units_median_value)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-owner_occupied_housing_units_upper_value_quartile)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-married_households)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-occupied_housing_units)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-housing_units_renter_occupied)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-dwellings_1_units_detached)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-dwellings_1_units_attached)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-dwellings_2_units)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-dwellings_3_to_4_units)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-dwellings_5_to_9_units)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-dwellings_10_to_19_units)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-dwellings_20_to_49_units)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-dwellings_50_or_more_units)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-mobile_homes)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-housing_built_2005_or_later)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-housing_built_2000_to_2004)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-housing_built_1939_or_earlier)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-white_including_hispanic)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-black_including_hispanic)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-asian_including_hispanic)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-amerindian_including_hispanic)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-commuters_by_carpool)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-households_retirement_income)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-armed_forces)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-civilian_labor_force)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-asian_male_45_54)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-asian_male_55_64)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-black_male_45_54)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-black_male_55_64)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-hispanic_male_45_54)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-hispanic_male_55_64)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-white_male_45_54)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-white_male_55_64)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-bachelors_degree_or_higher_25_64)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-children_in_single_female_hh)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-children)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-different_house_year_ago_different_city)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-different_house_year_ago_same_city)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-male_45_64_less_than_9_grade)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-male_45_64_grade_9_12)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-male_45_64_high_school)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-male_45_64_some_college)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-in_grades_1_to_4)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-in_grades_5_to_8)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-in_grades_9_to_12)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-in_school)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-group_quarters)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-management_business_sci_arts_employed)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-bachelors_degree_2)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-occupation_management_arts)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-occupation_natural_resources_construction_maintenance)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-occupation_production_transportation_material)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-occupation_sales_office)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-occupation_services)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-sales_office_employed)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-one_year_more_college)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-in_undergrad_college)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-less_one_year_college)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-some_college_and_associates_degree)














































###### Grouping Education Level ######
## Higher Education(associates, bachelors, masters)(includes partial)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% 
  mutate(`Has Graduated Higher Education` = (`associates_degree` + 
                                             `bachelors_degree` + 
                                             `masters_degree` + 
                                             `graduate_professional_degree`))
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% 
  select(-c(`associates_degree`, `bachelors_degree`, `masters_degree`,
            `graduate_professional_degree`))
## High School Education
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% 
  mutate(`Has Graduated High School` = (`high_school_including_ged` + 
                                            `high_school_diploma`))
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% 
  select(-c(high_school_including_ged, high_school_diploma))
## No High School Education 
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% 
  mutate(`Has Not Graduated High School` = (`less_than_high_school_graduate`))
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% 
  select(-c(less_than_high_school_graduate))
###### Grouping Commute Types ######
## Commute by Public Transit Types(bus, train)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% 
  mutate(`Commute Less than 10 minutes` = (`commuters_by_bus` + `commuters_by_subway_or_elevated`))
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% 
  select(-c(`commuters_by_bus`, `commuters_by_subway_or_elevated`))
## Commute by Personal Vehicle(car, truck, van)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% 
  mutate(`Commute Less than 10 minutes` = (`commuters_drove_alone` + `commuters_by_car_truck_van`))
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% 
  select(-c(`commuters_drove_alone`, `commuters_by_car_truck_van`))
###### Grouping Employment Types ######
## "White Collar" Jobs
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% 
  mutate(`White Collar Workers` = (`employed_education_health_social` + 
                                    `employed_finance_insurance_real_estate` +
                                    `employed_information` +
                                    `employed_other_services_not_public_admin` + 
                                    `employed_public_administration` + 
                                    `employed_science_management_admin_waste`))
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% 
  select(-c(employed_education_health_social, 
            employed_finance_insurance_real_estate,
            employed_information,
            employed_other_services_not_public_admin,
            employed_public_administration,
            employed_science_management_admin_waste))
## "Blue Collar" Jobs
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% 
  mutate(`Blue Collar Workers` = (`employed_agriculture_forestry_fishing_hunting_mining` + 
                                             `employed_arts_entertainment_recreation_accommodation_food` +
                                             `employed_construction` +
                                             `employed_manufacturing` + 
                                             `employed_transportation_warehousing_utilities` + 
                                             `employed_retail_trade` +
                                             `employed_wholesale_trade`))
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% 
  select(-c(employed_agriculture_forestry_fishing_hunting_mining, 
            employed_arts_entertainment_recreation_accommodation_food,
            employed_construction,
            employed_manufacturing,
            employed_transportation_warehousing_utilities,
            employed_retail_trade,
            employed_wholesale_trade))
###### Grouping Commute Times ######
## Commute Less than 10 minutes
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% 
  mutate(`Commute Less than 10 minutes` = (`commute_5_9_mins` + `commute_less_10_mins`))
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% 
  select(-c(`commute_5_9_mins`, `commute_less_10_mins`))
## Commute Less than 60 mins
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% 
  mutate(`Commute Less than 60 minutes` = (`commute_10_14_mins` + 
                                             `commute_15_19_mins` + 
                                             `commute_20_24_mins` + 
                                             `commute_25_29_mins` + 
                                             `commute_30_34_mins` + 
                                             `commute_35_44_mins` + 
                                             `commute_40_44_mins` + 
                                             `commute_45_59_mins`))
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% 
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
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% 
  mutate(`Commute More than 60 minutes` = (`commute_60_89_mins` + 
                                             `commute_90_more_mins`))
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% 
  select(-c(`commute_60_more_mins`, `commute_60_89_mins`,
            `commute_90_more_mins`))
###### Grouping Ages Together ######
## Male
### 0-17
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% 
  mutate(`Male Ages 0-17` = (`male_under_5` + `male_5_to_9` + `male_10_to_14` + `male_15_to_17`))
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% 
  select(-c(`male_under_5`, `male_5_to_9`, `male_10_to_14`, `male_15_to_17`))
### 18-64
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% 
  mutate(`Male Ages 18-64` = (`male_18_to_19` + `male_20` + `male_21` + 
                                `male_22_to_24` + `male_25_to_29` +
                                `male_30_to_34` + `male_35_to_39` +
                                `male_40_to_44` + `male_45_to_49` +
                                `male_50_to_54` + `male_55_to_59` +
                                `male_60_61` + `male_62_64`))
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% 
  select(-c(`male_18_to_19`, `male_20`, `male_21`, 
            `male_22_to_24`, `male_25_to_29`,
            `male_30_to_34`, `male_35_to_39`,
            `male_40_to_44`, `male_45_to_49`,
            `male_50_to_54`, `male_55_to_59`,
            `male_60_61`, `male_62_64`))
### 65-85+
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% 
  mutate(`Male Ages 65-85+` = (`male_65_to_66` + `male_67_to_69` + 
                                 `male_70_to_74` + `male_70_to_74` +
                                 `male_75_to_79` + `male_80_to_84` + 
                                 `male_85_and_over`))
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% 
  select(-c(`male_65_to_66`, `male_67_to_69`, `male_70_to_74`, `male_70_to_74`, 
            `male_75_to_79`, `male_80_to_84`, `male_85_and_over`))
## Female
### 0-17
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% 
  mutate(`Female Ages 0-17` = (`female_under_5` + `female_5_to_9` + 
                                 `female_10_to_14` + `female_15_to_17`))
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% 
  select(-c(`female_under_5`, `female_5_to_9`, `female_10_to_14`, `female_15_to_17`))
### 18-64
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% 
  mutate(`Female Ages 18-64` = (`female_18_to_19` + `female_20` + `female_21` + 
                                  `female_22_to_24` + `female_25_to_29` +
                                  `female_30_to_34` + `female_35_to_39` +
                                  `female_40_to_44` + `female_45_to_49` +
                                  `female_50_to_54` + `female_55_to_59` +
                                  `female_60_to_61` + `female_62_to_64`))
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% 
  select(-c(`female_18_to_19`, `female_20`, `female_21`, 
            `female_22_to_24`, `female_25_to_29`,
            `female_30_to_34`, `female_35_to_39`,
            `female_40_to_44`, `female_45_to_49`,
            `female_50_to_54`, `female_55_to_59`,
            `female_60_to_61`, `female_62_to_64`))
### 65-85+
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% 
  mutate(`Female Ages 65-85+` = (`female_65_to_66` + `female_67_to_69` + 
                                   `female_70_to_74` + `female_70_to_74` +
                                   `female_75_to_79` + `female_80_to_84` + 
                                   `female_85_and_over`))
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% 
  select(-c(`female_65_to_66`, `female_67_to_69`, `female_70_to_74`, `female_70_to_74`, 
            `female_75_to_79`, `female_80_to_84`, `female_85_and_over`))


###### Scaling Function ######
scale_numeric <- function(x) x %>% 
  mutate_if(is.numeric, function(y) as.vector(scale(y)))
#covid_census_tx_ca_ny_expanded_scaled <- covid_census_tx_ca_ny_expanded %>% scale_numeric()
###### Part 2 Clusters ######
# Sex -> Age Groups -> Socioeconomic standing

###### TX ######
covid_census_tx_ <- covid_census_tx_ca_ny_expanded %>% 
  filter(State == "TX")
###### Our Data Table: TX ######
# We consider confirmed cases over 100 to be significant
covid_census_tx_ <- covid_census_tx_ %>% 
  filter(Confirmed > 100) %>% 
  arrange(desc(Confirmed))
#calculate per capita numbers
covid_census_tx_ <- covid_census_tx_ %>% mutate(
  `Cases Per 1000` = Confirmed / Population*1000, 
  `Deaths Per 1000` = Deaths / Population*1000, 
  `Death Per Case` = Deaths / Confirmed,
  `Male Population per 1000` = `Male Population` / Population*1000,
  `Female Population per 1000` = `Female Population` / Population*1000,
  `Median Age per 1000` = `Median Age` / Population*1000,
  `Median Income per 1000` = `Median Income` / Population*1000,
  `Poverty per 1000` = `Poverty` / Population*1000,
  `Population Determined Poverty Status per 1000` = `Population Determined Poverty Status` / Population*1000,
  `Commuters by Public Transit per 1000` = `Commuters by Public Transit` / Population*1000,
  `White per 1000` = White / Population*1000,
  `Black per 1000` = Black / Population*1000,
  `Asian per 1000` = Asian / Population*1000,
  `Hispanic per 1000`= Hispanic /Population*1000,
  `American Indian per 1000` = `American Indian` / Population*1000,
  `Other per 1000` = Other / Population*1000,
  `Two or More Races per 1000` = `Two or More Races` / Population*1000,
  `Male Ages 0-17 per 1000` = `Male Ages 0-17` / Population*1000,
  `Male Ages 18-64 per 1000` = `Male Ages 18-64` / Population*1000,
  `Male Ages 65-85+ per 1000` = `Male Ages 65-85+` / Population*1000,
  `Female Ages 0-17 per 1000` = `Female Ages 0-17` / Population*1000,
  `Female Ages 18-64 per 1000` = `Female Ages 18-64` / Population*1000,
  `Female Ages 65-85+ per 1000` = `Female Ages 65-85+` / Population*1000)

#get county data for latest date
covid_census_tx_latest <- covid_census_tx_ %>% 
  filter(Date == "2022-02-27")

covid_census_tx_per_capita <- covid_census_tx_latest %>% 
  select(`Cases Per 1000`,`Deaths Per 1000`,`Death Per Case`,`Male Population per 1000`, `Female Population per 1000`, `Median Age per 1000`, `Median Income per 1000`,
         `Poverty per 1000`, `Population Determined Poverty Status per 1000`, 
         `Commuters by Public Transit per 1000`, `White per 1000`, `Black per 1000`, `Asian per 1000`,
         `American Indian per 1000`, `Other per 1000`, `Two or More Races per 1000`, `Male Ages 0-17 per 1000`,
         `Male Ages 18-64 per 1000`, `Male Ages 18-64 per 1000`, `Male Ages 65-85+ per 1000`,
         `Female Ages 0-17 per 1000`, `Female Ages 18-64 per 1000`, `Female Ages 65-85+ per 1000`)
#broken down by race
covid_census_tx_per_capita_race <- covid_census_tx_latest %>% 
  select(`White per 1000`, `Black per 1000`, `Asian per 1000`, `Hispanic per 1000`,
         `American Indian per 1000`, `Other per 1000`, `Two or More Races per 1000`)

#broken down by age and sex
covid_census_tx_per_capita_age_sex <- covid_census_tx_latest %>% 
  select(`Male Ages 0-17 per 1000`, `Male Ages 18-64 per 1000`, `Male Ages 18-64 per 1000`, `Male Ages 65-85+ per 1000`,
         `Female Ages 0-17 per 1000`, `Female Ages 18-64 per 1000`, `Female Ages 65-85+ per 1000`)

#broken down by socioeconomic factors
covid_census_tx_per_capita_socioeconomic <- covid_census_tx_latest %>% 
  select(`Median Age per 1000`, `Median Income per 1000`,
         `Poverty per 1000`, `Population Determined Poverty Status per 1000`, 
         `Commuters by Public Transit per 1000`)

####### Removing outliers from the 3 data frames to be used for clustering ######
## Race
covid_census_tx_per_capita_race_scaled <- covid_census_tx_per_capita_race %>% scale_numeric
lof_km_tx <- lof(covid_census_tx_per_capita_race_scaled, minPts = 15)
lof_km_tx
# Plot a graph to see the outliers - we choose two features as a means to start
ggplot(covid_census_tx_per_capita_race_scaled %>% 
         add_column(lof_km_tx = lof_km_tx), 
       aes(`White per 1000`, `Black per 1000`, color = lof_km_tx)) +
  geom_point() + scale_color_gradient(low = "gray", high = "red")
# Try to find the elbow - we'll choose 2
ggplot(tibble(index = seq_len(length(lof_km_tx)), lof_km_tx = sort(lof_km_tx)), aes(index, lof_km_tx)) +
  geom_line() +
  geom_hline(yintercept = 1, color = "red", linetype = 2)
# Plot out which ones are outliers
ggplot(covid_census_tx_per_capita_race_scaled %>% 
         add_column(outlier = lof_km_tx >= 2), aes(`White per 1000`, `Black per 1000`, color = outlier)) +
  geom_point()
# We'll filter those outliers out
covid_census_tx_per_capita_race_scaled_cleaned <- covid_census_tx_per_capita_race_scaled  %>% filter(lof_km_tx < 2)

## Age & Sex
covid_census_tx_per_capita_age_sex_scaled <- covid_census_tx_per_capita_age_sex %>% scale_numeric
lof_km_tx <- lof(covid_census_tx_per_capita_age_sex_scaled, minPts = 15)
lof_km_tx
# Plot a graph to see the outliers - we choose two features as a means to start
ggplot(covid_census_tx_per_capita_age_sex_scaled %>% 
         add_column(lof_km_tx = lof_km_tx), 
       aes(`Male Ages 0-17 per 1000`, `Female Ages 0-17 per 1000`, color = lof_km_tx)) +
  geom_point() + scale_color_gradient(low = "gray", high = "red")
# Try to find the elbow - we'll choose 2
ggplot(tibble(index = seq_len(length(lof_km_tx)), lof_km_tx = sort(lof_km_tx)), aes(index, lof_km_tx)) +
  geom_line() +
  geom_hline(yintercept = 1, color = "red", linetype = 2)
# Plot out which ones are outliers
ggplot(covid_census_tx_per_capita_age_sex_scaled %>% 
         add_column(outlier = lof_km_tx >= 1.5), aes(`Male Ages 0-17 per 1000`, `Female Ages 0-17 per 1000`, color = outlier)) +
  geom_point()
# We'll filter those outliers out
covid_census_tx_per_capita_age_sex_scaled_cleaned <- 
  covid_census_tx_per_capita_age_sex_scaled  %>% filter(lof_km_tx < 1.5)

## Socioeconomic
covid_census_tx_per_capita_socioeconomic_scaled <- covid_census_tx_per_capita_socioeconomic %>% scale_numeric
lof_km_tx <- lof(covid_census_tx_per_capita_socioeconomic_scaled, minPts = 15)
lof_km_tx
# Plot a graph to see the outliers - we choose two features as a means to start
ggplot(covid_census_tx_per_capita_socioeconomic_scaled %>% 
         add_column(lof_km_tx = lof_km_tx), 
       aes(`Poverty per 1000`, `Commuters by Public Transit per 1000`, color = lof_km_tx)) +
  geom_point() + scale_color_gradient(low = "gray", high = "red")
# Try to find the elbow - we'll choose 2
ggplot(tibble(index = seq_len(length(lof_km_tx)), lof_km_tx = sort(lof_km_tx)), aes(index, lof_km_tx)) +
  geom_line() +
  geom_hline(yintercept = 1, color = "red", linetype = 2)
# Plot out which ones are outliers
ggplot(covid_census_tx_per_capita_socioeconomic_scaled %>% 
         add_column(outlier = lof_km_tx >= 2.1), 
       aes(`Poverty per 1000`, `Commuters by Public Transit per 1000`, color = outlier)) +
  geom_point()
# We'll filter those outliers out
covid_census_tx_per_capita_socioeconomic_scaled_cleaned <- 
  covid_census_tx_per_capita_socioeconomic_scaled  %>% filter(lof_km_tx < 2.1)

###### K-Means: TX ######
####### K-Means: TX on race ######
# To see clustering for other races, simply change the x and y for the aes()
# We'll add centroids and begin visualizing our K-Means
km_tx_race <- kmeans(covid_census_tx_per_capita_race_scaled_cleaned, centers = 4, nstart = 10)

covid_census_tx_scaled_continuous_race_km <- covid_census_tx_per_capita_race_scaled_cleaned%>%
  add_column(cluster = factor(km_tx_race$cluster))
centroids_race <- as_tibble(km_tx_race$centers, rownames = "cluster")

ggplot(covid_census_tx_scaled_continuous_race_km, 
       aes(x = `White per 1000`, y = `Hispanic per 1000`, color = cluster)) + geom_point() +
  geom_point(data = centroids_race, aes(x = `White per 1000`, y = `Hispanic per 1000`, color = cluster), 
             shape = 3, size = 10)

# Using outlines to better see the clusters
fviz_cluster(km_tx_race, data = covid_census_tx_per_capita_race_scaled_cleaned, 
             centroids_race = TRUE, repel = TRUE, ellipse.type = "norm")

#### Finding optimal cluster: K-Means: TX on race
# We determine the optimal cluster using the Elbow Method
# We set the projected optimal cluster range 
ks <- 2:15
WCSS <- sapply(ks, FUN = function(k) {
  kmeans(covid_census_tx_scaled_continuous_race_km, centers = k, nstart = 10)$tot.withinss
})

# Here we assume that the optimal number of clusters is 4
ggplot(as_tibble(ks, WCSS), aes(ks, WCSS)) + geom_line() +
  geom_vline(xintercept = 4, color = "red", linetype = 2)

####### K-Means: TX on age & sex ######
# We'll add centroids and begin visualizing our K-Means
km_tx_age_sex <- kmeans(covid_census_tx_per_capita_age_sex_scaled_cleaned, centers = 4, nstart = 10)

covid_census_tx_per_capita_age_sex_scaled_cleaned_km <- covid_census_tx_per_capita_age_sex_scaled_cleaned %>%
  add_column(cluster = factor(km_tx_age_sex$cluster))
centroids_age_sex <- as_tibble(km_tx_age_sex$centers, rownames = "cluster")

ggplot(covid_census_tx_per_capita_age_sex_scaled_cleaned_km, 
       aes(x = `Male Ages 65-85+ per 1000`, y = `Female Ages 65-85+ per 1000`, color = cluster)) + geom_point() +
  geom_point(data = centroids_age_sex, aes(x = `Male Ages 65-85+ per 1000`, y = `Female Ages 65-85+ per 1000`, color = cluster), 
             shape = 3, size = 10)

# Using outlines to better see the clusters
fviz_cluster(km_tx_age_sex, data = covid_census_tx_per_capita_age_sex_scaled_cleaned, 
             centroids_age_sex = TRUE, repel = TRUE, ellipse.type = "norm")

#### Finding optimal cluster: K-Means: TX on age & sex
# We determine the optimal cluster using the Elbow Method
# We set the projected optimal cluster range 
ks <- 2:15
WCSS <- sapply(ks, FUN = function(k) {
  kmeans(covid_census_tx_per_capita_age_sex_scaled_cleaned_km, centers = k, nstart = 10)$tot.withinss
})

####### K-Means: TX on socioeconomic ######
# We'll add centroids and begin visualizing our K-Means
km_tx_socioecon <- kmeans(covid_census_tx_per_capita_socioeconomic_scaled_cleaned, centers = 4, nstart = 10)
summary(covid_census_tx_)
covid_census_tx_per_capita_socioeconomic_scaled_cleaned_km <- 
  covid_census_tx_per_capita_socioeconomic_scaled_cleaned %>%
  add_column(cluster = factor(km_tx_socioecon$cluster))
centroids_socioecon <- as_tibble(km_tx_socioecon$centers, rownames = "cluster")

ggplot(covid_census_tx_per_capita_socioeconomic_scaled_cleaned_km, 
       aes(y = `Median Income per 1000`, x = `Population Determined Poverty Status per 1000`, color = cluster)) + geom_point() +
  geom_point(data = centroids_socioecon, aes(y = `Median Income per 1000`, x = `Population Determined Poverty Status per 1000`, color = cluster), 
             shape = 3, size = 10)

# Using outlines to better see the clusters
fviz_cluster(km_tx_socioecon, data = covid_census_tx_per_capita_socioeconomic_scaled_cleaned, 
             centroids_socioecon = TRUE, repel = TRUE, ellipse.type = "norm")

#### Finding optimal cluster: K-Means: TX on socioeconomic
# We determine the optimal cluster using the Elbow Method
# We set the projected optimal cluster range 
ks <- 2:15
WCSS <- sapply(ks, FUN = function(k) {
  kmeans(covid_census_tx_per_capita_socioeconomic_scaled_cleaned_km, centers = k, nstart = 10)$tot.withinss
})

# Here we assume that the optimal number of clusters is 4
ggplot(as_tibble(ks, WCSS), aes(ks, WCSS)) + geom_line() +
  geom_vline(xintercept = 4, color = "red", linetype = 2)

####### Visualize: K-Means TX######
# We want to visualize the clusters on a map of TX
counties <- as_tibble(map_data("county"))
counties_TX <- counties %>% dplyr::filter(region == "texas") %>% 
  rename(c(County = subregion))
# Race
coivd_census_tx_km <- covid_census_tx_latest %>% select(-c(County))
# Age & Sex
# Socioeconomic


covid_cases_TX <- covid_census_tx_latest %>% mutate(County = County %>% 
                                  str_to_lower() %>% str_replace('\\s+county\\s*$', ''))

counties_TX_clust <- counties_TX %>% inner_join(covid_cases_TX %>% 
                                                 add_column(cluster = factor(km_tx_race$cluster)))

ggplot(counties_TX_clust, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = cluster)) +
  coord_quickmap() + 
  scale_fill_viridis_d() + 
  labs(title = "Clusters", subtitle = "Only counties reporting 100+ cases")
###### Hierarchical Complete: TX ######
####### Hierarchical Complete: TX on race ######
dist_mat_tx_race <- dist(covid_census_tx_per_capita_race_scaled_cleaned)
hc_race <- hclust(dist_mat_tx_race, method = "complete")
plot(hc_race)

# We're aiming to better understand how the clusters are divided
fviz_dend(hc_race, k = 4)

# We use also to verify our assertion in K-Means
clusters_race <- cutree(hc_race, k = 4)
cluster_complete_race <- covid_census_tx_per_capita_race_scaled_cleaned %>%
  add_column(cluster = factor(clusters_race))
ggplot(cluster_complete_race, aes(`White per 1000`, `Black per 1000`, color = cluster)) +
  geom_point()

fviz_cluster(list(data = covid_census_tx_per_capita_race_scaled_cleaned, 
                  cluster = cutree(hc_race, k = 4)), geom = "point")

####### Hierarchical Complete: TX on age & sex ######
dist_mat_tx_age_sex <- dist(covid_census_tx_per_capita_age_sex_scaled_cleaned)
hc_age_sex <- hclust(dist_mat_tx_age_sex, method = "complete")
plot(hc_age_sex)

# We're aiming to better understand how the clusters are divided
fviz_dend(hc_age_sex, k = 4)

# We use also to verify our assertion in K-Means
clusters_age_sex <- cutree(hc_age_sex, k = 4)
cluster_complete_age_sex <- covid_census_tx_per_capita_age_sex_scaled_cleaned %>%
  add_column(cluster = factor(clusters_age_sex))
ggplot(cluster_complete_age_sex, aes(`Male Ages 0-17 per 1000`, `Female Ages 0-17 per 1000`, color = cluster)) +
  geom_point()

fviz_cluster(list(data = covid_census_tx_per_capita_age_sex_scaled_cleaned, 
                  cluster = cutree(hc_age_sex, k = 4)), geom = "point")
####### Hierarchical Complete: TX on socioeconomic ######
dist_mat_tx_socioecon <- dist(covid_census_tx_per_capita_socioeconomic_scaled_cleaned)
hc_socioecon <- hclust(dist_mat_tx_socioecon, method = "complete")
plot(hc_socioecon)

# We're aiming to better understand how the clusters are divided
fviz_dend(hc_socioecon, k = 4)

# We use also to verify our assertion in K-Means
clusters_socioecon <- cutree(hc_socioecon, k = 4)
cluster_complete_socio_econ <- covid_census_tx_per_capita_socioeconomic_scaled_cleaned %>%
  add_column(cluster = factor(clusters_socioecon))
ggplot(cluster_complete_socio_econ, aes(`Poverty per 1000`, `Commuters by Public Transit per 1000`, color = cluster)) +
  geom_point()

fviz_cluster(list(data = covid_census_tx_per_capita_socioeconomic_scaled_cleaned, 
                  cluster = cutree(hc_socioecon, k = 4)), geom = "point")


###### Hierarchical Single Link: TX ######
####### Hierarchical Single Link: TX on race ######
hc_single_race <- hclust(dist_mat_tx_race, method = "single")
fviz_dend(hc_single_race, k = 4)
fviz_cluster(list(data = covid_census_tx_per_capita_race_scaled_cleaned, 
                  cluster = cutree(hc_single_race, k = 4)), geom = "point")
####### Hierarchical Single Link: TX on age & sex ######
hc_single_age_sex <- hclust(dist_mat_tx_age_sex, method = "single")
fviz_dend(hc_single_age_sex, k = 4)
fviz_cluster(list(data = covid_census_tx_per_capita_age_sex_scaled_cleaned, 
                  cluster = cutree(hc_single_age_sex, k = 4)), geom = "point")
####### Hierarchical Single Link: TX on socioeconomic ######
hc_single_socioecon <- hclust(dist_mat_tx_socioecon, method = "single")
fviz_dend(hc_single_socioecon, k = 6)
fviz_cluster(list(data = covid_census_tx_per_capita_socioeconomic_scaled_cleaned, 
                  cluster = cutree(hc_single_socioecon, k = 6)), geom = "point")

###### DBSCAN: TX ######
## Enobong
# We need to decide on the epsilon 
kNNdistplot(covid_census_tx_per_capita_age_sex_scaled_cleaned, k = 50)
# We eyeball the eps  
abline(h = 1.5, col = "red")
# We run DBSCAN
db_age_sex <- dbscan(covid_census_tx_per_capita_age_sex_scaled_cleaned, eps = 1.5, minPts =50)
db_age_sex
str(db_age_sex)
ggplot(covid_census_tx_per_capita_age_sex_scaled_cleaned %>% add_column(cluster = factor(db_age_sex$cluster)),
       aes(`Male Ages 65-85+ per 1000`, `Female Ages 65-85+ per 1000`, color = cluster)) + geom_point()
fviz_cluster(db_age_sex, covid_census_tx_per_capita_age_sex_scaled_cleaned, geom = "point")





# We need to decide on the epsilon 
kNNdistplot(covid_census_tx_per_capita_socioeconomic_scaled_cleaned, k = 50)
# We eyeball the eps  
abline(h = 1.5, col = "red")
# We run DBSCAN
db_socioecon <- dbscan(covid_census_tx_per_capita_socioeconomic_scaled_cleaned, eps = 1.5, minPts =50)
db_socioecon
str(db_socioecon)
ggplot(covid_census_tx_per_capita_socioeconomic_scaled_cleaned %>% add_column(cluster = factor(db_socioecon$cluster)),
       aes(`Poverty per 1000`, `Commuters by Public Transit per 1000`, color = cluster)) + geom_point()
fviz_cluster(db_socioecon, covid_census_tx_per_capita_socioeconomic_scaled_cleaned, geom = "point")




# We need to decide on the epsilon 
kNNdistplot(covid_census_tx_per_capita_race_scaled_cleaned, k = 50)
# We eyeball the eps  
abline(h = 1.7, col = "red")
# We run DBSCAN
db_race <- dbscan(covid_census_tx_per_capita_race_scaled_cleaned, eps = 1.7, minPts =50)
db_race
str(db_race)
ggplot(covid_census_tx_per_capita_race_scaled_cleaned %>% add_column(cluster = factor(db_race$cluster)),
       aes(`Black per 1000`, `Asian per 1000`, color = cluster)) + geom_point()
fviz_cluster(db_race, covid_census_tx_per_capita_race_scaled_cleaned, geom = "point")

###### PAM: TX ######
## Reuben
## Race
#using the elbow method earlier we determined the optimal k value was 3

#pam clustering for covid_census_tx_per_capita_race
pam.res <- pam(covid_census_tx_per_capita_race_scaled_cleaned, 4)
fviz_cluster(pam.res, ellipse.type = "norm")

# Look at cluster profiles
ggplot(pivot_longer(as_tibble(pam.res$medoids,  rownames = "cluster"), 
                    cols = colnames(pam.res$medoids)), 
       aes(y = name, x = value)) +
  geom_bar(stat = "identity") +
  facet_grid(rows = vars(cluster))

# External validation
ggplot(covid_census_tx_per_capita, 
       aes(x = `Deaths Per 1000`, y = `Black per 1000`)) + geom_point()

ggplot(covid_census_tx_per_capita, 
       aes(x = `Deaths Per 1000`, y = `White per 1000`)) + geom_point()

## Age and Sex
pam.res <- pam(covid_census_tx_per_capita_age_sex_scaled, 4)
fviz_cluster(pam.res, ellipse.type = "norm")

# Look at cluster profiles
ggplot(pivot_longer(as_tibble(pam.res$medoids,  rownames = "cluster"), 
                    cols = colnames(pam.res$medoids)), 
       aes(y = name, x = value)) +
  geom_bar(stat = "identity") +
  facet_grid(rows = vars(cluster))

# External validation
ggplot(covid_census_tx_per_capita, 
       aes(x = `Deaths Per 1000`, y = `Male Ages 0-17 per 1000`)) + geom_point()

ggplot(covid_census_tx_per_capita, 
       aes(x = `Deaths Per 1000`, y = `Male Ages 65-85+ per 1000`)) + geom_point()

ggplot(covid_census_tx_per_capita, 
       aes(x = `Deaths Per 1000`, y = `Female Ages 0-17 per 1000`)) + geom_point()

ggplot(covid_census_tx_per_capita, 
       aes(x = `Deaths Per 1000`, y = `Female Ages 65-85+ per 1000`)) + geom_point()


## Socioeconomic
pam.res <- pam(covid_census_tx_per_capita_socioeconomic_scaled, 4)
fviz_cluster(pam.res, ellipse.type = "norm")

# Look at cluster profiles
ggplot(pivot_longer(as_tibble(pam.res$medoids,  rownames = "cluster"), 
                    cols = colnames(pam.res$medoids)), 
       aes(y = name, x = value)) +
  geom_bar(stat = "identity") +
  facet_grid(rows = vars(cluster))

# External validation
ggplot(covid_census_tx_per_capita, 
       aes(x = `Deaths Per 1000`, y = `Poverty per 1000`)) + geom_point()
