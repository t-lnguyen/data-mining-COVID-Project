# So it's a bit easier to manipulate
cleaned_data <- as_tibble(
  read_csv("COVID-19_cases_plus_census_column_cleaned.csv"))
glimpse(cleaned_data)

# Column Cleaning
cleaned_data <- cleaned_data %>% select(-FIP)
cleaned_data <- rename(cleaned_data, County = `county_name`)
cleaned_data <- rename(cleaned_data, State = `state`)
cleaned_data <- cleaned_data %>% select(-state_fips_code)
cleaned_data <- rename(cleaned_data, Date = `date`)
cleaned_data <- rename(cleaned_data, Confirmed = `confirmed_cases`)
cleaned_data <- rename(cleaned_data, Deaths = `deaths`)
cleaned_data <- rename(cleaned_data, `Geographic ID` = `geo_id`)
cleaned_data <- rename(cleaned_data, Population = `total_pop`)
cleaned_data <- rename(cleaned_data, `Median Age` = `median_age`)
cleaned_data <- rename(cleaned_data, `Number of Households` = `households`)
cleaned_data <- rename(cleaned_data, `Median Income` = `median_income`)
cleaned_data <- rename(cleaned_data, `Income Per Capita` = `income_per_capita`)
cleaned_data <- rename(cleaned_data, Employed = `employed_pop`)
cleaned_data <- rename(cleaned_data, Unemployed = `unemployed_pop`)

# Grouping Ages Together
## Male
### 0-17
cleaned_data <- cleaned_data %>% 
  mutate(`Male Ages 0-17` = (`male_under_5` + `male_5_to_9` + `male_10_to_14` + `male_15_to_17`))
cleaned_data <- cleaned_data %>% 
  select(-c(`male_under_5`, `male_5_to_9`, `male_10_to_14`, `male_15_to_17`))
### 18-64
cleaned_data <- cleaned_data %>% 
  mutate(`Male Ages 18-64` = (`male_18_to_19` + `male_20` + `male_21` + 
                                `male_22_to_24` + `male_25_to_29` +
                                `male_30_to_34` + `male_35_to_39` +
                                `male_40_to_44` + `male_45_to_49` +
                                `male_50_to_54` + `male_55_to_59` +
                                `male_60_61` + `male_62_64`))
cleaned_data <- cleaned_data %>% 
  select(-c(`male_18_to_19`, `male_20`, `male_21`, 
            `male_22_to_24`, `male_25_to_29`,
            `male_30_to_34`, `male_35_to_39`,
            `male_40_to_44`, `male_45_to_49`,
            `male_50_to_54`, `male_55_to_59`,
            `male_60_61`, `male_62_64`))
### 65-85+
cleaned_data <- cleaned_data %>% 
  mutate(`Male Ages 65-85+` = (`male_65_to_66` + `male_67_to_69` + 
                                 `male_70_to_74` + `male_70_to_74` +
                                 `male_75_to_79` + `male_80_to_84` + 
                                 `male_85_and_over`))
cleaned_data <- cleaned_data %>% 
  select(-c(`male_65_to_66`, `male_67_to_69`, `male_70_to_74`, `male_70_to_74`, 
            `male_75_to_79`, `male_80_to_84`, `male_85_and_over`))
## Female
### 0-17
cleaned_data <- cleaned_data %>% 
  mutate(`Female Ages 0-17` = (`female_under_5` + `female_5_to_9` + 
                                 `female_10_to_14` + `female_15_to_17`))
cleaned_data <- cleaned_data %>% 
  select(-c(`female_under_5`, `female_5_to_9`, `female_10_to_14`, `female_15_to_17`))
### 18-64
cleaned_data <- cleaned_data %>% 
  mutate(`Female Ages 18-64` = (`female_18_to_19` + `female_20` + `female_21` + 
                                `female_22_to_24` + `female_25_to_29` +
                                `female_30_to_34` + `female_35_to_39` +
                                `female_40_to_44` + `female_45_to_49` +
                                `female_50_to_54` + `female_55_to_59` +
                                `female_60_to_61` + `female_62_to_64`))
cleaned_data <- cleaned_data %>% 
  select(-c(`female_18_to_19`, `female_20`, `female_21`, 
            `female_22_to_24`, `female_25_to_29`,
            `female_30_to_34`, `female_35_to_39`,
            `female_40_to_44`, `female_45_to_49`,
            `female_50_to_54`, `female_55_to_59`,
            `female_60_to_61`, `female_62_to_64`))
### 65-85+
cleaned_data <- cleaned_data %>% 
  mutate(`Female Ages 65-85+` = (`female_65_to_66` + `female_67_to_69` + 
                                 `female_70_to_74` + `female_70_to_74` +
                                 `female_75_to_79` + `female_80_to_84` + 
                                 `female_85_and_over`))
cleaned_data <- cleaned_data %>% 
  select(-c(`female_65_to_66`, `female_67_to_69`, `female_70_to_74`, `female_70_to_74`, 
            `female_75_to_79`, `female_80_to_84`, `female_85_and_over`))

# Cleaning data types
cleaned_data %>% mutate(across())

# And now we're finished!
write_csv(cleaned_data, "COVID_19_cases_plus_census_ThangCleaned.csv")
