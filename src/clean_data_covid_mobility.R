# So it's a bit easier to manipulate
cleaned_data <- as_tibble(read_csv("Global_Mobility_Report.csv"))
cleaned_data %>% glimpse()

# Column cleaning
cleaned_data <- rename(cleaned_data, `Country Region Code` = `country_region_code`)
cleaned_data <- rename(cleaned_data, `Country Region` = `country_region`)
cleaned_data <- rename(cleaned_data, `Country Region Code` = `country_region_code`)
cleaned_data <- rename(cleaned_data, `Sub Region 1` = `sub_region_1`)
cleaned_data <- rename(cleaned_data, `Sub Region 2` = `sub_region_2`)
cleaned_data <- cleaned_data %>% select(-metro_area)
cleaned_data <- cleaned_data %>% select(-iso_3166_2_code)
cleaned_data <- cleaned_data %>% select(-census_fips_code)
cleaned_data <- rename(cleaned_data, `Date` = `date`)
cleaned_data <- rename(cleaned_data, `Retail & Recreation % Delta` = `retail_and_recreation_percent_change_from_baseline`)
cleaned_data <- rename(cleaned_data, `Grocery & Pharmacy % Delta` = `grocery_and_pharmacy_percent_change_from_baseline`)
cleaned_data <- rename(cleaned_data, `Public Transit % Delta` = `transit_stations_percent_change_from_baseline`)
cleaned_data <- rename(cleaned_data, `Parks % Delta` = `parks_percent_change_from_baseline`)
cleaned_data <- rename(cleaned_data, `Workplace % Delta` = `workplaces_percent_change_from_baseline`)
cleaned_data <- rename(cleaned_data, `Residential % Delta` = `residential_percent_change_from_baseline`)

# Removing unrelated
cleaned_data <- cleaned_data %>% drop_na(`Sub Region 1`)
cleaned_data <- cleaned_data %>% drop_na(`Sub Region 2`)

# And now we're finished!
write_csv(cleaned_data, "Global_Mobility_Report_ThangCleaned.csv")