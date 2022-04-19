# So it's a bit easier to manipulate
cleaned_data <- as_tibble(read_csv("COVID-19_cases_TX.csv"))
glimpse(cleaned_data)

# Column Cleaning
cleaned_data <- rename(cleaned_data, State = `state`)
cleaned_data <- rename(cleaned_data, Date = `date`)
cleaned_data <- rename(cleaned_data, Confirmed = `confirmed_cases`)
cleaned_data <- rename(cleaned_data, Deaths = `deaths`)
cleaned_data <- cleaned_data %>% select(-state_fips_code)
cleaned_data <- cleaned_data %>% select(-county_fips_code)

write_csv(cleaned_data, "COVID_19_TX_cases_ThangCleaned.csv")
