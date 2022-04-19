# Libraries
library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggrepel)
library(ggcorrplot)

# Data tibbles
covid_census <- as_tibble(read_csv("~/Documents/Development/GradSchool/data-mining-COVID-Project/data/COVID-19_cases_plus_census.csv"))
covid_tx <- as_tibble((read_csv("~/Documents/Development/GradSchool/data-mining-COVID-Project/data/COVID_19_TX_cases_ThangCleaned.csv")))
covid_global_mobility <- read_csv("~/Documents/Development/GradSchool/data-mining-COVID-Project/data/Global_Mobility_Report_ThangCleaned.csv")

###### Mobility #######
covid_global_mobility <- covid_global_mobility %>% mutate_if(is.character, factor)
covid_us_mobility <- covid_global_mobility %>% filter(`Country Region Code` == 'US')

# Light cleaning of characters to factors
covid_census <- covid_census %>% mutate_if(is.character, factor)

###### Base Stats Analysis #######
# AS OF 2021-01-19
## Total Cases in US ##
sum(covid_census$confirmed_cases)
## Total Deaths in US ##
sum(covid_census$deaths)
## Mean of Deaths in US ##
mean(covid_census$deaths)
## Mean of Cases in US ## 
mean(covid_census$confirmed_cases)
## Median of Cases in US ##
median(covid_census$confirmed_cases)
## Median of Deaths in US ##
median(covid_census$deaths)
## How many cases to deaths in US ##
sum(covid_census$confirmed_cases) / sum(covid_census$deaths)
## ##

## Daily COVID19 Cases in TX ##
tx_cases <- covid_tx %>% ggplot( 
                   aes(Date, as.numeric(Confirmed / 100))) +
  geom_col(fill = 'blue', alpha = 1) + 
  theme_minimal(base_size = 14) +
  xlab(NULL) + ylab("Per 1000") + 
  scale_x_date(date_labels = "%Y/%m/%d")
tx_cases + labs(title= "Daily COVID19 Cases in TX")
## Daily COVID19 Deaths in TX ##
tx_deaths <- covid_tx %>% ggplot( 
  aes(Date, as.numeric(Deaths / 10))) +
  geom_col(fill = 'red', alpha = 1) + 
  theme_minimal(base_size = 14) +
  xlab(NULL) + ylab("Per 10") + 
  scale_x_date(date_labels = "%Y/%m/%d")
tx_deaths + labs(title= "Daily COVID19 Deaths in TX")
## Daily COVID19 Cases + Deaths in TX ##
tx_cases_deaths <- tx_cases +
  geom_col(data = covid_tx, aes(Date, as.numeric(Deaths)), 
  fill = 'red', alpha = 0.5) + 
  theme_minimal(base_size = 10) +
  xlab("From First Recorded Case") + ylab("Per 100") + 
  scale_x_date(date_labels = "%Y/%m/%d")
### TODO: Add a legend where Cases: Orange, Deaths: Purple ###
tx_cases_deaths + labs(title = "Superimposed cases and deaths in TX")

###### Analysis of COVID19 with census ######


# Observing NY - Filter for NY
covid_ny <- covid_census %>% filter(state == "NY")
# Mean of cases in NY #
mean(covid_ny$confirmed_cases)
# Mean of deaths in NY #
mean(covid_ny$deaths)
# Sum of cases in NY #
sum(covid_ny$confirmed_cases)
# Sum of deaths in NY #
sum(covid_ny$deaths)
# Total Population in NY #
sum(covid_ny$total_pop)
# Ratio of cases to deaths in NY #
sum(covid_ny$confirmed_cases) / sum(covid_ny$deaths)
# Are there many counties with many cases?
ggplot(covid_ny, mapping = aes(confirmed_cases)) + geom_histogram(bins = 62)
# Relationship between cases and deaths
ggplot(covid_ny, mapping = aes(x = confirmed_cases, y = deaths, size = total_pop)) + geom_point()
ggplot(covid_ny, mapping = aes(x = confirmed_cases, y = deaths, label = county_name)) + 
  geom_smooth(method = lm) +
  geom_point(mapping = aes(size = total_pop), color = "grey") + 
  geom_text_repel(data = subset(covid_ny, deaths >= 1000)) 
# NY's rates
cases_NY_select <- covid_ny %>% filter(confirmed_cases > 100) %>% 
  arrange(desc(confirmed_cases)) %>%    
  select(county_name, confirmed_cases, deaths,total_pop, white_pop, black_pop, asian_pop, 
         hispanic_pop, amerindian_pop, two_or_more_races_pop, not_hispanic_pop, median_income)
cases_NY_select <- cases_NY_select %>% mutate(
  cases_per_1000 = confirmed_cases/total_pop*1000, 
  deaths_per_1000 = deaths/total_pop*1000, 
  death_per_case = deaths/confirmed_cases)
# Rates per 1000 people
ggplot(cases_NY_select, mapping = aes(x = cases_per_1000, y = deaths_per_1000, label = county_name)) + 
  geom_smooth(method = lm) +
  geom_point(mapping = aes(size = total_pop), color = "grey") + 
  geom_text_repel(data = subset(cases_NY_select, deaths_per_1000 > quantile(deaths_per_1000, .95)))

# Does death per case depend on population?
ggplot(cases_NY_select, mapping = aes(x= total_pop, y = deaths_per_1000, label = county_name)) + 
  geom_smooth(method = lm) +
  geom_point(mapping = aes(size = total_pop), color = "grey") + 
  geom_text_repel(data = subset(cases_NY_select, deaths_per_1000 > quantile(deaths_per_1000, .95)))

# What variables are correlated?
cor_NY <- cor(cases_NY_select[,-1])
ggcorrplot(cor_NY, p.mat = cor_pmat(cases_NY_select[,-1]), insig = "blank", hc.order = TRUE)
# Plot as a map
counties <- as_tibble(map_data("county"))
counties_NY <- counties %>% filter(region == "new york") %>% rename(c(county = subregion))

cases_NY <- cases_NY_select %>% mutate(county = county_name %>% str_to_lower() %>% 
                                         str_replace('\\s+county\\s*$', ''))

counties_NY <- counties_NY %>% 
  left_join(cases_NY %>% select(c(county, cases_per_1000, deaths_per_1000, death_per_case)))
## Lite cleaning ##
counties_NY <- counties_NY %>% na.omit()
## Cases Across NY ##
ggplot(counties_NY, aes(long, lat, label = county)) + 
  geom_polygon(aes(group = group, fill = cases_per_1000)) +
  coord_quickmap() + 
  scale_fill_gradient(low="yellow", high="red") +
  labs(title = "COVID-19 Cases per 1000 People", 
       subtitle = "Only counties reporting 100+ cases")
## Cases Across NY above certain cases threshold ##
counties_NY %>% filter(cases_per_1000 > 70) %>%
  ggplot(aes(long, lat, label = county)) + 
  geom_polygon(aes(group = group, fill = cases_per_1000)) +
  coord_quickmap() + 
  scale_fill_gradient(low="yellow", high = "red") +
  labs(title = "COVID-19 Cases per 1000 People")
## Deaths Across NY ##
counties_NY %>% ggplot(aes(long, lat, label = county)) + 
  geom_polygon(aes(group = group, fill = deaths_per_1000))  +
  coord_quickmap() + 
  scale_fill_gradient(low="yellow", high="red") +
  labs(title = "COVID-19 Deaths per 1000 People", 
       subtitle = "Only counties reporting 100+ Deaths")
## Deaths Across NY above certain threshold ##
counties_NY %>% filter(deaths_per_1000 > 2.0) %>%
  ggplot(aes(long, lat, label = county)) + 
  geom_polygon(aes(group = group, fill = deaths_per_1000))  +
  coord_quickmap() + 
  scale_fill_gradient(low="yellow", high="red") +
  labs(title = "COVID-19 Deaths per 1000 People")

# Observing TX - Filter for TX
covid_tx <- covid_census %>% filter(state == "TX")
# Sum of Cases in TX#
sum(covid_tx$confirmed_cases)
# Sum of Deaths in TX#
sum(covid_tx$deaths)
# Total Population in TX#
sum(covid_tx$total_pop)
# Are there many counties with many cases?
ggplot(covid_tx, mapping = aes(confirmed_cases)) + geom_histogram(bins = 100)
# Relationship between cases and deaths
ggplot(covid_tx, 
       mapping = aes(x = confirmed_cases, y = deaths, size = total_pop)) + 
  geom_point()
ggplot(covid_tx, 
       mapping = aes(x = confirmed_cases, y = deaths, label = county_name)) + 
  geom_smooth(method = lm) +
  geom_point(mapping = aes(size = total_pop), color = "grey") + 
  geom_text_repel(data = subset(covid_tx, deaths >= 1000)) 
# TX's rates
cases_TX_select <- covid_tx %>% filter(confirmed_cases > 100) %>% 
  arrange(desc(confirmed_cases)) %>%    
  select(county_name, confirmed_cases, deaths, total_pop, white_pop, black_pop, 
         asian_pop, hispanic_pop, amerindian_pop, two_or_more_races_pop, 
         not_hispanic_pop, median_income)
cases_TX_select <- cases_TX_select %>% mutate(
  cases_per_1000 = confirmed_cases/total_pop*1000, 
  deaths_per_1000 = deaths/total_pop*1000, 
  death_per_case = deaths/confirmed_cases)
# Rates per 1000 people
ggplot(cases_TX_select, 
       mapping = 
         aes(x = cases_per_1000, y = deaths_per_1000, label = county_name)) + 
  geom_smooth(method = lm) +
  geom_point(mapping = aes(size = total_pop), color = "grey") + 
  geom_text_repel(data = subset(cases_TX_select, deaths_per_1000 > quantile(deaths_per_1000, .95)))

# Does death per case depend on population?
ggplot(cases_TX_select, mapping = aes(x= total_pop, y = deaths_per_1000, 
                                      label = county_name)) + 
  geom_smooth(method = lm) +
  geom_point(mapping = aes(size = total_pop), color = "grey") + 
  geom_text_repel(data = subset(cases_TX_select, 
                                deaths_per_1000 > quantile(deaths_per_1000, .95)))

# What variables are correlated?
cor_TX <- cor(cases_TX_select[,-1])
ggcorrplot(cor_TX, p.mat = cor_pmat(cases_TX_select[,-1]), 
           insig = "blank", hc.order = TRUE)

# Plot as a map
counties <- as_tibble(map_data("county"))
counties_TX <- counties %>% 
  filter(region == "texas") %>% 
  rename(c(county = subregion))

cases_TX <- cases_TX_select %>% 
  mutate(county = county_name %>% 
           str_to_lower() %>% str_replace('\\s+county\\s*$', ''))

counties_TX <- counties_TX %>% 
  left_join(cases_TX %>% 
              select(c(county, cases_per_1000, deaths_per_1000, death_per_case)))
## Lite cleaning ##
counties_TX <- counties_TX %>% na.omit()
## Cases Across TX##
ggplot(counties_TX, aes(long, lat, label = county)) + 
  geom_polygon(aes(group = group, fill = cases_per_1000)) +
  coord_quickmap() + 
  scale_fill_gradient(low="yellow", high="red") +
  labs(title = "COVID-19 Cases per 1000 People", 
       subtitle = "Only counties reporting 100+ cases")
## Cases Across TX above certain cases threshold ##
counties_TX %>% filter(cases_per_1000 > 100) %>%
  ggplot(aes(long, lat, label = county)) + 
  geom_polygon(aes(group = group, fill = cases_per_1000)) +
  coord_quickmap() + 
  scale_fill_gradient(low="yellow", high = "red") +
  labs(title = "COVID-19 Cases per 1000 People")
## Deaths Across TX ##
counties_TX %>% ggplot(aes(long, lat, label = county)) + 
  geom_polygon(aes(group = group, fill = deaths_per_1000))  +
  coord_quickmap() + 
  scale_fill_gradient(low="yellow", high="red") +
  labs(title = "COVID-19 Deaths per 1000 People", 
       subtitle = "Only counties reporting 100+ Deaths")
## Deaths Across TX above certain threshold ##
counties_TX %>% filter(deaths_per_1000 > 2.0) %>%
  ggplot(aes(long, lat, label = county)) + 
  geom_polygon(aes(group = group, fill = deaths_per_1000))  +
  coord_quickmap() + 
  scale_fill_gradient(low="yellow", high="red") +
  labs(title = "COVID-19 Deaths per 1000 People")

# Observing CA - Filter for CA
covid_ca <- covid_census %>% filter(state == "CA")
# Sum of Cases in CA #
sum(covid_ca$confirmed_cases)
# Sum of Deaths in CA # 
sum(covid_ca$deaths)
# Sum of Population in CA #
sum(covid_ca$total_pop)
# Are there many counties with many cases?
ggplot(covid_ca, mapping = aes(confirmed_cases)) + geom_histogram(bins = 20)
# Relationship between cases and deaths
ggplot(covid_ca, mapping = aes(x = confirmed_cases, y = deaths, size = total_pop)) + geom_point()
ggplot(covid_ca, mapping = aes(x = confirmed_cases, y = deaths, label = county_name)) + 
  geom_smooth(method = lm) +
  geom_point(mapping = aes(size = total_pop), color = "grey") + 
  geom_text_repel(data = subset(covid_ca, deaths >= 1000)) 
# CA's rates
cases_CA_select <- covid_ca %>% filter(confirmed_cases > 100) %>% 
  arrange(desc(confirmed_cases)) %>%    
  select(county_name, confirmed_cases, deaths, total_pop, white_pop, black_pop, 
         asian_pop, hispanic_pop, amerindian_pop, two_or_more_races_pop, 
         not_hispanic_pop, median_income)
cases_CA_select <- cases_CA_select %>% mutate(
  cases_per_1000 = confirmed_cases/total_pop*1000, 
  deaths_per_1000 = deaths/total_pop*1000, 
  death_per_case = deaths/confirmed_cases)
# Rates per 1000 people
ggplot(cases_CA_select, mapping = aes(x = cases_per_1000, y = deaths_per_1000, label = county_name)) + 
  geom_smooth(method = lm) +
  geom_point(mapping = aes(size = total_pop), color = "grey") + 
  geom_text_repel(data = subset(cases_CA_select, deaths_per_1000 > quantile(deaths_per_1000, .95)))

# Does death per case depend on population?
ggplot(cases_CA_select, mapping = aes(x= total_pop, y = deaths_per_1000, label = county_name)) + 
  geom_smooth(method = lm) +
  geom_point(mapping = aes(size = total_pop), color = "grey") + 
  geom_text_repel(data = subset(cases_CA_select, deaths_per_1000 > quantile(deaths_per_1000, .95)))

# What variables are correlated?
cor_CA <- cor(cases_CA_select[,-1])
ggcorrplot(cor_CA, p.mat = cor_pmat(cases_CA_select[,-1]), insig = "blank", hc.order = TRUE)

# Plot as a map
counties <- as_tibble(map_data("county"))
counties_CA <- counties %>% filter(region == "california") %>% rename(c(county = subregion))

cases_CA <- cases_CA_select %>% 
  mutate(county = county_name %>% str_to_lower() %>% 
                                         str_replace('\\s+county\\s*$', ''))

counties_CA <- counties_CA %>% 
  left_join(cases_CA %>% 
              select(c(county, cases_per_1000, deaths_per_1000, death_per_case)))
## Lite cleaning ##
counties_CA <- counties_CA %>% na.omit()
## Cases Across CA##
ggplot(counties_CA, aes(long, lat, label = county)) + 
  geom_polygon(aes(group = group, fill = cases_per_1000)) +
  coord_quickmap() + 
  scale_fill_gradient(low="yellow", high="red") +
  labs(title = "COVID-19 Cases per 1000 People", 
       subtitle = "Only counties reporting 100+ cases")
## Cases Across CA above certain cases threshold ##
counties_CA %>% filter(cases_per_1000 > 100) %>%
  ggplot(aes(long, lat, label = county)) + 
  geom_polygon(aes(group = group, fill = cases_per_1000)) +
  coord_quickmap() + 
  scale_fill_gradient(low="yellow", high = "red") +
  labs(title = "COVID-19 Cases per 1000 People")
## Deaths Across CA ##
counties_CA %>% ggplot(aes(long, lat, label = county)) + 
  geom_polygon(aes(group = group, fill = deaths_per_1000))  +
  coord_quickmap() + 
  scale_fill_gradient(low="yellow", high="red") +
  labs(title = "COVID-19 Deaths per 1000 People", 
       subtitle = "Only counties reporting 100+ Deaths")
## Deaths Across CA above certain threshold ##
counties_CA %>% filter(deaths_per_1000 > 1.0) %>%
  ggplot(aes(long, lat, label = county)) + 
  geom_polygon(aes(group = group, fill = deaths_per_1000))  +
  coord_quickmap() + 
  scale_fill_gradient(low="yellow", high="red") +
  labs(title = "COVID-19 Deaths per 1000 People")









###### Data Preparation ######
# Libraries
library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggrepel)
library(ggcorrplot)

covid_census_tx_ca_ny_expanded <- as_tibble((read_csv("~/Documents/Development/GradSchool/data-mining-COVID-Project/data/covid_tx_ca_ny_expaned_census_3.csv")))
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% mutate_if(is.character, factor)

# Getting rid of columns #
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

covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-Employed)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-Unemployed)
covid_census_tx_ca_ny_expanded <- covid_census_tx_ca_ny_expanded %>% select(-`Income Per Capita`)
# Grouping Ages Together
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

## Overall US Mobility ##
summary(us_mobility_2020)
ggplot(us_mobility_2020, mapping = aes(x = Date, y = `Retail & Recreation % Delta`)) + geom_line() + geom_smooth()
ggplot(us_mobility_2020, mapping = aes(x = Date, y = `Grocery & Pharmacy % Delta`)) + geom_line() + geom_smooth()
ggplot(us_mobility_2020, mapping = aes(x = Date, y = `Parks % Delta`)) + geom_line() + geom_smooth()
ggplot(us_mobility_2020, mapping = aes(x = Date, y = `Public Transit % Delta`)) + geom_line() + geom_smooth()
ggplot(us_mobility_2020, mapping = aes(x = Date, y = `Workplaces % Delta`)) + geom_line() + geom_smooth()
ggplot(us_mobility_2020, mapping = aes(x = Date, y = `Residential % Delta`)) + geom_line() + geom_smooth()

### Heatmap of US Mobility ###
usa_2020 <- as_tibble(map_data("county"))
usa_2020_cleaned <- usa_2020 %>% rename(c(County = subregion))

## Need to adjust the County values to just have the name ##
us_mobility_2020_cleaned<- us_mobility_2020 %>% 
  mutate(County = County %>% str_to_lower() 
         %>% str_replace('\\s+county\\s*$', '')) 

## Merging our map and mobility on the County ##
usa_2020_cleaned <- usa_2020_cleaned %>% 
  inner_join(us_mobility_2020_cleaned %>% 
              select(c(County, Date,
                       `Retail & Recreation % Delta`,
                       `Grocery & Pharmacy % Delta`,
                       `Parks % Delta`,
                       `Public Transit % Delta`,
                       `Workplaces % Delta`,
                       `Residential % Delta`)))

ggplot(usa_2020_cleaned, aes(long, lat, label = County)) + 
  geom_polygon(aes(group = group, fill = `Retail & Recreation % Delta`)) +
  coord_quickmap() + 
  scale_fill_gradient(low="yellow", high="red") +
  labs(title = "Movement of People in 2020", subtitle = "US Retail & Rec")
ggplot(usa_2020_cleaned, aes(long, lat, label = County)) + 
  geom_polygon(aes(group = group, fill = `Grocery & Pharmacy % Delta`)) +
  coord_quickmap() + 
  scale_fill_gradient(low="yellow", high="red") +
  labs(title = "Movement of People in 2020", subtitle = "US Grocery & Pharmacy")
ggplot(usa_2020_cleaned, aes(long, lat, label = County)) + 
  geom_polygon(aes(group = group, fill = `Parks % Delta`)) +
  coord_quickmap() + 
  scale_fill_gradient(low="yellow", high="red") +
  labs(title = "Movement of People in 2020", subtitle = "US Parks")
ggplot(usa_2020_cleaned, aes(long, lat, label = County)) + 
  geom_polygon(aes(group = group, fill = `Public Transit % Delta`)) +
  coord_quickmap() + 
  scale_fill_gradient(low="yellow", high="red") +
  labs(title = "Movement of People in 2020", subtitle = "US Public Transit")
ggplot(usa_2020_cleaned, aes(long, lat, label = County)) + 
  geom_polygon(aes(group = group, fill = `Workplaces % Delta`)) +
  coord_quickmap() + 
  scale_fill_gradient(low="yellow", high="red") +
  labs(title = "Movement of People in 2020", subtitle = "US Workplaces")
ggplot(usa_2020_cleaned, aes(long, lat, label = County)) + 
  geom_polygon(aes(group = group, fill = `Residential % Delta`)) +
  coord_quickmap() + 
  scale_fill_gradient(low="yellow", high="red") +
  labs(title = "Movement of People in 2020", subtitle = "US Residential Areas")
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

## Overall US Mobility ##
summary(us_mobility_2021)
ggplot(us_mobility_2021, mapping = aes(x = Date, y = `Retail & Recreation % Delta`)) + geom_line() + geom_smooth()
ggplot(us_mobility_2021, mapping = aes(x = Date, y = `Grocery & Pharmacy % Delta`)) + geom_line() + geom_smooth()
ggplot(us_mobility_2021, mapping = aes(x = Date, y = `Parks % Delta`)) + geom_line() + geom_smooth()
ggplot(us_mobility_2021, mapping = aes(x = Date, y = `Public Transit % Delta`)) + geom_line() + geom_smooth()
ggplot(us_mobility_2021, mapping = aes(x = Date, y = `Workplaces % Delta`)) + geom_line() + geom_smooth()
ggplot(us_mobility_2021, mapping = aes(x = Date, y = `Residential % Delta`)) + geom_line() + geom_smooth()

### Heatmap of US Mobility ###
usa_2021 <- as_tibble(map_data("county"))
usa_2021_cleaned <- usa_2021 %>% rename(c(County = subregion))

## Need to adjust the County values to just have the name ##
us_mobility_2021_cleaned<- us_mobility_2021 %>% 
  mutate(County = County %>% str_to_lower() 
         %>% str_replace('\\s+county\\s*$', '')) 

## Merging our map and mobility on the County ##
usa_2021_cleaned <- usa_2021_cleaned %>% 
  inner_join(us_mobility_2021_cleaned %>% 
               select(c(County, Date,
                        `Retail & Recreation % Delta`,
                        `Grocery & Pharmacy % Delta`,
                        `Parks % Delta`,
                        `Public Transit % Delta`,
                        `Workplaces % Delta`,
                        `Residential % Delta`)))

ggplot(usa_2021_cleaned, aes(long, lat, label = County)) + 
  geom_polygon(aes(group = group, fill = `Retail & Recreation % Delta`)) +
  coord_quickmap() + 
  scale_fill_gradient(low="yellow", high="red") +
  labs(title = "Movement of People in 2021", subtitle = "US Retail & Rec")
ggplot(usa_2021_cleaned, aes(long, lat, label = County)) + 
  geom_polygon(aes(group = group, fill = `Grocery & Pharmacy % Delta`)) +
  coord_quickmap() + 
  scale_fill_gradient(low="yellow", high="red") +
  labs(title = "Movement of People in 2021", subtitle = "US Grocery & Pharmacy")
ggplot(usa_2021_cleaned, aes(long, lat, label = County)) + 
  geom_polygon(aes(group = group, fill = `Parks % Delta`)) +
  coord_quickmap() + 
  scale_fill_gradient(low="yellow", high="red") +
  labs(title = "Movement of People in 2021", subtitle = "US Parks")
ggplot(usa_2021_cleaned, aes(long, lat, label = County)) + 
  geom_polygon(aes(group = group, fill = `Public Transit % Delta`)) +
  coord_quickmap() + 
  scale_fill_gradient(low="yellow", high="red") +
  labs(title = "Movement of People in 2021", subtitle = "US Public Transit")
ggplot(usa_2021_cleaned, aes(long, lat, label = County)) + 
  geom_polygon(aes(group = group, fill = `Workplaces % Delta`)) +
  coord_quickmap() + 
  scale_fill_gradient(low="yellow", high="red") +
  labs(title = "Movement of People in 2021", subtitle = "US Workplaces")
ggplot(usa_2021_cleaned, aes(long, lat, label = County)) + 
  geom_polygon(aes(group = group, fill = `Residential % Delta`)) +
  coord_quickmap() + 
  scale_fill_gradient(low="yellow", high="red") +
  labs(title = "Movement of People in 2021", subtitle = "US Residential Areas")
###### 2022 US Region Mobility ######
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

## Overall US Mobility ##
summary(us_mobility_2022)
ggplot(us_mobility_2022, mapping = aes(x = Date, y = `Retail & Recreation % Delta`)) + geom_line() + geom_smooth()
ggplot(us_mobility_2021, mapping = aes(x = Date, y = `Grocery & Pharmacy % Delta`)) + geom_line() + geom_smooth()
ggplot(us_mobility_2021, mapping = aes(x = Date, y = `Parks % Delta`)) + geom_line() + geom_smooth()
ggplot(us_mobility_2021, mapping = aes(x = Date, y = `Public Transit % Delta`)) + geom_line() + geom_smooth()
ggplot(us_mobility_2021, mapping = aes(x = Date, y = `Workplaces % Delta`)) + geom_line() + geom_smooth()
ggplot(us_mobility_2021, mapping = aes(x = Date, y = `Residential % Delta`)) + geom_line() + geom_smooth()

### Heatmap of US Mobility ###
usa_2022 <- as_tibble(map_data("county"))
usa_2022_cleaned <- usa_2022 %>% rename(c(County = subregion))

## Need to adjust the County values to just have the name ##
us_mobility_2022_cleaned<- us_mobility_2022 %>% 
  mutate(County = County %>% str_to_lower() 
         %>% str_replace('\\s+county\\s*$', '')) 

## Merging our map and mobility on the County ##
usa_2022_cleaned <- usa_2022_cleaned %>% 
  inner_join(us_mobility_2022_cleaned %>% 
               select(c(County, Date,
                        `Retail & Recreation % Delta`,
                        `Grocery & Pharmacy % Delta`,
                        `Parks % Delta`,
                        `Public Transit % Delta`,
                        `Workplaces % Delta`,
                        `Residential % Delta`)))

ggplot(usa_2022_cleaned, aes(long, lat, label = County)) + 
  geom_polygon(aes(group = group, fill = `Retail & Recreation % Delta`)) +
  coord_quickmap() + 
  scale_fill_gradient(low="yellow", high="red") +
  labs(title = "Movement of People in 2022", subtitle = "US Retail & Rec")
ggplot(usa_2022_cleaned, aes(long, lat, label = County)) + 
  geom_polygon(aes(group = group, fill = `Grocery & Pharmacy % Delta`)) +
  coord_quickmap() + 
  scale_fill_gradient(low="yellow", high="red") +
  labs(title = "Movement of People in 2022", subtitle = "US Grocery & Pharmacy")
ggplot(usa_2022_cleaned, aes(long, lat, label = County)) + 
  geom_polygon(aes(group = group, fill = `Parks % Delta`)) +
  coord_quickmap() + 
  scale_fill_gradient(low="yellow", high="red") +
  labs(title = "Movement of People in 2022", subtitle = "US Parks")
ggplot(usa_2022_cleaned, aes(long, lat, label = County)) + 
  geom_polygon(aes(group = group, fill = `Public Transit % Delta`)) +
  coord_quickmap() + 
  scale_fill_gradient(low="yellow", high="red") +
  labs(title = "Movement of People in 2022", subtitle = "US Public Transit")
ggplot(usa_2022_cleaned, aes(long, lat, label = County)) + 
  geom_polygon(aes(group = group, fill = `Workplaces % Delta`)) +
  coord_quickmap() + 
  scale_fill_gradient(low="yellow", high="red") +
  labs(title = "Movement of People in 2022", subtitle = "US Workplaces")
ggplot(usa_2022_cleaned, aes(long, lat, label = County)) + 
  geom_polygon(aes(group = group, fill = `Residential % Delta`)) +
  coord_quickmap() + 
  scale_fill_gradient(low="yellow", high="red") +
  labs(title = "Movement of People in 2022", subtitle = "US Residential Areas")
