# Libraries
library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggrepel)
library(ggcorrplot)

#Data
US2020 <- read_csv("~/Documents/Development/GradSchool/data-mining-COVID-Project/data/2020_US_Region_Mobility_Report.csv")
US2021 <- read_csv("~/Documents/Development/GradSchool/data-mining-COVID-Project/data/2021_US_Region_Mobility_Report.csv")
US2022 <- read_csv("~/Documents/Development/GradSchool/data-mining-COVID-Project/data/2022_US_Region_Mobility_Report.csv")

#Data Aggregate by Mean
US2020Aggregate <- aggregate( cbind(retail_2020, grocery_2020, parks_2020, transit_2020, workplaces_2020, residential_2020) ~ county, US2020, mean )

US2021Aggregate <- aggregate( cbind(retail_2021, grocery_2021, parks_2021, transit_2021, workplaces_2021, residential_2021) ~ county, US2021, mean )

US2022Aggregate <- aggregate( cbind(retail_2022, grocery_2022, parks_2022, transit_2022, workplaces_2022, residential_2022) ~ county, US2022, mean )

AggregateList <- list(US2020Aggregate, US2021Aggregate, US2022Aggregate)
AggregateDF <- Reduce(function(x, y) merge(x, y, all=TRUE), AggregateList)  


head(AggregateDF)


