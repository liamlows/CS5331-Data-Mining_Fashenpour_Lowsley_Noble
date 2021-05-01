library(tidyverse)
library(lubridate)
library(ggplot2)
library(ggdendro)
library(seriation)
library(cluster)
library(factoextra)
# -----------------------------------------------------------------------------------
# STAT FUNCTIONS
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
Freq <- function (x){
  max(table(x))
}
Rnge <- function(x) {
  if(is.numeric(x)){
    (max(x) - min(x))
  }
}
# -----------------------------------------------------------------------------------
# read in Texas data csv
cases_TX <- read.csv("./Projects/Project\ 3/data/COVID-19_cases_TX.csv")
# remove state and state_fips_code features
cases_TX <- subset(cases_TX, select = c("county_fips_code",
                                        "county_name",
                                        "date",
                                        "confirmed_cases",
                                        "deaths"))

# remove statewide allocated values from dataframe
cases_TX <- cases_TX[-with(cases_TX, which(cases_TX$county_name == "Statewide Unallocated", arr.ind = TRUE)),]
row.names(cases_TX) <- NULL
# cast date to date
cases_TX$date <- ymd(cases_TX$date)
# check types
lapply(cases_TX, class)
# check for NA values
lapply(cases_TX, anyNA)
# remove instances up to first recorded case in any county
cases_TX <- subset(cases_TX, date >= "2020-03-05")
# AT THIS POINT NO NA VALUES AND TYPES ARE CORRECT

# -----------------------------------------------------------------------------------

# read in US data w/ census csv
cases_US_census <- read.csv("./Projects/Project\ 3/data/COVID-19_cases_plus_census.csv")
colnames(cases_US_census)
# subset to include chosen data attributes
cases_US_census <- subset(cases_US_census, select = c("county_fips_code",
                                                      "county_name",
                                                      "state",
                                                      "total_pop",
                                                      "male_pop",
                                                      "female_pop",
                                                      "median_age",
                                                      "white_pop",
                                                      "black_pop",
                                                      "asian_pop",
                                                      "hispanic_pop",
                                                      "amerindian_pop",
                                                      "other_race_pop",
                                                      "median_income",
                                                      "income_per_capita",
                                                      "commuters_by_public_transportation",
                                                      "worked_at_home",
                                                      "poverty",
                                                      "gini_index",
                                                      "associates_degree",
                                                      "bachelors_degree",
                                                      "high_school_diploma",
                                                      "high_school_including_ged"))

# subset to only use Texas data
cases_TX_census <- subset(cases_US_census, state == "TX")
# check types
lapply(cases_TX_census, class)
# check NA
lapply(cases_TX_census, anyNA)
# AT THIS POINT NO NA VALUES AND TYPES ARE CORRECT

# -----------------------------------------------------------------------------------

# cumulative number of cases/deaths at 01/25/21 for each county
dates <- unique(cases_TX$date)
cases_TX_cumulative <- subset(cases_TX, date == dates[length(dates)])
# remove fips code and date
cases_TX_cumulative <- cases_TX_cumulative [-c(1,3)]
# add populations
dataset <- merge(cases_TX_cumulative, cases_TX_census, by="county_name")
# remove state
dataset <- dataset [-c(5)]

# change county name from DeWitt to De Witt to fix map issue
dataset$county_name[dataset$county_name=="DeWitt County" | 
                    dataset$county_name=="Dewitt County" ] <- "De witt County"

# -----------------------------------------------------------------------------------

# add per 1000 values
dataset <- dataset %>% mutate(
  cases_per_1000 = confirmed_cases/total_pop*1000,
  deaths_per_1000 = deaths/total_pop*1000,
  commuters_public_1000 = commuters_by_public_transportation/total_pop*1000,
  work_home_1000 = worked_at_home/total_pop*1000,
  poverty_1000 = poverty/total_pop*1000,
  white_per_1000 = white_pop/total_pop*1000,
  hispanic_per_1000 = hispanic_pop/total_pop*1000,
  black_per_1000 = black_pop/total_pop*1000,
  asian_per_1000 = asian_pop/total_pop*1000,
  amerindian_per_1000 = amerindian_pop/total_pop*1000,
  other_per_1000 = other_race_pop/total_pop*1000,
  male_per_1000 = male_pop/total_pop*1000,
  female_per_1000 = female_pop/total_pop*1000,
  fatality_rate = deaths/confirmed_cases,
  associates_per_1000 = associates_degree/total_pop*1000,
  bachelors_per_1000 = bachelors_degree/total_pop*1000,
  high_school_per_1000 = high_school_diploma/total_pop*1000,
  ged_per_1000 = high_school_including_ged/total_pop*1000
)

colnames(dataset)

counties <- as_tibble(map_data("county"))
counties_TX <- counties %>% dplyr::filter(region == "texas") %>% rename(c(county = subregion))

dataset <- dataset %>% mutate(county = county_name %>% 
           str_to_lower() %>% str_replace('\\s+county\\s*$', ''))

head(dataset)
summary(dataset)

lapply(dataset, FUN = mean)
lapply(dataset, FUN = median)
lapply(dataset, FUN = Mode)
lapply(dataset, FUN = Freq)
lapply(dataset, FUN = sd)
lapply(dataset, FUN = var)
lapply(dataset, FUN = min)
lapply(dataset, FUN = max)
lapply(dataset, FUN = Rnge)

