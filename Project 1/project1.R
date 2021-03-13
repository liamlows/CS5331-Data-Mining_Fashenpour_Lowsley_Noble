library(tidyverse)
library(lubridate)
library(ggplot2)

# read in texas data csv
cases_TX <- read.csv("./Projects/Project\ 1/data/COVID-19_cases_TX.csv")
colnames(cases_TX)
head(cases_TX, 20)
# remove statewide unallocated values from dataframe
cases_TX <- cases_TX[-with(cases_TX, which(cases_TX$county_name == "Statewide Unallocated", arr.ind = TRUE)),]
row.names(cases_TX) <- NULL
cases_TX
# cast date to date
cases_TX$date <- ymd(cases_TX$date)
# check types
lapply(cases_TX, class)
# check for NA values
lapply(cases_TX, anyNA)

# AT THIS POINT NO NA VALUES AND TYPES ARE CORRECT

summary(cases_TX)

# check for duplicate/missing counties in TX - there are none
length(unique(cases_TX$county_fips_code))
length(unique(cases_TX$county_name))
# check for non texas entries - there are none
length(unique(cases_TX$state))

# texas data is clean, only needed to convert dates to dates and remove statewide unallocated

# -----------------------------------------------------------------------------------

# read in US data w/ census csv
cases_US_census <- read.csv("./Projects/Project\ 1/data/COVID-19_cases_plus_census.csv")
colnames(cases_US_census)
head(cases_US_census, 20)
# subset to include chosen data attributes
cases_US_census <- subset(cases_US_census, select = c("county_fips_code",
                                              "county_name",
                                              "state",
                                              "state_fips_code",
                                              "date",
                                              "confirmed_cases",
                                              "deaths",
                                              "total_pop",
                                              "median_age",
                                              "white_pop",
                                              "black_pop",
                                              "asian_pop",
                                              "hispanic_pop",
                                              "amerindian_pop",
                                              "other_race_pop",
                                              "median_income",
                                              "income_per_capita"))

# subset to only use Texas data
cases_TX_census <- subset(cases_US_census, state == "TX")
head(cases_TX_census)
# cast date to date
cases_TX_census$date <- ymd(cases_TX_census$date)
# check types
lapply(cases_TX_census, class)
# check NA
lapply(cases_TX_census, anyNA)

# AT THIS POINT NO NA VALUES AND TYPES ARE CORRECT

summary(cases_TX_census)

# check for duplicate/missing counties in TX - there are none
length(unique(cases_TX_census$county_fips_code))
length(unique(cases_TX_census$county_name))
# check for non texas entries - there are none
length(unique(cases_TX_census$state))

subset(cases_TX_census, black_pop == 0)




head(cases_US_df)
cases_US_df$date <- ymd(cases_US_df$date)

# get start and end date for data
min(cases_US_df$date)
max(cases_US_df$date)
