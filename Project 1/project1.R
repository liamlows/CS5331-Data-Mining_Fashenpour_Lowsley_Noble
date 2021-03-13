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
# remove instances up to first recorded case in any county
cases_TX <- subset(cases_TX, date >= "2020-03-05")

# AT THIS POINT NO NA VALUES AND TYPES ARE CORRECT

summary(cases_TX)
head(cases_TX, 20)

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
mean(cases_TX$confirmed_cases)
median(cases_TX$confirmed_cases)
Mode(cases_TX$confirmed_cases)
sd(cases_TX$confirmed_cases)
var(cases_TX$confirmed_cases)

mean(cases_TX$deaths)
median(cases_TX$deaths)
Mode(cases_TX$deaths)
sd(cases_TX$deaths)
var(cases_TX$deaths)

Mode(cases_TX$county_fips_code)
table(cases_TX$county_fips_code)
Mode(cases_TX$county_name)
table(cases_TX$county_name)

Mode(cases_TX$state)
table(cases_TX$state)
Mode(cases_TX$state_fips_code)
table(cases_TX$state_fips_code)


table(cases_TX$confirmed_cases)[1]
table(cases_TX$deaths)[1]


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

multi_stats <- function(x){
  c(mean = mean(x), median = median(x), mode = Mode(x), freq = table(x)[x[x==Mode(x)]], sd = sd(x), var = var(x), min = min(x), max = max(x) )
}



test1 <- lapply(cases_TX_census, FUN = multi_stats)

lapply(cases_TX_census, FUN = mean)
lapply(cases_TX_census, FUN = median)
lapply(cases_TX_census, FUN = Mode)
lapply(cases_TX_census, FUN = sd)
lapply(cases_TX_census, FUN = var)



mean(cases_TX$confirmed_cases)
median(cases_TX$confirmed_cases)
Mode(cases_TX$confirmed_cases)
table(cases_TX$county_fips_code)
sd(cases_TX$confirmed_cases)
var(cases_TX$confirmed_cases)


mean(cases_TX$deaths)
median(cases_TX$deaths)
Mode(cases_TX$deaths)
sd(cases_TX$deaths)
var(cases_TX$deaths)

Mode(cases_TX$county_fips_code)
table(cases_TX$county_fips_code)
Mode(cases_TX$county_name)
table(cases_TX$county_name)

Mode(cases_TX$state)
table(cases_TX$state)
Mode(cases_TX$state_fips_code)
table(cases_TX$state_fips_code)


table(cases_TX$confirmed_cases)[1]
table(cases_TX$deaths)[1]

subset(cases_TX_census, black_pop == 0)

subset(cases_TX_census, hispanic_pop == 12)

subset(cases_TX_census, white_pop == 55)


head(cases_US_df)
cases_US_df$date <- ymd(cases_US_df$date)

# get start and end date for data
min(cases_US_df$date)
max(cases_US_df$date)
