library(tidyverse)
library(lubridate)
library(ggplot2)
library(ggdendro)
library(seriation)
library(cluster)
library(factoextra)
library(caret)
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
# read in US data w/ census csv
cases_US_census <- read.csv("./Projects/Project\ 3/data/COVID-19_cases_plus_census.csv")
# subset to include chosen data attributes
cases_US_census <- subset(cases_US_census, select = c("county_name",
                                                      "state",
                                                      "total_pop",
                                                      "confirmed_cases",
                                                      "deaths",
                                                      "male_pop",
                                                      "female_pop",
                                                      "white_pop",
                                                      "black_pop",
                                                      "asian_pop",
                                                      "hispanic_pop",
                                                      "amerindian_pop",
                                                      "other_race_pop",
                                                      "commuters_by_public_transportation",
                                                      "worked_at_home",
                                                      "poverty",
                                                      "associates_degree",
                                                      "bachelors_degree",
                                                      "high_school_diploma",
                                                      "high_school_including_ged",
                                                      "gini_index",
                                                      "median_age",
                                                      "median_income",
                                                      "income_per_capita"))

head(cases_US_census)
# check types
lapply(cases_US_census, class)
# check NA
lapply(cases_US_census, anyNA)
# AT THIS POINT NO NA VALUES AND TYPES ARE CORRECT
# -----------------------------------------------------------------------------------

# add per 1000 values
dataset <- cases_US_census %>% mutate(
  confirmed_cases = confirmed_cases/total_pop*1000,
  deaths = deaths/total_pop*1000,
  male_pop = male_pop/total_pop*1000,
  female_pop = female_pop/total_pop*1000,
  white_pop = white_pop/total_pop*1000,
  black_pop = black_pop/total_pop*1000,
  asian_pop = asian_pop/total_pop*1000,
  hispanic_pop = hispanic_pop/total_pop*1000,
  amerindian_pop = amerindian_pop/total_pop*1000,
  other_race_pop = other_race_pop/total_pop*1000,
  commuters_by_public_transportation = commuters_by_public_transportation/total_pop*1000,
  worked_at_home = worked_at_home/total_pop*1000,
  poverty = poverty/total_pop*1000,
  associates_degree = associates_degree/total_pop*1000,
  bachelors_degree = bachelors_degree/total_pop*1000,
  high_school_diploma = high_school_diploma/total_pop*1000,
  high_school_including_ged = high_school_including_ged/total_pop*1000,
  fatality_rate = deaths/confirmed_cases
)

# remove NA values because 0/0
dataset[which(is.na(dataset$fatality_rate)),]$fatality_rate <- 0

colnames(dataset)
head(dataset)
summary(dataset)

# counties <- as_tibble(map_data("county"))
# counties_TX <- counties %>% dplyr::filter(region == "texas") %>% rename(c(county = subregion))
# dataset <- dataset %>% mutate(county = county_name %>% str_to_lower() %>% str_replace('\\s+county\\s*$', ''))

lapply(dataset, FUN = mean)
lapply(dataset, FUN = median)
lapply(dataset, FUN = Mode)
lapply(dataset, FUN = Freq)
lapply(dataset, FUN = sd)
lapply(dataset, FUN = var)
lapply(dataset, FUN = min)
lapply(dataset, FUN = max)
lapply(dataset, FUN = Rnge)

dataset

subset(dataset, select=c(confirmed_cases == 0))


dataset[which(is.na(dataset$fatality_rate)),]


dataset_sel <- dataset %>% mutate(fatal = as.factor(deaths_per_1000 > 1))

head(dataset_sel)

dataset_sel %>% pull(fatal) %>% table()




