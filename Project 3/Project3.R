library(tidyverse)
library(ggplot2)
library(caret)
library(FSelector)
library(DT)
library(seriation)
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
cases_US_census <- read.csv("./Projects/Project\ 3/data/04-24-21_COVID-CENSUS.csv")
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
summary(dataset)
# remove NA values because 0/0
dataset[which(is.na(dataset$fatality_rate)),]
dataset[which(is.na(dataset$fatality_rate)),]$fatality_rate <- 0
# check
summary(dataset)


# counties <- as_tibble(map_data("county"))
# counties_TX <- counties %>% dplyr::filter(region == "texas") %>% rename(c(county = subregion))
# dataset <- dataset %>% mutate(county = county_name %>% str_to_lower() %>% str_replace('\\s+county\\s*$', ''))

# statistic functions
lapply(dataset, FUN = mean)
lapply(dataset, FUN = median)
lapply(dataset, FUN = Mode)
lapply(dataset, FUN = Freq)
lapply(dataset, FUN = sd)
lapply(dataset, FUN = var)
lapply(dataset, FUN = min)
lapply(dataset, FUN = max)
lapply(dataset, FUN = Rnge)

# deaths per 1000 > 1.8
dataset_sel <- dataset %>% mutate(fatal = as.factor(deaths > 1.8))

# county names in Louisiana contain 'Parish' at the end, need to remove them to
# match the map counties
la_county <- dataset_sel$county_name[dataset_sel$state == 'LA']
la_county <- gsub("[.]","",la_county) 
la_county <- gsub("\\s*\\w*$", "", la_county)

dataset_sel$county_name[dataset_sel$state == 'LA'] <- la_county
dataset_sel$county_name[dataset_sel$state == 'LA']

dataset_sel$county_name[dataset_sel$county_name=="DeWitt County" | dataset_sel$county_name=="Dewitt County" ] <- "De witt County"

head(dataset_sel)

# balance is ok
dataset_sel %>% pull(fatal) %>% table()

# states affected worst
dataset_sel %>% group_by(state) %>% 
  summarize(fatal_pct = sum(fatal == TRUE)/n()) %>%
  arrange(desc(fatal_pct))

# train is not TX
cases_train1 <- dataset_sel %>% filter(!(state %in% c("TX")))
cases_train1 %>% pull(fatal) %>% table()

# test is TX
cases_test1 <-  dataset_sel %>% filter((state %in% c("TX")))
cases_test1 %>% pull(fatal) %>% table()

# obtain counties
counties <- as_tibble(map_data("county"))
counties <- counties %>% 
  rename(c(county = subregion, state = region)) %>%
  mutate(state = state.abb[match(state, tolower(state.name))]) %>%
  select(state, county, long, lat, group)
counties



# add county names to map
counties_all <- counties %>% left_join(cases_train1 %>% 
    mutate(county = county_name %>% str_to_lower() %>% 
    str_replace('\\s+county\\s*$', '')))

# plot
ggplot(counties_all, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = fatal), color = "black", size = 0.1) + 
  coord_quickmap() + scale_fill_manual(values = c('TRUE' = 'red', 'FALSE' = 'grey')) +
  labs(title = "U.S. Map of Deaths Per 1000", subtitle = "Red = greater than 1.8 per 1000, Grey = less than 1.8 per 1000")

colnames(cases_train1)

# check variable importance
cases_train1 <- cases_train1 %>% select(-deaths, -confirmed_cases, -fatality_rate)

cases_train1 %>%  chi.squared(fatal ~ ., data = .) %>% arrange(desc(attr_importance)) %>% head(n = 10)

# RPART METHOD
fit1 <- cases_train1 %>%
  train(fatal ~ . - county_name - state,
        data = . ,
        method = "rpart",
        trControl = trainControl(method = "cv", number = 10)
  )
fit1

# RF METHOD
fit2 <- cases_train1 %>%
  train(fatal ~ . - county_name - state,
        data = . ,
        method = "rf",
        trControl = trainControl(method = "cv", number = 10)
  )
fit2

# NB METHOD
fit3 <- cases_train1 %>%
  train(fatal ~ . - county_name - state,
        data = . ,
        method = "nb",
        trControl = trainControl(method = "cv", number = 10)
  )
fit3





